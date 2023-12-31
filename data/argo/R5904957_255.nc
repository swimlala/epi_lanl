CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:53Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140853  20181024140853  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�H�1   @��eW:۬@5��j~���c���O�;1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2�C4  C5�fC8  C:  C<  C>  C@  CB  CD  CE�fCH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C��3C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
y�D  D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D  Dy�D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8y�D9  D9� D:  D:� D;  D;�fD<  D<y�D=  D=� D>  D>� D?  D?y�D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DRfDR�fDSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\�fD]fD]� D^  D^� D_  D_� D`fD`�fDa  Da� Db  Db� Dc  Dcy�Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dp� Dq  Dq� Dq��Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�{D�-�D�)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@�A�HA"�HAB�HAb�HA�p�A�=qA�=qA�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBXQ�B`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*G�C,G�C..C0.C2G�C4.C6zC8.C:.C<.C>.C@.CB.CD.CFzCH.CJ.CL.CNzCP.CR.CT.CV.CX.CZ.C\.C^G�C`G�Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.CxG�Cz.C|.C~.C�
C�
=C�
C�#�C�#�C�#�C�
C�
C�
C�
C�#�C�#�C�#�C�#�C�
C�
C�
C�#�C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
C�
C�
C�
C�
C�
=C�
C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
=C�
C�
=C�
=C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�#�C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D	�D	��D
�D
�D�D��D�D�DD�DD��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%D%�D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8D8�D9�D9��D:�D:��D;�D;��D<�D<�D=�D=��D>�D>��D?�D?�D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�DdDd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do�DpDp��Dq�Dq��DrDr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy� D�3�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��-A��!A��9A��A��RA�ƨA�ĜA�ĜA�ĜA�ƨA�ĜA���A���A���A���A���A��
A���A���A���A���A�A��!A��A��7A�I�A�  A�t�A�/A�=qA�l�A��FA���A�9XA�p�A��A�"�A���A��#A�7LA���A��A��A�&�A�=qA��A��A�VA���A�+A��uA�x�A�A�l�A�1'A�hsA�5?A��A��DA�5?A��#A�1'A�hsA��A�{A���A��A�bNA��9A�oA��/A�l�A��+A��A�bNA���A���A��
A�VA�Q�A�#A~�`A}��A|�yA|�!A|r�A|1'A{&�Ay�7AxffAw��Aw��AvĜAu�FAsVAo�Ao�hAo�PAo\)Ao\)Ao%Al��Ak"�Ai�Ag�Af�\Ad�+Ab�Aa|�A`ĜA_�-A^ȴA^bNA^  A]VAZ9XAX�yAW�wAV�`AV �AU&�AS�
AQ�AP��AO��AOt�AO+AM��AG��AD-AA�A@ffA?hsA>M�A< �A;�A;hsA;oA9x�A9O�A7��A6�\A6{A5�A3C�A2��A2I�A/�7A-A-"�A,�A*��A)�FA)G�A)33A(�HA'�A&��A&I�A%&�A#XA!VA ^5A�A�A%AE�A��A`BA/A�uAhsAz�Al�A=qA��AhsAXA�wA�A��AE�A�TAK�AbAA/AȴA9XA �AbA��A�A�TA��At�A��A7LA�yA�TA �A Z@�dZ@�n�@�%@��@�r�@���@�V@�-@�X@�C�@�?}@�@�hs@�A�@�1@�w@�@�33@��@�V@��@�9@�dZ@�dZ@�p�@�b@�@���@��@�G�@�V@ԛ�@�A�@��@ӝ�@��y@���@�5?@��@�p�@�?}@��@��@Ь@���@�1@��@�;d@��@�n�@�@���@�G�@���@̴9@�j@�A�@�b@ˍP@�o@�ȴ@ʗ�@��@���@�(�@Ǯ@���@�`B@��
@�l�@�K�@�ȴ@��@�G�@�z�@�b@���@�`B@���@�b@�K�@�n�@��-@�Ĝ@��@��H@���@�ff@�@�&�@��`@�bN@�1@�;d@���@��@�hs@�&�@��/@�z�@���@�|�@�
=@��H@���@��^@�?}@�&�@��`@���@�r�@�A�@�1@��;@��P@�|�@�l�@�dZ@�+@�M�@��-@�p�@��@�Q�@�S�@�33@�;d@�33@��@��@���@��@��j@�C�@�5?@�$�@�5?@�-@���@�G�@� �@�|�@�5?@��@��T@���@�O�@���@��@�j@�A�@��@��@��+@�M�@�=q@�5?@�{@�@�p�@���@�A�@��w@��F@�|�@���@�M�@�-@��-@��@���@���@���@��u@�bN@�I�@�A�@�1'@�(�@���@��F@���@��@�33@���@�v�@�^5@�M�@���@��@��@� �@���@��P@�|�@�dZ@�C�@��y@�V@��T@��^@��7@�O�@�%@���@��D@� �@�t�@�33@���@��+@�ff@�-@��-@���@��7@�p�@�X@�G�@�%@��`@��@��9@�Z@�A�@�(�@�  @��
@��F@�dZ@�+@��@�o@�
=@�
=@��H@���@���@�V@���@�?}@�O�@�z�@���@��@���@�v�@�{@�J@��@��@��@��h@��/@��9@�r�@�1'@�w@
=@~�R@}�T@|z�@{�m@{�@z�!@z^5@z��@y�7@yhs@zq�@n�}@^;�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��-A��!A��9A��A��RA�ƨA�ĜA�ĜA�ĜA�ƨA�ĜA���A���A���A���A���A��
A���A���A���A���A�A��!A��A��7A�I�A�  A�t�A�/A�=qA�l�A��FA���A�9XA�p�A��A�"�A���A��#A�7LA���A��A��A�&�A�=qA��A��A�VA���A�+A��uA�x�A�A�l�A�1'A�hsA�5?A��A��DA�5?A��#A�1'A�hsA��A�{A���A��A�bNA��9A�oA��/A�l�A��+A��A�bNA���A���A��
A�VA�Q�A�#A~�`A}��A|�yA|�!A|r�A|1'A{&�Ay�7AxffAw��Aw��AvĜAu�FAsVAo�Ao�hAo�PAo\)Ao\)Ao%Al��Ak"�Ai�Ag�Af�\Ad�+Ab�Aa|�A`ĜA_�-A^ȴA^bNA^  A]VAZ9XAX�yAW�wAV�`AV �AU&�AS�
AQ�AP��AO��AOt�AO+AM��AG��AD-AA�A@ffA?hsA>M�A< �A;�A;hsA;oA9x�A9O�A7��A6�\A6{A5�A3C�A2��A2I�A/�7A-A-"�A,�A*��A)�FA)G�A)33A(�HA'�A&��A&I�A%&�A#XA!VA ^5A�A�A%AE�A��A`BA/A�uAhsAz�Al�A=qA��AhsAXA�wA�A��AE�A�TAK�AbAA/AȴA9XA �AbA��A�A�TA��At�A��A7LA�yA�TA �A Z@�dZ@�n�@�%@��@�r�@���@�V@�-@�X@�C�@�?}@�@�hs@�A�@�1@�w@�@�33@��@�V@��@�9@�dZ@�dZ@�p�@�b@�@���@��@�G�@�V@ԛ�@�A�@��@ӝ�@��y@���@�5?@��@�p�@�?}@��@��@Ь@���@�1@��@�;d@��@�n�@�@���@�G�@���@̴9@�j@�A�@�b@ˍP@�o@�ȴ@ʗ�@��@���@�(�@Ǯ@���@�`B@��
@�l�@�K�@�ȴ@��@�G�@�z�@�b@���@�`B@���@�b@�K�@�n�@��-@�Ĝ@��@��H@���@�ff@�@�&�@��`@�bN@�1@�;d@���@��@�hs@�&�@��/@�z�@���@�|�@�
=@��H@���@��^@�?}@�&�@��`@���@�r�@�A�@�1@��;@��P@�|�@�l�@�dZ@�+@�M�@��-@�p�@��@�Q�@�S�@�33@�;d@�33@��@��@���@��@��j@�C�@�5?@�$�@�5?@�-@���@�G�@� �@�|�@�5?@��@��T@���@�O�@���@��@�j@�A�@��@��@��+@�M�@�=q@�5?@�{@�@�p�@���@�A�@��w@��F@�|�@���@�M�@�-@��-@��@���@���@���@��u@�bN@�I�@�A�@�1'@�(�@���@��F@���@��@�33@���@�v�@�^5@�M�@���@��@��@� �@���@��P@�|�@�dZ@�C�@��y@�V@��T@��^@��7@�O�@�%@���@��D@� �@�t�@�33@���@��+@�ff@�-@��-@���@��7@�p�@�X@�G�@�%@��`@��@��9@�Z@�A�@�(�@�  @��
@��F@�dZ@�+@��@�o@�
=@�
=@��H@���@���@�V@���@�?}@�O�@�z�@���@��@���@�v�@�{@�J@��@��@��@��h@��/@��9@�r�@�1'@�w@
=@~�R@}�T@|z�@{�m@{�@z�!@z^5@z��@y�7@yhs@zq�@n�}@^;�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBBBBBB  BBBBBBBBBBBBBBBB+BPBPB�B!�B)�B0!B5?B� Bx�Bt�Bq�Bo�Bk�BgmB_;BXBZB]/B]/B[#BS�BM�BI�B?}B33B�B%B��B�mB��B��BƨB�LB�B��B��B��B�{B�bB�+Br�BW
BP�BM�BI�B=qB49B+B�B	7B
��B
�B
�yB
��B
�9B
��B
�hB
�+B
�B
z�B
s�B
m�B
k�B
iyB
ffB
_;B
S�B
K�B
F�B
D�B
=qB
49B
#�B
bB
PB
JB
DB

=B
+B	��B	�B	�;B	�
B	��B	ĜB	�dB	�3B	�B	��B	��B	��B	��B	��B	�1B	�B	y�B	t�B	p�B	jB	bNB	W
B	Q�B	L�B	I�B	F�B	;dB	�B	
=B��B��B�B�B�mB�`B�TB�;B�#B�)B�B��B��BɺB��B�wB�dB�LB�9B�3B�'B�B�B��B��B��B��B��B��B��B�{B�PB�=B�1B�%B�B�B� B~�B|�Bz�Bx�Bu�Bt�Bq�Bo�Bm�BjBhsBffBdZBbNBaHB_;B^5B`BBaHB_;B^5B^5B^5B]/B]/B]/B\)B[#BYBXBW
BW
BW
BW
BXBXBW
BYBYB]/B_;B`BB_;B_;BbNBdZBhsBjBk�Bl�Bm�Bn�Bo�Bo�Bp�Bp�Bn�Bo�Bn�Bo�Bx�B~�B�B�B�B�B�B�B�+B�JB�VB�{B��B��B��B��B��B��B��B��B�B�!B�!B�!B�'B�'B�!B�'B�-B�3B�3B�3B�3B�9B�9B�9B�FB�dB�qB�}B��BŢB��B��B��B��B��B��B�B�
B�/B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B		7B	\B	bB	uB	�B	�B	�B	�B	�B	!�B	'�B	,B	-B	/B	1'B	33B	6FB	;dB	>wB	C�B	E�B	G�B	I�B	M�B	R�B	S�B	S�B	W
B	XB	VB	VB	VB	W
B	XB	YB	ZB	ZB	ZB	]/B	\)B	\)B	]/B	]/B	]/B	]/B	]/B	[#B	ffB	m�B	v�B	w�B	w�B	x�B	{�B	}�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�3B	�?B	�LB	�LB	�LB	�RB	�RB	�dB	�dB	�jB	�qB	�qB	�qB	�}B	��B	B	ŢB	ƨB	ǮB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�5B	�BB	�HB	�HB	�ZB	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
aB
B
'�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBBBBBB  BBBBBBBBBBBBBBBB+BPBPB�B!�B)�B0!B5?B� Bx�Bt�Bq�Bo�Bk�BgmB_;BXBZB]/B]/B[#BS�BM�BI�B?}B33B�B%B��B�mB��B��BƨB�LB�B��B��B��B�{B�bB�+Br�BW
BP�BM�BI�B=qB49B+B�B	7B
��B
�B
�yB
��B
�9B
��B
�hB
�+B
�B
z�B
s�B
m�B
k�B
iyB
ffB
_;B
S�B
K�B
F�B
D�B
=qB
49B
#�B
bB
PB
JB
DB

=B
+B	��B	�B	�;B	�
B	��B	ĜB	�dB	�3B	�B	��B	��B	��B	��B	��B	�1B	�B	y�B	t�B	p�B	jB	bNB	W
B	Q�B	L�B	I�B	F�B	;dB	�B	
=B��B��B�B�B�mB�`B�TB�;B�#B�)B�B��B��BɺB��B�wB�dB�LB�9B�3B�'B�B�B��B��B��B��B��B��B��B�{B�PB�=B�1B�%B�B�B� B~�B|�Bz�Bx�Bu�Bt�Bq�Bo�Bm�BjBhsBffBdZBbNBaHB_;B^5B`BBaHB_;B^5B^5B^5B]/B]/B]/B\)B[#BYBXBW
BW
BW
BW
BXBXBW
BYBYB]/B_;B`BB_;B_;BbNBdZBhsBjBk�Bl�Bm�Bn�Bo�Bo�Bp�Bp�Bn�Bo�Bn�Bo�Bx�B~�B�B�B�B�B�B�B�+B�JB�VB�{B��B��B��B��B��B��B��B��B�B�!B�!B�!B�'B�'B�!B�'B�-B�3B�3B�3B�3B�9B�9B�9B�FB�dB�qB�}B��BŢB��B��B��B��B��B��B�B�
B�/B�ZB�sB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B		7B	\B	bB	uB	�B	�B	�B	�B	�B	!�B	'�B	,B	-B	/B	1'B	33B	6FB	;dB	>wB	C�B	E�B	G�B	I�B	M�B	R�B	S�B	S�B	W
B	XB	VB	VB	VB	W
B	XB	YB	ZB	ZB	ZB	]/B	\)B	\)B	]/B	]/B	]/B	]/B	]/B	[#B	ffB	m�B	v�B	w�B	w�B	x�B	{�B	}�B	�B	�B	�+B	�1B	�=B	�DB	�JB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�'B	�3B	�?B	�LB	�LB	�LB	�RB	�RB	�dB	�dB	�jB	�qB	�qB	�qB	�}B	��B	B	ŢB	ƨB	ǮB	ȴB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�5B	�BB	�HB	�HB	�ZB	�`B	�fB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
aB
B
'�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140853                              AO  ARCAADJP                                                                    20181024140853    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140853  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140853  QCF$                G�O�G�O�G�O�0               