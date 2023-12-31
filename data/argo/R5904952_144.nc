CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:37Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190537  20181005190537  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�+P�1   @��j8� @0��Q��c��t�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A!��AA��A`  A�  A�  A�  A�  A���A�  A�  A�33A�33B��B��B��B   B(  B0  B8  B@  BHffBPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD ��Dy�D  D�fD  D� D  Dy�D  D� D  Dy�D��D� D  D� D	  D	� D
fD
� D  D�fDfD�fDfD� D��D� DfD� D  D� D  D�fDfD� DfD� D  D� D��Dy�D  D� DfD�fD  Dy�D  D�fDfD� D  D�fD  D� DfD�fDfD� D  D� D   D � D!  D!�fD"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,y�D,��D-� D.  D.� D/  D/�fD0fD0�fD1  D1� D2  D2�fD3  D3y�D3��D4y�D4��D5y�D6  D6y�D6��D7y�D7��D8� D9  D9� D:  D:y�D;  D;� D;��D<y�D<��D=y�D=��D>y�D>��D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DDy�DE  DE�fDFfDF� DGfDG� DH  DH�fDI  DIy�DJ  DJ� DKfDK�fDLfDL�fDM  DM� DNfDN�fDOfDO� DP  DP� DQ  DQ� DQ��DRy�DS  DS�fDT  DT� DU  DU� DV  DV�fDWfDW� DW��DXy�DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D]��D^� D_  D_y�D_��D`� DafDa� Db  Db� Dc  Dc� DdfDd�fDe  De� Df  Df� DgfDg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� DrfDr�fDsfDs�fDtfDt�fDu  Duy�Du��Dvy�Dw  Dw�fDw�3Dy�D�@ D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�A�HA$z�ADz�Ab�HA�p�A�p�A�p�A�p�A�=qA�p�A�p�A��B Q�BQ�BQ�BQ�B �RB(�RB0�RB8�RB@�RBI�BQ�BY�B`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�(�B�\)B��\B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0G�C2.C4.C6.C8.C:.C<.C>zC@.CB.CD.CF.CH.CJ.CLzCN.CP.CR.CT.CVG�CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�#�C�
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
C�
=C�
C�#�C�#�C�
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
C�
C�
C�
C�#�C�#�C�
C�
=C�
C�
C�#�C�
C�
C�
C�
C�
=C�
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
C�
=C�
=C�
C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
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
C�#�C�#�C�#�C�
C�
=C�
C�
C�
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
C�
C�
D �D ��DD�D�D��D�D��D�D�D�D��D�D�DD��D�D��D	�D	��D
�D
��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D��DD�D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*D*��D+�D+��D,�D,�D-D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3�D4D4�D5D5�D6�D6�D7D7�D8D8��D9�D9��D:�D:�D;�D;��D<D<�D=D=�D>D>�D?D?��D@�D@��DA�DA�DB�DB��DC�DC��DD�DD�DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DRDR�DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DXDX�DY�DY��DZ�DZ��D[�D[��D\�D\�D]�D]��D^D^��D_�D_�D`D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du�DvDv�Dw�Dw��Dw޸Dy��D�E�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ƨA�ȴA�ȴA���A���A���A�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A��
A��
A��
A��
A��A��#A��/A��/A��HA��mA��yA��A��A��A���A���A���A���A���A���A���A���A���A���A���A��A��yA��A���A�z�A�~�A�ffA�\)A�O�A�=qA��A�x�A�dZA�E�A�oAϸRA��A�VA͇+A̟�A�&�A�VA���A��
A�I�A�bA��/AœuA\A�ȴA���A��!A���A��A��A��HA��PA�n�A�1'A���A�bNA��!A���A�{A�VA�dZA�|�A���A�JA�r�A���A�p�A�1'A���A�~�A��A�ȴA�jA�A��A�|�A�A�(�A��;A���A�33A�z�A�;dA��jA�r�A���A�"�A�O�A�VA���A�l�A���A�t�A~ZA}Av��Ar5?Ap�Am�wAiXAc��A`�/A_dZA]�FAY��ATffARJAM��AI�AG|�AE��AD�AC|�AAA>�yA<ĜA;l�A:��A:�A8bNA7��A61'A5�7A4��A3ƨA0��A-��A,�/A,M�A+�-A*v�A'�A%��A%+A$�A"�A bAZAO�A�AQ�Az�AȴA��A��AVA^5A��A�A��A�A�AA��A~�AJA��A�TA"�A��A�\A��A�A
�`A
�!A
bA	/AĜAr�A�A��A�!A��AVA�A��A�9A�\AbA
=@���@��@�"�@�
=@��@���@���@��
@�{@���@�K�@�t�@�33@��
@���@�`B@��@�!@�5?@�%@�@�J@���@�1@◍@�{@��@�@��@߶F@�|�@�S�@ܛ�@�+@�~�@���@���@�v�@�=q@�J@���@���@٩�@�V@���@ؼj@�z�@�I�@�(�@���@ם�@֟�@�hs@ӶF@�V@��#@�X@���@ЋD@�ƨ@�;d@θR@�$�@���@�?}@�bN@��;@�+@�ff@�@��@��/@ǥ�@�+@��@���@�V@ļj@�(�@�C�@��@\@�J@��@�"�@��h@�?}@��@�V@��-@��@��;@���@�33@��H@�@���@���@��P@���@���@�C�@���@���@���@��\@�ff@���@��j@���@���@��/@��/@�r�@��@��;@�t�@�C�@�"�@��@���@���@��\@�=q@�5?@�$�@��@�p�@���@��@��P@�+@��y@�v�@�5?@��7@�@���@��@�G�@��9@���@��@��w@�ƨ@��@���@�|�@��@���@�E�@�{@��@���@�A�@��@��@��R@�J@���@�p�@�?}@�V@��@�I�@�b@��F@��P@�t�@�dZ@�\)@�S�@�C�@�33@�
=@�~�@���@���@�x�@�7L@���@�bN@�  @�ƨ@�C�@�ȴ@�V@��#@�hs@�O�@�?}@��@��/@���@��D@�I�@�A�@�9X@��@��
@���@��@�t�@�l�@�33@�@��H@�~�@�$�@���@��-@���@��h@�`B@���@���@�9X@���@�C�@�;d@��@�
=@��H@�ȴ@���@�=q@�-@���@���@�p�@�`B@�O�@��@���@�Ĝ@��9@��D@��@�j@�Q�@�A�@��@��w@��P@�l�@�+@��@��+@�V@�M�@�J@��h@�O�@��@��@��@�I�@��;@���@���@��P@�\)@���@�V@�5?@��@�{@��@��^@��h@��@�hs@��@��j@���@� �@��@�\)@�+@��y@�ȴ@���@�V@���@��#@���@���@�Ɇ@i�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ƨA�ȴA�ȴA���A���A���A�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A��
A��
A��
A��
A��A��#A��/A��/A��HA��mA��yA��A��A��A���A���A���A���A���A���A���A���A���A���A���A��A��yA��A���A�z�A�~�A�ffA�\)A�O�A�=qA��A�x�A�dZA�E�A�oAϸRA��A�VA͇+A̟�A�&�A�VA���A��
A�I�A�bA��/AœuA\A�ȴA���A��!A���A��A��A��HA��PA�n�A�1'A���A�bNA��!A���A�{A�VA�dZA�|�A���A�JA�r�A���A�p�A�1'A���A�~�A��A�ȴA�jA�A��A�|�A�A�(�A��;A���A�33A�z�A�;dA��jA�r�A���A�"�A�O�A�VA���A�l�A���A�t�A~ZA}Av��Ar5?Ap�Am�wAiXAc��A`�/A_dZA]�FAY��ATffARJAM��AI�AG|�AE��AD�AC|�AAA>�yA<ĜA;l�A:��A:�A8bNA7��A61'A5�7A4��A3ƨA0��A-��A,�/A,M�A+�-A*v�A'�A%��A%+A$�A"�A bAZAO�A�AQ�Az�AȴA��A��AVA^5A��A�A��A�A�AA��A~�AJA��A�TA"�A��A�\A��A�A
�`A
�!A
bA	/AĜAr�A�A��A�!A��AVA�A��A�9A�\AbA
=@���@��@�"�@�
=@��@���@���@��
@�{@���@�K�@�t�@�33@��
@���@�`B@��@�!@�5?@�%@�@�J@���@�1@◍@�{@��@�@��@߶F@�|�@�S�@ܛ�@�+@�~�@���@���@�v�@�=q@�J@���@���@٩�@�V@���@ؼj@�z�@�I�@�(�@���@ם�@֟�@�hs@ӶF@�V@��#@�X@���@ЋD@�ƨ@�;d@θR@�$�@���@�?}@�bN@��;@�+@�ff@�@��@��/@ǥ�@�+@��@���@�V@ļj@�(�@�C�@��@\@�J@��@�"�@��h@�?}@��@�V@��-@��@��;@���@�33@��H@�@���@���@��P@���@���@�C�@���@���@���@��\@�ff@���@��j@���@���@��/@��/@�r�@��@��;@�t�@�C�@�"�@��@���@���@��\@�=q@�5?@�$�@��@�p�@���@��@��P@�+@��y@�v�@�5?@��7@�@���@��@�G�@��9@���@��@��w@�ƨ@��@���@�|�@��@���@�E�@�{@��@���@�A�@��@��@��R@�J@���@�p�@�?}@�V@��@�I�@�b@��F@��P@�t�@�dZ@�\)@�S�@�C�@�33@�
=@�~�@���@���@�x�@�7L@���@�bN@�  @�ƨ@�C�@�ȴ@�V@��#@�hs@�O�@�?}@��@��/@���@��D@�I�@�A�@�9X@��@��
@���@��@�t�@�l�@�33@�@��H@�~�@�$�@���@��-@���@��h@�`B@���@���@�9X@���@�C�@�;d@��@�
=@��H@�ȴ@���@�=q@�-@���@���@�p�@�`B@�O�@��@���@�Ĝ@��9@��D@��@�j@�Q�@�A�@��@��w@��P@�l�@�+@��@��+@�V@�M�@�J@��h@�O�@��@��@��@�I�@��;@���@���@��P@�\)@���@�V@�5?@��@�{@��@��^@��h@��@�hs@��@��j@���@� �@��@�\)@�+@��y@�ȴ@���@�V@���@��#@���@���@�Ɇ@i�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Be`BffBgmBffBffBffBgmBe`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`BffBffBgmBhsBhsBjBk�Bl�Bl�Bm�Bn�Bo�Bp�Bo�Bo�Bo�Bo�Bp�Bp�Br�Bs�Br�Bu�By�By�Bz�Bz�Bz�Bz�Bz�B}�B~�B~�B}�B}�B~�B� B�B�uB��B��B��B�B�XB�ZB�B�BB�B�NB��B49B<jBI�BN�BO�BR�Bn�Bp�Br�B�VB�bB�\B�7B}�Bm�Be`B]/BI�B7LB�BPB��B�BB��BȴBÖB�9B��B��B�oB�JB�Bw�BiyB_;BK�B'�B%B
�B
�9B
��B
\)B
'�B
PB	�B	�mB	��B	�FB	��B	�B	aHB	Q�B	5?B	�B	  B�sB�B��B�?B��B��B�1B�B~�B|�B|�B{�Bz�B{�B~�B�B�B�B�+B�7B�VB�bB�hB��B��B�'B�FB�?B�9B�9B�9B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�3B�9B�?B�FB�qBŢBĜBÖBƨBǮB��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�5B�;B�5B�;B�mB�B�B�sB�fB�B�B�BB�B��BɺB��B��B��B�B�B�#B�BB�B�B�B�B�B�B�B�B�B�B�B��B	%B	1B		7B		7B	
=B		7B		7B	JB	VB	VB	bB	{B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	'�B	)�B	)�B	,B	0!B	2-B	33B	5?B	:^B	<jB	=qB	A�B	?}B	@�B	C�B	H�B	J�B	K�B	K�B	L�B	M�B	P�B	Q�B	P�B	O�B	N�B	O�B	S�B	R�B	P�B	O�B	N�B	O�B	T�B	T�B	P�B	Q�B	Q�B	Q�B	P�B	O�B	P�B	S�B	T�B	VB	XB	YB	ZB	[#B	\)B	]/B	aHB	bNB	cTB	e`B	ffB	iyB	jB	n�B	q�B	v�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	�B	� B	�B	�B	�B	�+B	�+B	�1B	�1B	�=B	�DB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�9B	�FB	�FB	�^B	�jB	�jB	�jB	�qB	�wB	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�NB	�ZB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
1B
	7B

=B
DB
DB
DB
DB
DB
JB
DB
hB

�B
"�B
/O222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Be`BffBgmBffBffBffBgmBe`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`Be`BffBffBgmBhsBhsBjBk�Bl�Bl�Bm�Bn�Bo�Bp�Bo�Bo�Bo�Bo�Bp�Bp�Br�Bs�Br�Bu�By�By�Bz�Bz�Bz�Bz�Bz�B}�B~�B~�B}�B}�B~�B� B�B�uB��B��B��B�B�XB�ZB�B�BB�B�NB��B49B<jBI�BN�BO�BR�Bn�Bp�Br�B�VB�bB�\B�7B}�Bm�Be`B]/BI�B7LB�BPB��B�BB��BȴBÖB�9B��B��B�oB�JB�Bw�BiyB_;BK�B'�B%B
�B
�9B
��B
\)B
'�B
PB	�B	�mB	��B	�FB	��B	�B	aHB	Q�B	5?B	�B	  B�sB�B��B�?B��B��B�1B�B~�B|�B|�B{�Bz�B{�B~�B�B�B�B�+B�7B�VB�bB�hB��B��B�'B�FB�?B�9B�9B�9B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�3B�3B�9B�?B�FB�qBŢBĜBÖBƨBǮB��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�5B�;B�5B�;B�mB�B�B�sB�fB�B�B�BB�B��BɺB��B��B��B�B�B�#B�BB�B�B�B�B�B�B�B�B�B�B�B��B	%B	1B		7B		7B	
=B		7B		7B	JB	VB	VB	bB	{B	�B	�B	�B	�B	!�B	"�B	$�B	&�B	'�B	)�B	)�B	,B	0!B	2-B	33B	5?B	:^B	<jB	=qB	A�B	?}B	@�B	C�B	H�B	J�B	K�B	K�B	L�B	M�B	P�B	Q�B	P�B	O�B	N�B	O�B	S�B	R�B	P�B	O�B	N�B	O�B	T�B	T�B	P�B	Q�B	Q�B	Q�B	P�B	O�B	P�B	S�B	T�B	VB	XB	YB	ZB	[#B	\)B	]/B	aHB	bNB	cTB	e`B	ffB	iyB	jB	n�B	q�B	v�B	y�B	z�B	{�B	|�B	|�B	}�B	~�B	�B	� B	�B	�B	�B	�+B	�+B	�1B	�1B	�=B	�DB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�-B	�-B	�-B	�9B	�FB	�FB	�^B	�jB	�jB	�jB	�qB	�wB	��B	��B	B	ÖB	ĜB	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�NB	�ZB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
1B
	7B

=B
DB
DB
DB
DB
DB
JB
DB
hB

�B
"�B
/O222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190537                              AO  ARCAADJP                                                                    20181005190537    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190537  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190537  QCF$                G�O�G�O�G�O�8000            