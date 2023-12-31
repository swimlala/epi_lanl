CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:12Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140812  20181024140812  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ,A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׺�8Q�w1   @׺���A�@3a$�/�c����S�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ,A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�33B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C�C  C  C   C"  C$�C&  C(  C*  C+�fC.  C0  C2  C4�C6  C8  C:  C<  C>  C?�fCA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  Dy�D  D� DfD� D  D� D  D� D  D� D	  D	y�D	��D
y�D  D� D  D� D  D� D  D� D  D� D��Dy�D  D� DfD�fDfD� D  D�fD  D� D  Dy�D��Dy�D  D� D��D� D  D� D  Dy�D��Dy�D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/y�D0  D0� D1  D1� D2  D2� D3fD3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�D@��DAy�DA��DB� DB��DCy�DC��DD� DE  DEy�DE��DF� DF��DGy�DH  DH� DI  DI� DJfDJ�fDKfDK�fDL  DL� DL��DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D_��D`� DafDa� Db  Db�fDc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Do��Dpy�Dp��Dqy�Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dw�3Dy�
D�9�D�Ф111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@\A�HA"�HAB�HAb�HA�p�A�p�A�p�A�=qA�p�A�p�A�p�A��B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�(�B�\)B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B��\B�\)B�\)Bď\Bȏ\B̏\B�\)B�\)B�\)B�\)B��\B�\B�\B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.CzC.C.C.C.CG�C.C.C .C".C$G�C&.C(.C*.C,zC..C0.C2.C4G�C6.C8.C:.C<.C>.C@zCBzCD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.CvG�Cx.Cz.C|.C~.C�
C�
C�
C�
=C�
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
=C�
=C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
=C�
C�
C�
C�#�C�#�C�
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
C�
=C�
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
C�#�C�
C�
C�
C�
C�
=C�
=C�
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
C�
C�
D �D ��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D	�D	�D
D
�D�D��D�D��D�D��D�D��D�D��DD�D�D��D�D��D�D��D�D��D�D��D�D�DD�D�D��DD��D�D��D�D�DD�D�D��D�D�D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/D/�D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@�DADA�DBDB��DCDC�DDDD��DE�DE�DFDF��DGDG�DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DMDM��DN�DN��DO�DO��DP�DP��DQ�DQ�DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[D[��D\�D\��D]�D]��D^�D^��D_�D_��D`D`��Da�Da��Db�Db��Dc�Dc�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�Dn�Dn��Do�Do��DpDp�DqDq�Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw޸Dy��D�?�D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�oAե�A�|�A�|�A�|�A�dZA�M�A�VA��HA�(�A��A�G�AΉ7A�=qȂhA�z�A�
=A�1AȲ-A��HA��AŁA�oA��HAė�A�$�A��;AÇ+A�bNA�l�A� �A�S�A�-A���A�"�A��
A��A�5?A��#A�n�A���A�O�A��jA��A�%A�v�A��#A�A��jA��A�z�A�
=A��A�A�A�$�A��A�K�A�+A�l�A��yA��
A� �A��#A�ffA�ƨA�jA�33A��A��
A�ffA�
=A���A���A�\)A���A�oA�|�A��A�VA��A�ffA�"�A�VA��A���A�z�A�A��A�{A��A��DA�p�A��A��A��\A�?}A���A�dZA�`BA�A|�!Ay�;Ax�Avn�AnjAi��AiVAhVAf-AcƨA]��AZ��AWl�AVVAU��AT�HAR��AQ�AN��AK7LAJJAI�FAHĜAF��ACx�A?��A<{A;
=A9��A8�A7XA6�/A6�\A5��A49XA1��A1��A1l�A0�9A/�wA.ȴA-��A,n�A,�A+�7A*�DA*1A)ƨA)�A)�A)/A(I�A'%A%G�A$ȴA$�uA#�wA#/A"�yA"E�A!p�A �/A�AS�AZAJA7LA�!A�Az�A��A�wA33A�7AZA�mAhsA;dA�yA�A;dAVA��A�A��A�7A|�AS�A  A9XAC�A
��A
v�A�AƨA�AQ�A7LAQ�AAA7LA z�@�l�@��7@�K�@�5?@�p�@���@��#@�X@�"�@�@�~�@�?}@���@�Z@�F@��@�%@��@��@�hs@�u@�j@�9X@�  @��@�^5@���@���@ݡ�@ܣ�@�S�@�~�@��#@�z�@�ȴ@Ձ@ӝ�@҇+@�X@�Ĝ@�Q�@�5?@̋D@˅@��@�v�@�p�@��;@ư!@�@�7L@ģ�@�Z@���@�ȴ@�^5@��@�/@���@� �@�ƨ@�J@���@�I�@�b@���@��P@�l�@�C�@�@��@�v�@�5?@��@��@��@�O�@�1'@��@��;@��;@��F@�|�@�|�@�\)@���@��R@��!@�^5@��@���@�7L@���@��9@�Z@�b@���@�"�@�ȴ@�{@��-@���@���@�x�@�p�@�`B@�?}@���@�t�@�ȴ@��R@�o@�|�@��+@���@��@�%@��`@�Ĝ@��@���@��@��@��@���@�$�@��@�bN@��P@��H@�ȴ@�K�@�|�@��R@���@��@���@��7@�p�@�X@��/@�j@�I�@� �@�|�@��H@�~�@�^5@��@�@��@�A�@�\)@��y@��R@���@���@��\@�n�@�M�@��@��T@��-@���@�r�@�j@�Q�@�I�@�A�@�(�@�1@��
@���@�t�@�"�@���@�=q@�J@���@��@���@�?}@���@��@�bN@�b@��F@�|�@�t�@�l�@�dZ@�;d@�n�@�{@���@���@�x�@�`B@�7L@�&�@�V@��@��j@���@��D@�j@��@���@��m@��w@��P@�\)@�@���@���@�E�@�@��@�?}@���@��@���@��;@��F@�
=@��!@�=q@��@��@��^@���@�O�@��@��@��@�A�@�b@�1@�1@�1@���@��@�
=@���@��@���@��@��T@���@���@��@�7L@��9@�Z@�  @��;@��
@���@�ƨ@��@��@�S�@�o@��@�~�@�=q@��@��T@�O�@�V@��/@�Z@�b@��m@��w@���@�|�@�C�@�o@��@���@��!@���@���@�v�@�$�@�@��#@��^@��-@��^@��h@�-w@rC�@`�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�oAե�A�|�A�|�A�|�A�dZA�M�A�VA��HA�(�A��A�G�AΉ7A�=qȂhA�z�A�
=A�1AȲ-A��HA��AŁA�oA��HAė�A�$�A��;AÇ+A�bNA�l�A� �A�S�A�-A���A�"�A��
A��A�5?A��#A�n�A���A�O�A��jA��A�%A�v�A��#A�A��jA��A�z�A�
=A��A�A�A�$�A��A�K�A�+A�l�A��yA��
A� �A��#A�ffA�ƨA�jA�33A��A��
A�ffA�
=A���A���A�\)A���A�oA�|�A��A�VA��A�ffA�"�A�VA��A���A�z�A�A��A�{A��A��DA�p�A��A��A��\A�?}A���A�dZA�`BA�A|�!Ay�;Ax�Avn�AnjAi��AiVAhVAf-AcƨA]��AZ��AWl�AVVAU��AT�HAR��AQ�AN��AK7LAJJAI�FAHĜAF��ACx�A?��A<{A;
=A9��A8�A7XA6�/A6�\A5��A49XA1��A1��A1l�A0�9A/�wA.ȴA-��A,n�A,�A+�7A*�DA*1A)ƨA)�A)�A)/A(I�A'%A%G�A$ȴA$�uA#�wA#/A"�yA"E�A!p�A �/A�AS�AZAJA7LA�!A�Az�A��A�wA33A�7AZA�mAhsA;dA�yA�A;dAVA��A�A��A�7A|�AS�A  A9XAC�A
��A
v�A�AƨA�AQ�A7LAQ�AAA7LA z�@�l�@��7@�K�@�5?@�p�@���@��#@�X@�"�@�@�~�@�?}@���@�Z@�F@��@�%@��@��@�hs@�u@�j@�9X@�  @��@�^5@���@���@ݡ�@ܣ�@�S�@�~�@��#@�z�@�ȴ@Ձ@ӝ�@҇+@�X@�Ĝ@�Q�@�5?@̋D@˅@��@�v�@�p�@��;@ư!@�@�7L@ģ�@�Z@���@�ȴ@�^5@��@�/@���@� �@�ƨ@�J@���@�I�@�b@���@��P@�l�@�C�@�@��@�v�@�5?@��@��@��@�O�@�1'@��@��;@��;@��F@�|�@�|�@�\)@���@��R@��!@�^5@��@���@�7L@���@��9@�Z@�b@���@�"�@�ȴ@�{@��-@���@���@�x�@�p�@�`B@�?}@���@�t�@�ȴ@��R@�o@�|�@��+@���@��@�%@��`@�Ĝ@��@���@��@��@��@���@�$�@��@�bN@��P@��H@�ȴ@�K�@�|�@��R@���@��@���@��7@�p�@�X@��/@�j@�I�@� �@�|�@��H@�~�@�^5@��@�@��@�A�@�\)@��y@��R@���@���@��\@�n�@�M�@��@��T@��-@���@�r�@�j@�Q�@�I�@�A�@�(�@�1@��
@���@�t�@�"�@���@�=q@�J@���@��@���@�?}@���@��@�bN@�b@��F@�|�@�t�@�l�@�dZ@�;d@�n�@�{@���@���@�x�@�`B@�7L@�&�@�V@��@��j@���@��D@�j@��@���@��m@��w@��P@�\)@�@���@���@�E�@�@��@�?}@���@��@���@��;@��F@�
=@��!@�=q@��@��@��^@���@�O�@��@��@��@�A�@�b@�1@�1@�1@���@��@�
=@���@��@���@��@��T@���@���@��@�7L@��9@�Z@�  @��;@��
@���@�ƨ@��@��@�S�@�o@��@�~�@�=q@��@��T@�O�@�V@��/@�Z@�b@��m@��w@���@�|�@�C�@�o@��@���@��!@���@���@�v�@�$�@�@��#@��^@��-@��^@��h@�-w@rC�@`�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BYB\)B]/B]/B]/B^5B]/B\)BdZBn�Bw�B}�B�%B��B��B�3B�RBƨB�)B  BVB�B�B!�B"�B�B�B/B33B?}BF�BL�BT�BffBs�Bz�B}�B� B�B~�B|�B}�B� B�B�=B�PB��B��B��B�'B�?B�FB�FB�wB��B��B��B�VBgmBXB]/Bn�B~�B�BP�BC�B;dB8RB"�BB�B�NB�5B�
B��B��B�^B�oBy�BiyB]/BdZBiyBhsBdZBaHBO�BA�B2-BhB
��B
�yB
�B
ŢB
�-B
�hB
|�B
gmB
J�B
(�B
{B
B	��B	�NB	�!B	��B	�uB	�JB	~�B	n�B	L�B	;dB	-B	%�B	 �B	�B	PB	B��B�B�B�mB�NB�B��BÖB�9B�!B�B�B�B��B��B��B��B��B��B��B��B�B�9B�^B�dB�^B�dB�dB�^B�dB�dB�jB�wBƨB��B��B��B��B��B��B��BɺBȴB�B�B�
B�
B�B�/B�5B�;B�mB�B�yB�fB�`B�sB�mB�fB�`B�fB�`B�TB�TB�TB�ZB�`B�`B�`B�TB�BB�B�B�B��B��BɺB��BĜB�}B�^B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�!B�!B�B�B�-B�9B�FB�LB�RB�XB�XB�^B�dB�wB�}BÖBŢBǮBȴBȴB��B��B��B��B��B�
B�TB�sB�B�B�B�B��B��B��B��B	B	1B	+B	+B	%B	+B	+B	1B		7B	DB	JB	JB	JB	PB	bB	uB	oB	uB	�B	�B	"�B	%�B	&�B	&�B	'�B	(�B	(�B	)�B	.B	.B	.B	0!B	2-B	49B	6FB	8RB	9XB	;dB	<jB	@�B	D�B	H�B	N�B	YB	_;B	ffB	k�B	l�B	o�B	s�B	w�B	u�B	v�B	y�B	}�B	�+B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�FB	�RB	�RB	�RB	�^B	�jB	�jB	��B	��B	B	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�;B	�BB	�BB	�BB	�BB	�TB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
hB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
eB
'8B
7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BYB\)B]/B]/B]/B^5B]/B\)BdZBn�Bw�B}�B�%B��B��B�3B�RBƨB�)B  BVB�B�B!�B"�B�B�B/B33B?}BF�BL�BT�BffBs�Bz�B}�B� B�B~�B|�B}�B� B�B�=B�PB��B��B��B�'B�?B�FB�FB�wB��B��B��B�VBgmBXB]/Bn�B~�B�BP�BC�B;dB8RB"�BB�B�NB�5B�
B��B��B�^B�oBy�BiyB]/BdZBiyBhsBdZBaHBO�BA�B2-BhB
��B
�yB
�B
ŢB
�-B
�hB
|�B
gmB
J�B
(�B
{B
B	��B	�NB	�!B	��B	�uB	�JB	~�B	n�B	L�B	;dB	-B	%�B	 �B	�B	PB	B��B�B�B�mB�NB�B��BÖB�9B�!B�B�B�B��B��B��B��B��B��B��B��B�B�9B�^B�dB�^B�dB�dB�^B�dB�dB�jB�wBƨB��B��B��B��B��B��B��BɺBȴB�B�B�
B�
B�B�/B�5B�;B�mB�B�yB�fB�`B�sB�mB�fB�`B�fB�`B�TB�TB�TB�ZB�`B�`B�`B�TB�BB�B�B�B��B��BɺB��BĜB�}B�^B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�!B�!B�B�B�-B�9B�FB�LB�RB�XB�XB�^B�dB�wB�}BÖBŢBǮBȴBȴB��B��B��B��B��B�
B�TB�sB�B�B�B�B��B��B��B��B	B	1B	+B	+B	%B	+B	+B	1B		7B	DB	JB	JB	JB	PB	bB	uB	oB	uB	�B	�B	"�B	%�B	&�B	&�B	'�B	(�B	(�B	)�B	.B	.B	.B	0!B	2-B	49B	6FB	8RB	9XB	;dB	<jB	@�B	D�B	H�B	N�B	YB	_;B	ffB	k�B	l�B	o�B	s�B	w�B	u�B	v�B	y�B	}�B	�+B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�-B	�3B	�FB	�RB	�RB	�RB	�^B	�jB	�jB	��B	��B	B	ÖB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�;B	�BB	�BB	�BB	�BB	�TB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
hB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
eB
'8B
7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140812                              AO  ARCAADJP                                                                    20181024140812    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140812  QCP$                G�O�G�O�G�O�F03E            AO  ARGQQCPL                                                                    20181024140812  QCF$                G�O�G�O�G�O�0               