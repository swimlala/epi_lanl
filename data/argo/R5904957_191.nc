CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:40Z creation      
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \P   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  g�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  wp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140840  20181024140840  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d��u1   @��e:�B@5^5?|��d����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33C �C�C�C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*�C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9�fD:fD:� D;  D;� D<  D<�fD=fD=� D>  D>� D?  D?� D@  D@y�D@��DA� DB  DB� DC  DC� DD  DD�fDEfDE�fDF  DF� DG  DG� DG��DHy�DH��DI� DJ  DJ�fDKfDK� DL  DL� DM  DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� DefDe� Df  Df� DgfDg� Dh  Dh� DifDi� Dj  Dj� Dk  Dky�Dl  Dl� DmfDm�fDn  Dn� Do  Do�fDpfDp� Dq  Dq� Dr  Dry�Dr��Dsy�Dt  Dt� Du  Du�fDv  Dv� Dw  Dw�fDx  D�L)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��H@��HAp�A%p�AEp�Aep�A��RA��RA��RA��RA¸RAҸRA�RA�RBB	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)BiBqBy\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�z�B�z�B��B��GC p�Cp�Cp�CW
C=pC
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$=pC&W
C(W
C*p�C,W
C.=pC0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@p�CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
Cn=pCpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C��C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C�+�C�+�C��C�+�C�+�C�+�C��C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D)D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�]D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7)D7��D8�D8��D9�D9�)D:)D:��D;�D;��D<�D<�)D=)D=��D>�D>��D?�D?��D@�D@�]DA]DA��DB�DB��DC�DC��DD�DD�)DE)DE�)DF�DF��DG�DG��DH]DH�]DI]DI��DJ�DJ�)DK)DK��DL�DL��DM�DM�]DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW]DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc)Dc��Dd�Dd��De)De��Df�Df��Dg)Dg��Dh�Dh��Di)Di��Dj�Dj��Dk�Dk�]Dl�Dl��Dm)Dm�)Dn�Dn��Do�Do�)Dp)Dp��Dq�Dq��Dr�Dr�]Ds]Ds�]Dt�Dt��Du�Du�)Dv�Dv��Dw�Dw�)Dx�D�W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�5?A�5?A�7LA�9XA�=qA�O�A�G�A��A���A͙�A�jA�O�A�?}A�7LA�33A�33A�/A�+A� �A��A�VA��A̬A�$�A�1'A��;A�
=AȍPA�1'Aŏ\A�
=A�-AhA��A���A��A���A�r�A�Q�A�33A�"�A��A�ȴA��7A�+A��;A��A��FA�33A���A��yA��A�5?A�?}A�t�A���A�VA���A�M�A���A��^A��A�  A��A���A�=qA��A�-A��HA�p�A���A�|�A���A�ZA�/A��RA��A���A�`BA�9XA���A�`BA�n�A��TA�ZA�hsA�|�A��/A��;A��^A���A���A��A��A��HA��A��A�oA��A�A~VA|��Az{AwAu�hAuK�At�`AtA�Asp�Ap�9Am�#Aj�9Af�AfAd�Aa��AaoA`�RA_��A^�jA]hsA[|�AZ$�AY/AW�wAUC�AR�`AQ�#AN(�ALbAK33AJz�AI�FAH^5AGl�AFI�AE;dAD�/AD��AC�AB{A@VA>jA<��A;��A;?}A9A6z�A4ȴA2�+A/��A-�;A+t�A*��A*JA)%A(-A'��A't�A'oA&��A%��A%��A$��A#?}A!|�A ��A\)A^5A�wA��A�7Ap�A�Ap�AVA�mA�9A��AK�AVAbA/A��A�7A��A�A=qAO�AjAC�AoA�A/Az�A
(�A	S�A�RA�PA��A�uA=qAƨAC�A�9A�A��A�hA 1@�K�@��H@�5?@��@�~�@�Q�@�bN@��@�-@��@�K�@��/@���@�ȴ@��@�7L@䛦@��@���@�R@�n�@�E�@�E�@�E�@�$�@�G�@���@��y@�x�@�|�@���@ڏ\@�J@�p�@�z�@ו�@���@�v�@�$�@պ^@�G�@���@� �@�|�@�x�@�Ĝ@̛�@˶F@���@���@ǝ�@�&�@���@�^5@��@�M�@���@��@�l�@��@��j@�dZ@� �@���@���@��@�`B@���@���@���@�
=@�r�@���@��H@���@�X@��h@�p�@��@�Z@��@��m@�ƨ@��@�l�@�"�@�@�ff@��#@�?}@�z�@�1'@��@�dZ@��@���@���@�$�@��h@�7L@��@�X@��@��/@�Ĝ@���@�9X@� �@���@�\)@�K�@��@�A�@�7L@���@��@�  @��D@��@�Q�@�9X@�`B@���@�  @�(�@�Z@�  @�;d@�n�@�&�@�G�@��@��T@��T@�x�@�hs@�p�@��7@���@�J@�n�@�M�@�p�@�%@�bN@�I�@��u@��@�z�@��@���@�V@��h@���@�9X@��
@�dZ@��!@���@��@���@��@�t�@�l�@�\)@�+@�ȴ@��T@���@�o@��y@���@���@�ff@��#@�O�@�?}@���@���@��@��`@���@���@���@��j@��9@��@���@�j@�bN@��@��w@�dZ@���@�dZ@��@���@�v�@�-@���@�p�@�`B@�O�@�O�@���@���@�bN@�1'@�  @���@�l�@�;d@��@���@�^5@��@��@���@���@�hs@�?}@�7L@�/@�/@���@�r�@�  @��@�"�@���@���@���@���@��h@�x�@���@���@��j@���@� �@��P@��@�t�@�C�@�+@��@��y@��@��@���@���@��+@�E�@�{@��h@�O�@�7L@��@���@��/@��u@�Q�@�b@���@�\)@��@�v�@�E�@�$�@�{@���@��T@��^@��h@�p�@�V@�bN@���@��;@��w@�K�@�"�@�+@�~�@��@y��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�33A�5?A�5?A�7LA�9XA�=qA�O�A�G�A��A���A͙�A�jA�O�A�?}A�7LA�33A�33A�/A�+A� �A��A�VA��A̬A�$�A�1'A��;A�
=AȍPA�1'Aŏ\A�
=A�-AhA��A���A��A���A�r�A�Q�A�33A�"�A��A�ȴA��7A�+A��;A��A��FA�33A���A��yA��A�5?A�?}A�t�A���A�VA���A�M�A���A��^A��A�  A��A���A�=qA��A�-A��HA�p�A���A�|�A���A�ZA�/A��RA��A���A�`BA�9XA���A�`BA�n�A��TA�ZA�hsA�|�A��/A��;A��^A���A���A��A��A��HA��A��A�oA��A�A~VA|��Az{AwAu�hAuK�At�`AtA�Asp�Ap�9Am�#Aj�9Af�AfAd�Aa��AaoA`�RA_��A^�jA]hsA[|�AZ$�AY/AW�wAUC�AR�`AQ�#AN(�ALbAK33AJz�AI�FAH^5AGl�AFI�AE;dAD�/AD��AC�AB{A@VA>jA<��A;��A;?}A9A6z�A4ȴA2�+A/��A-�;A+t�A*��A*JA)%A(-A'��A't�A'oA&��A%��A%��A$��A#?}A!|�A ��A\)A^5A�wA��A�7Ap�A�Ap�AVA�mA�9A��AK�AVAbA/A��A�7A��A�A=qAO�AjAC�AoA�A/Az�A
(�A	S�A�RA�PA��A�uA=qAƨAC�A�9A�A��A�hA 1@�K�@��H@�5?@��@�~�@�Q�@�bN@��@�-@��@�K�@��/@���@�ȴ@��@�7L@䛦@��@���@�R@�n�@�E�@�E�@�E�@�$�@�G�@���@��y@�x�@�|�@���@ڏ\@�J@�p�@�z�@ו�@���@�v�@�$�@պ^@�G�@���@� �@�|�@�x�@�Ĝ@̛�@˶F@���@���@ǝ�@�&�@���@�^5@��@�M�@���@��@�l�@��@��j@�dZ@� �@���@���@��@�`B@���@���@���@�
=@�r�@���@��H@���@�X@��h@�p�@��@�Z@��@��m@�ƨ@��@�l�@�"�@�@�ff@��#@�?}@�z�@�1'@��@�dZ@��@���@���@�$�@��h@�7L@��@�X@��@��/@�Ĝ@���@�9X@� �@���@�\)@�K�@��@�A�@�7L@���@��@�  @��D@��@�Q�@�9X@�`B@���@�  @�(�@�Z@�  @�;d@�n�@�&�@�G�@��@��T@��T@�x�@�hs@�p�@��7@���@�J@�n�@�M�@�p�@�%@�bN@�I�@��u@��@�z�@��@���@�V@��h@���@�9X@��
@�dZ@��!@���@��@���@��@�t�@�l�@�\)@�+@�ȴ@��T@���@�o@��y@���@���@�ff@��#@�O�@�?}@���@���@��@��`@���@���@���@��j@��9@��@���@�j@�bN@��@��w@�dZ@���@�dZ@��@���@�v�@�-@���@�p�@�`B@�O�@�O�@���@���@�bN@�1'@�  @���@�l�@�;d@��@���@�^5@��@��@���@���@�hs@�?}@�7L@�/@�/@���@�r�@�  @��@�"�@���@���@���@���@��h@�x�@���@���@��j@���@� �@��P@��@�t�@�C�@�+@��@��y@��@��@���@���@��+@�E�@�{@��h@�O�@�7L@��@���@��/@��u@�Q�@�b@���@�\)@��@�v�@�E�@�$�@�{@���@��T@��^@��h@�p�@�V@�bN@���@��;@��w@�K�@�"�@�+@�~�@��@y��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1B1B1B1B	7B	7B�B�B<jBs�B��B�-B�jBBŢBƨBǮBɺB��B�B�B�NB�BBuB�B�B!�B6FBF�B[#BcTB`BB[#BZBR�BP�BP�BP�BP�BP�BO�BO�BO�BO�BN�BS�BdZBs�By�Bz�By�Bx�Bw�Br�Bm�BiyBe`BdZB^5BZBW
BJ�BC�B?}B7LB0!B�BB�B�B�`B�HB�#B��BÖB�!B�bBt�BbNBW
BG�B0!BB
�B
�sB
�B
��B
ÖB
�-B
��B
�=B
w�B
�1B
��B
�PB
�%B
}�B
jB
Q�B
<jB
)�B
�B

=B	��B	�B	�B	�B	�`B	�5B	��B	�^B	��B	�uB	�VB	�B	x�B	s�B	p�B	k�B	dZB	]/B	S�B	L�B	E�B	>wB	1'B	&�B	�B	oB		7B	B	B��B��B��B�B�B�B�B�mB�HB�B��B��B��B��BÖB�jB�LB�'B��B��B��B��B��B��B��B�{B�uB�hB�\B�JB�=B�%B�B~�B|�By�Bx�Bx�Bx�By�B�%B�bB��B��B�{B�{B�hB�\B�PB�1B�%B�B�B�B�B�B�1B�+B�B�B�B~�B}�B� B�B�B�B�B�B�B�B�B�B�B�B|�Bt�Bs�Bq�Bo�Bk�BbNB`BBZBQ�BQ�BQ�BQ�BS�BR�BS�BS�BT�BVBXBYBZB\)B]/B]/B]/B]/B^5B`BBbNBe`BjBjBjBjBjBjBk�Bl�Bm�Bn�Bo�Bp�Bq�Bq�B~�B�B�B�B�B�B�B|�Bw�Bt�Bs�B|�B~�B�B�%B�B|�By�Bv�Bs�Bt�Bz�B{�B{�Bz�B�B�B�PB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�-B�-B�3B�FB�XB�^B�^B�jB�wB��BĜB��B��B��B�B�
B�B�#B�)B�;B�ZB�B�B��B	B	
=B	bB	{B	�B	�B	�B	�B	#�B	/B	@�B	C�B	E�B	H�B	H�B	H�B	L�B	N�B	Q�B	T�B	VB	XB	ZB	]/B	bNB	ffB	m�B	t�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�%B	�1B	�7B	�7B	�1B	�+B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	� B	� B	�B	�B	�VB	�hB	�hB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�XB	�qB	��B	��B	B	B	ÖB	ÖB	ÖB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
!H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1B1B1B1B	7B	7B�B�B<jBs�B��B�-B�jBBŢBƨBǮBɺB��B�B�B�NB�BBuB�B�B!�B6FBF�B[#BcTB`BB[#BZBR�BP�BP�BP�BP�BP�BO�BO�BO�BO�BN�BS�BdZBs�By�Bz�By�Bx�Bw�Br�Bm�BiyBe`BdZB^5BZBW
BJ�BC�B?}B7LB0!B�BB�B�B�`B�HB�#B��BÖB�!B�bBt�BbNBW
BG�B0!BB
�B
�sB
�B
��B
ÖB
�-B
��B
�=B
w�B
�1B
��B
�PB
�%B
}�B
jB
Q�B
<jB
)�B
�B

=B	��B	�B	�B	�B	�`B	�5B	��B	�^B	��B	�uB	�VB	�B	x�B	s�B	p�B	k�B	dZB	]/B	S�B	L�B	E�B	>wB	1'B	&�B	�B	oB		7B	B	B��B��B��B�B�B�B�B�mB�HB�B��B��B��B��BÖB�jB�LB�'B��B��B��B��B��B��B��B�{B�uB�hB�\B�JB�=B�%B�B~�B|�By�Bx�Bx�Bx�By�B�%B�bB��B��B�{B�{B�hB�\B�PB�1B�%B�B�B�B�B�B�1B�+B�B�B�B~�B}�B� B�B�B�B�B�B�B�B�B�B�B�B|�Bt�Bs�Bq�Bo�Bk�BbNB`BBZBQ�BQ�BQ�BQ�BS�BR�BS�BS�BT�BVBXBYBZB\)B]/B]/B]/B]/B^5B`BBbNBe`BjBjBjBjBjBjBk�Bl�Bm�Bn�Bo�Bp�Bq�Bq�B~�B�B�B�B�B�B�B|�Bw�Bt�Bs�B|�B~�B�B�%B�B|�By�Bv�Bs�Bt�Bz�B{�B{�Bz�B�B�B�PB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�'B�-B�-B�3B�FB�XB�^B�^B�jB�wB��BĜB��B��B��B�B�
B�B�#B�)B�;B�ZB�B�B��B	B	
=B	bB	{B	�B	�B	�B	�B	#�B	/B	@�B	C�B	E�B	H�B	H�B	H�B	L�B	N�B	Q�B	T�B	VB	XB	ZB	]/B	bNB	ffB	m�B	t�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�%B	�1B	�7B	�7B	�1B	�+B	�B	�B	�B	�B	�B	� B	�B	�B	�B	�B	� B	� B	�B	�B	�VB	�hB	�hB	�hB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�XB	�qB	��B	��B	B	B	ÖB	ÖB	ÖB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�/B	�5B	�BB	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
!H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.34 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140840                              AO  ARCAADJP                                                                    20181024140840    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140840  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140840  QCF$                G�O�G�O�G�O�0               