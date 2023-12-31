CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:11Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190611  20181005190611  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�i���1   @�jO�@@1��G�{�c��vȴ91   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD�fD  D� D   D y�D!  D!�fD"  D"� D#  D#�fD$  D$� D$��D%y�D%��D&y�D'  D'�fD(fD(� D)  D)y�D*  D*� D+  D+� D,fD,� D,��D-� D.  D.� D.��D/� D0  D0y�D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D5��D6� D7  D7� D8  D8�fD9  D9� D9��D:y�D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DA��DB� DC  DC�fDD  DD� DEfDE� DF  DF� DF��DG� DH  DH� DH��DIy�DJ  DJ� DK  DK� DK��DL� DMfDM� DN  DN� DN��DOy�DO��DPy�DP��DQy�DRfDR�fDS  DSy�DT  DT� DT��DUy�DV  DV�fDW  DWy�DX  DX�fDY  DYy�DY��DZ� D[  D[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc�fDd  Dd� De  De�fDffDf�fDgfDg� Dh  Dh� Di  Diy�Di��Djy�Dj��Dky�Dk��Dl� Dl��Dmy�Dn  Dn� Do  Doy�Dp  Dp� Dq  Dq� Dr  Dry�Dr��Dsy�Ds��Dt� DufDu� Dv  Dv� Dv��Dws3Dy{�D�I�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
Bp�B�
B�
B �
B(�
B0�
B8�
B@�
BH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C)C5�C5�C)C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<O]C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�CfO]Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|)C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C�C��C��C��C��C��C��C��C��C��C��C�'�C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C�'�C��C��C��C��C��C�C�C�C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C�'�C��C��D qD �qDqD�qDqD�qDqD�DD�DqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�DD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�DD�DD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDD�qD�D��DqD�qD qD �D!qD!��D"qD"�qD#qD#��D$qD$�qD%D%�D&D&�D'qD'��D(�D(�qD)qD)�D*qD*�qD+qD+�qD,�D,�qD-D-�qD.qD.�qD/D/�qD0qD0�D1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�D6D6�qD7qD7�qD8qD8��D9qD9�qD:D:�D;D;�D<qD<�qD=qD=�qD>qD>�qD?qD?�D@qD@�qDAqDA�qDBDB�qDCqDC��DDqDD�qDE�DE�qDFqDF�qDGDG�qDHqDH�qDIDI�DJqDJ�qDKqDK�qDLDL�qDM�DM�qDNqDN�qDODO�DPDP�DQDQ�DR�DR��DSqDS�DTqDT�qDUDU�DVqDV��DWqDW�DXqDX��DYqDY�DZDZ�qD[qD[�qD\D\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaDa�qDbqDb�qDcqDc��DdqDd�qDeqDe��Df�Df��Dg�Dg�qDhqDh�qDiqDi�DjDj�DkDk�DlDl�qDmDm�DnqDn�qDoqDo�DpqDp�qDqqDq�qDrqDr�DsDs�DtDt�qDu�Du�qDvqDv�qDwDw��Dy��D�P�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˓uA˕�A˟�Aˣ�A˥�AˮAˮAˬA˩�A˩�A˩�Aˣ�A˩�A˧�Aˣ�Aˉ7A�C�A�"�A��A��Aʧ�A�=qA��
A���A�A˴9A˗�A�`BA�E�A�A�I�A�1A��;Aɺ^A�A��`A�x�A���AĶFA�\)A�;dA�{A���Aß�A�|�A��A´9A�p�A��A���A���A�VA��A�ƨA�S�A���A�^5A�$�A��9A�Q�A�  A��jA��A��uA�-A��A�Q�A��wA�z�A�5?A�K�A��
A���A���A���A�hsA�G�A���A��FA���A��A���A�33A�C�A�XA��-A�oA�A�|�A��A��TA��wA�O�A��#A�ĜA�-A�t�A���A���A��`A�ĜA���A�ĜA�A�A��yA� �A��uA�`BA��jA��A�jA�-A}O�Awp�As`BAo�Ak�AfbAc��AbJA]�AY33AW��AT~�AQ�AOx�AL��ALQ�AK&�AJVAI�AHVAG��AF��AEt�AC��AA�7A?��A=��A;XA8��A6bNA4ZA2��A1�TA0(�A/��A.bNA,�A+dZA*�+A)K�A'��A&n�A&(�A&  A%�hA$��A$E�A#��A#A"��A"Q�A!�A�7A��A��A��A�uA^5AffAjA$�AA�7A`BA��A��A�mA�wA��AĜA(�A��A"�A�/A�A�DAbNA�TA��AXA�A�HA�\A1'AƨAp�A
��A
��A
(�A	�AM�A  A��A��A��AbNA?}A^5A��A��A�A�mA�A ��@���A A�A   @�Ĝ@�  @�;d@�ȴ@�b@�v�@��@�@�{@��D@�33@�@��H@���@�V@�?}@�X@�?}@�/@웦@�z�@�M�@���@@�5?@�hs@��
@�!@�-@��@��/@��@�M�@�@�Q�@�S�@�V@�@��@�h@�o@���@��/@��@�9X@��@�|�@�ȴ@��T@���@�\)@��y@�ff@�{@݁@܋D@��
@ۅ@�\)@�;d@��y@ڰ!@�ff@��@ّh@�&�@؃@�1@ץ�@�K�@��y@�^5@���@�x�@��@ԓu@Ӯ@��@��@��#@Ѳ-@щ7@с@�`B@��@� �@���@�5?@�{@��T@͑h@�X@�O�@�`B@�7L@̋D@�9X@�I�@�Z@�(�@�|�@�
=@��@���@�~�@ɡ�@��/@�Ĝ@�Ĝ@���@���@�r�@���@�t�@�@ư!@�v�@�ff@�=q@���@�@ź^@�7L@�ƨ@�C�@��y@§�@�M�@��T@��@��@�  @�ƨ@�t�@�
=@�~�@�-@�p�@�1'@�+@��R@�v�@�~�@�M�@�-@���@�p�@��`@��@��@���@��+@�M�@�{@�@�@�G�@��@��@��u@�Q�@�9X@�  @��F@�S�@�;d@�"�@��H@�5?@��#@���@�hs@�7L@��`@���@��@��u@��D@�(�@���@��@�|�@�t�@�t�@�\)@�C�@�33@�+@�
=@��@��@���@��@�G�@���@�z�@��F@�|�@�C�@�
=@�@��@���@�ff@���@�7L@���@�bN@� �@��m@���@�t�@�\)@�;d@���@�ff@�$�@���@��-@��7@�p�@�X@�V@��u@�  @�;d@��R@���@��+@�~�@�~�@�~�@���@���@���@��@�z�@�C�@��\@�v�@�5?@��@�{@���@��^@��h@�p�@�&�@��`@��u@��m@���@��P@�t�@�dZ@�"�@��\@�{@�G�@��@�j@�9X@��@��m@�ƨ@���@�|�@�dZ@�;d@��@�@���@�v�@�J@��@�C@��@|q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A˓uA˕�A˟�Aˣ�A˥�AˮAˮAˬA˩�A˩�A˩�Aˣ�A˩�A˧�Aˣ�Aˉ7A�C�A�"�A��A��Aʧ�A�=qA��
A���A�A˴9A˗�A�`BA�E�A�A�I�A�1A��;Aɺ^A�A��`A�x�A���AĶFA�\)A�;dA�{A���Aß�A�|�A��A´9A�p�A��A���A���A�VA��A�ƨA�S�A���A�^5A�$�A��9A�Q�A�  A��jA��A��uA�-A��A�Q�A��wA�z�A�5?A�K�A��
A���A���A���A�hsA�G�A���A��FA���A��A���A�33A�C�A�XA��-A�oA�A�|�A��A��TA��wA�O�A��#A�ĜA�-A�t�A���A���A��`A�ĜA���A�ĜA�A�A��yA� �A��uA�`BA��jA��A�jA�-A}O�Awp�As`BAo�Ak�AfbAc��AbJA]�AY33AW��AT~�AQ�AOx�AL��ALQ�AK&�AJVAI�AHVAG��AF��AEt�AC��AA�7A?��A=��A;XA8��A6bNA4ZA2��A1�TA0(�A/��A.bNA,�A+dZA*�+A)K�A'��A&n�A&(�A&  A%�hA$��A$E�A#��A#A"��A"Q�A!�A�7A��A��A��A�uA^5AffAjA$�AA�7A`BA��A��A�mA�wA��AĜA(�A��A"�A�/A�A�DAbNA�TA��AXA�A�HA�\A1'AƨAp�A
��A
��A
(�A	�AM�A  A��A��A��AbNA?}A^5A��A��A�A�mA�A ��@���A A�A   @�Ĝ@�  @�;d@�ȴ@�b@�v�@��@�@�{@��D@�33@�@��H@���@�V@�?}@�X@�?}@�/@웦@�z�@�M�@���@@�5?@�hs@��
@�!@�-@��@��/@��@�M�@�@�Q�@�S�@�V@�@��@�h@�o@���@��/@��@�9X@��@�|�@�ȴ@��T@���@�\)@��y@�ff@�{@݁@܋D@��
@ۅ@�\)@�;d@��y@ڰ!@�ff@��@ّh@�&�@؃@�1@ץ�@�K�@��y@�^5@���@�x�@��@ԓu@Ӯ@��@��@��#@Ѳ-@щ7@с@�`B@��@� �@���@�5?@�{@��T@͑h@�X@�O�@�`B@�7L@̋D@�9X@�I�@�Z@�(�@�|�@�
=@��@���@�~�@ɡ�@��/@�Ĝ@�Ĝ@���@���@�r�@���@�t�@�@ư!@�v�@�ff@�=q@���@�@ź^@�7L@�ƨ@�C�@��y@§�@�M�@��T@��@��@�  @�ƨ@�t�@�
=@�~�@�-@�p�@�1'@�+@��R@�v�@�~�@�M�@�-@���@�p�@��`@��@��@���@��+@�M�@�{@�@�@�G�@��@��@��u@�Q�@�9X@�  @��F@�S�@�;d@�"�@��H@�5?@��#@���@�hs@�7L@��`@���@��@��u@��D@�(�@���@��@�|�@�t�@�t�@�\)@�C�@�33@�+@�
=@��@��@���@��@�G�@���@�z�@��F@�|�@�C�@�
=@�@��@���@�ff@���@�7L@���@�bN@� �@��m@���@�t�@�\)@�;d@���@�ff@�$�@���@��-@��7@�p�@�X@�V@��u@�  @�;d@��R@���@��+@�~�@�~�@�~�@���@���@���@��@�z�@�C�@��\@�v�@�5?@��@�{@���@��^@��h@�p�@�&�@��`@��u@��m@���@��P@�t�@�dZ@�"�@��\@�{@�G�@��@�j@�9X@��@��m@�ƨ@���@�|�@�dZ@�;d@��@�@���@�v�@�J@��@�C@��@|q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	0!B	0!B	/B	/B	0!B	/B	/B	/B	/B	/B	.B	-B	.B	-B	-B	)�B	"�B	$�B	,B	N�B	ZB	��B	�TB	��B

=B
�B
2-B
8RB
N�B
� B
��B
�B
��B
�B
��BBPB2-BdZBx�B|�B� B�B�B�PB��B��B��B��B��B��B�bB�hB��B��B�3B��B��B�B��B��B��B��B	7B�B�B�B�B�B�B#�B#�B#�B8RB5?BC�BG�BC�B=qB:^B9XB8RB1'B#�BoB%B��B�B�;BŢB��B�7By�BjBW
BF�B;dB-B�BB
�BB
�B
��B
ǮB
�'B
�JB
o�B
C�B
 �B
oB
JB
B	�B	�^B	�VB	l�B	VB	49B	$�B	�B	B�B�sB�)B��BȴB�}B�qB�dB�XB�LB�?B�3B�'B�B�B��B��B��B��B��B�B�B�B�B�!B�B�B�'BĜB��B��B��B��B��B��B��B�B��B��B��B��B�B�B��B��B��B��B��B�B�#B�;B�ZB�B��B��B	%B	
=B	
=B	�B	�B	�B	�B	{B	oB	hB	hB	bB	\B	PB	
=B	
=B	DB	VB	{B	�B	�B	(�B	2-B	7LB	:^B	7LB	0!B	.B	49B	7LB	.B	�B	{B	�B	"�B	)�B	)�B	'�B	%�B	0!B	2-B	>wB	<jB	0!B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	'�B	'�B	(�B	'�B	)�B	1'B	6FB	7LB	8RB	9XB	>wB	ZB	gmB	jB	l�B	l�B	o�B	r�B	s�B	u�B	y�B	z�B	y�B	x�B	v�B	s�B	r�B	s�B	t�B	v�B	�B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	B	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�)B	�)B	�)B	�/B	�;B	�BB	�HB	�TB	�TB	�TB	�NB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
  B	��B	��B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
�B
#n22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	0!B	0!B	/B	/B	0!B	/B	/B	/B	/B	/B	.B	-B	.B	-B	-B	)�B	"�B	$�B	,B	N�B	ZB	��B	�TB	��B

=B
�B
2-B
8RB
N�B
� B
��B
�B
��B
�B
��BBPB2-BdZBx�B|�B� B�B�B�PB��B��B��B��B��B��B�bB�hB��B��B�3B��B��B�B��B��B��B��B	7B�B�B�B�B�B�B#�B#�B#�B8RB5?BC�BG�BC�B=qB:^B9XB8RB1'B#�BoB%B��B�B�;BŢB��B�7By�BjBW
BF�B;dB-B�BB
�BB
�B
��B
ǮB
�'B
�JB
o�B
C�B
 �B
oB
JB
B	�B	�^B	�VB	l�B	VB	49B	$�B	�B	B�B�sB�)B��BȴB�}B�qB�dB�XB�LB�?B�3B�'B�B�B��B��B��B��B��B�B�B�B�B�!B�B�B�'BĜB��B��B��B��B��B��B��B�B��B��B��B��B�B�B��B��B��B��B��B�B�#B�;B�ZB�B��B��B	%B	
=B	
=B	�B	�B	�B	�B	{B	oB	hB	hB	bB	\B	PB	
=B	
=B	DB	VB	{B	�B	�B	(�B	2-B	7LB	:^B	7LB	0!B	.B	49B	7LB	.B	�B	{B	�B	"�B	)�B	)�B	'�B	%�B	0!B	2-B	>wB	<jB	0!B	�B	�B	�B	�B	�B	�B	�B	"�B	'�B	'�B	'�B	(�B	'�B	)�B	1'B	6FB	7LB	8RB	9XB	>wB	ZB	gmB	jB	l�B	l�B	o�B	r�B	s�B	u�B	y�B	z�B	y�B	x�B	v�B	s�B	r�B	s�B	t�B	v�B	�B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	B	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�)B	�)B	�)B	�/B	�;B	�BB	�HB	�TB	�TB	�TB	�NB	�`B	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
  B	��B	��B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
�B
#n22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190611                              AO  ARCAADJP                                                                    20181005190611    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190611  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190611  QCF$                G�O�G�O�G�O�8000            