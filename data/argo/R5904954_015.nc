CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:51Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181005191651  20181005191651  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @ר����1   @ר�@yo�@4E�����c����F1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @@  @�  @���A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C�fC   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ�CL  CM�fCO�fCR  CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�fC�  C��C�  C�  C��C�  C��C��3C�  C��C��C��C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��C��C��3C��3C�  C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C��C�  C��C��C�  C�  C��C�  C�  C��C��C��3C��3C��3C�  C�  C�  C�  C��C��C��3C��C��C��C�  C�  C�  C��fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C��3C��3C��C��C�  C�  C��C�  D	� D
fD
� D  D� D��Dy�D��D�fDfD� DfD�fD  D� D  D� DfD� D��Ds3D  D� D  D�fD  D� D  D� DfD� D��D� DfD�fD  Dy�D  Dy�D��Dy�D�3Dy�D��D� D+y�D+��D,� D-  D-y�D-��D.y�D/  D/�fD0  D0y�D0��D1� D2  D2y�D2��D3y�D3��D4� D5fD5y�D6  D6� D7  D7� D8fD8y�D9  D9� D:  D:� D;  D;�fD<  D<y�D<��D=y�D>  D>� D>��D?y�D?��D@y�DA  DA�fDBfDB�fDC  DC� DD  DDy�DE  DE� DF  DF� DG  DG�fDH  DHy�DH��DIy�DI��DJy�DK  DK� DK��DLy�DM  DM�fDN  DN� DO  DO� DPfDP�fDQ�DQ�fDRfDR�fDSfDS�fDT  DT�fDU  DU� DVfDV� DW  DW� DX  DX� DX��DY� DZ  DZy�DZ��D[y�D[�3D\y�D\��D]y�D]��D^� D^��D_� D`fD`�fDafDa�fDb  Db� DcfDc� Dd  Dd�fDefDe�fDf  Df� Df��Dg� Dh  Dhy�Dh��Di� Dj  Dj� Dk  Dky�Dl  Dl�fDm  Dm� Dn  Dny�Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt�fDu  Duy�Du�3Dvy�Dv��Dwy�Dw�3Dy�D�P�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Mp�@��R@ÅA\)A#\)AC\)Ac\)A��A�z�A��A��A��AѮA�A�B �
B�
B�
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
B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�8RB�8RB�8RB�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B잸B���B�8RB�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�CO]C5�C5�C5�C)C 5�C"5�C$5�C&5�C(5�C*5�C,O]C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJO]CL5�CN)CP)CR5�CTO]CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~)C�C��C�'�C��C��C�'�C��C�'�C�C��C�'�C�'�C�'�C��C�C��C�'�C��C��C��C��C��C��C�'�C�'�C��C�'�C��C��C��C�C�C��C��C��C��C��C��C��C�C�'�C�4{C�C�C��C�'�C�'�C��C�C�C��C��C��C��C��C��C��C��C�C�'�C��C��C��C��C�C�C��C��C��C�4{C��C��C��C�'�C��C�'�C�'�C��C��C�'�C��C��C�'�C�'�C�C�C�C��C��C��C��C�'�C�'�C�C�'�C�'�C�4{C��C��C��C�GC��C��C��C��C��C��C��C��C��C��C�C�'�C��C��C��C�C�C�'�C�'�C��C��C�'�C��D	�qD
�D
�qDqD�qDD�DD��D�D�qD�D��DqD�qDqD�qD�D�qDD��DqD�qDqD��DqD�qDqD�qD�D�qDD�qD�D��DqD�DqD�DD�D �D�DD�qD+�D,D,�qD-qD-�D.D.�D/qD/��D0qD0�D1D1�qD2qD2�D3D3�D4D4�qD5�D5�D6qD6�qD7qD7�qD8�D8�D9qD9�qD:qD:�qD;qD;��D<qD<�D=D=�D>qD>�qD?D?�D@D@�DAqDA��DB�DB��DCqDC�qDDqDD�DEqDE�qDFqDF�qDGqDG��DHqDH�DIDI�DJDJ�DKqDK�qDLDL�DMqDM��DNqDN�qDOqDO�qDP�DP��DQ>DQ��DR�DR��DS�DS��DTqDT��DUqDU�qDV�DV�qDWqDW�qDXqDX�qDYDY�qDZqDZ�D[D[�D\ �D\�D]D]�D^D^�qD_D_�qD`�D`��Da�Da��DbqDb�qDc�Dc�qDdqDd��De�De��DfqDf�qDgDg�qDhqDh�DiDi�qDjqDj�qDkqDk�DlqDl��DmqDm�qDnqDn�DoqDo�qDpqDp�DqqDq�qDrqDr�qDsqDs��DtqDt��DuqDu�Dv �Dv�DwDw�Dw�Dy��D�W\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ḁ�Ḁ�Ḁ�Ạ�Ạ�Ḁ�Ḁ�A̩�A̰!A̾wA�ĜA���A��
A��
A�ƨA�ĜA���A���A�  A�"�A�+A�/A�5?A�5?A�33A�5?A�9XA�I�A�K�A�S�A�^5A�dZA�ffA�dZA�dZA�bNA�ffA�l�A�p�A�r�A�p�A�n�A�n�A�dZA�=qA��A���A�z�A�VA�ȴAɥ�Aə�Aɇ+A�n�A�ZA�1AȶFA�+A�ffAơ�A��A�l�A��/A�"�A�l�A�1'A��
A��-A��A�-A��wA� �A��TA�ffA�\)A�"�A�|�A�A��/A���A��A���A�$�A�^5A��A�
=A��A�l�A�A��DA��PA�A���A���A���A���A�n�A��^A�M�A�p�A�7LA�%A�S�A�?}A��RA��hA�  A�VA�M�A��PA��
A��A�hsA���A�5?A�|�A�9XA�bA�(�A��AoA}��A{�-Ax�+At�Ap-AmO�Akx�AjVAi+Ah�+Ag|�Afv�Af{Ae��Ad�jAc33AaoA^�jAZ�RAYl�AX�DAW&�AT�RAS�AR�AO��AM&�AK%AIAG/AE�7AAK�A?33A>Q�A>5?A>$�A=A:��A8(�A5�PA2JA0^5A/�A.�uA-�#A,Q�A*��A)�^A(�A(M�A'�A%��A$$�A"�+A ��A M�A (�A�
A"�A�mA�`A��AG�A��AQ�A�mAt�A�A�AJA�!A{A�hA�AĜAAx�A�A�9A(�AK�A�DAJA�HA
bNA	�#A	\)AZA�AG�A/A��An�A$�A�HA�A{A�`A�-A ��A -A 1'A �@���@�33@��
@�O�@�K�@�%@�@��@��/@�1'@��H@��`@��@�|�@�&�@��@�~�@��#@���@��@�S�@���@�/@�I�@޸R@ݡ�@�bN@���@�|�@�o@�-@���@�Z@�(�@���@�;d@�=q@���@�p�@���@��;@��@��P@�S�@���@���@�v�@�v�@�-@���@�%@��@�I�@��@��@���@���@��P@��P@�|�@�\)@��h@���@�A�@��P@��!@�V@��@�hs@��@���@��/@�t�@�l�@�o@���@�M�@���@�p�@�O�@�/@�?}@�O�@�hs@�x�@���@���@��@�(�@�b@���@��w@��P@��P@�t�@�\)@�o@���@�~�@�^5@�$�@�J@���@�hs@�`B@�O�@�?}@�j@��@�  @��m@���@��@���@��P@�\)@�C�@��!@�{@�x�@�O�@�V@��j@��D@�Z@�(�@���@�ƨ@��F@���@�|�@�\)@�K�@��@���@�~�@�$�@�{@���@���@��@�O�@��@���@�Q�@���@�+@���@���@�^5@�^5@��#@��@���@��;@�l�@�K�@��R@�5?@���@�X@��@���@��9@�I�@�b@���@��
@���@��@��@���@�K�@��@��H@���@���@��+@�M�@�=q@�$�@��^@��h@�`B@��@��D@���@���@��@�t�@�\)@�K�@�;d@��@�@�;d@�ȴ@�V@���@��@��#@���@��@��@�A�@���@���@�dZ@�;d@�"�@���@�E�@���@�@��^@�@��^@��^@��h@�O�@���@�Ĝ@���@�z�@�r�@�Z@��@yzx11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ḁ�Ḁ�Ḁ�Ạ�Ạ�Ḁ�Ḁ�A̩�A̰!A̾wA�ĜA���A��
A��
A�ƨA�ĜA���A���A�  A�"�A�+A�/A�5?A�5?A�33A�5?A�9XA�I�A�K�A�S�A�^5A�dZA�ffA�dZA�dZA�bNA�ffA�l�A�p�A�r�A�p�A�n�A�n�A�dZA�=qA��A���A�z�A�VA�ȴAɥ�Aə�Aɇ+A�n�A�ZA�1AȶFA�+A�ffAơ�A��A�l�A��/A�"�A�l�A�1'A��
A��-A��A�-A��wA� �A��TA�ffA�\)A�"�A�|�A�A��/A���A��A���A�$�A�^5A��A�
=A��A�l�A�A��DA��PA�A���A���A���A���A�n�A��^A�M�A�p�A�7LA�%A�S�A�?}A��RA��hA�  A�VA�M�A��PA��
A��A�hsA���A�5?A�|�A�9XA�bA�(�A��AoA}��A{�-Ax�+At�Ap-AmO�Akx�AjVAi+Ah�+Ag|�Afv�Af{Ae��Ad�jAc33AaoA^�jAZ�RAYl�AX�DAW&�AT�RAS�AR�AO��AM&�AK%AIAG/AE�7AAK�A?33A>Q�A>5?A>$�A=A:��A8(�A5�PA2JA0^5A/�A.�uA-�#A,Q�A*��A)�^A(�A(M�A'�A%��A$$�A"�+A ��A M�A (�A�
A"�A�mA�`A��AG�A��AQ�A�mAt�A�A�AJA�!A{A�hA�AĜAAx�A�A�9A(�AK�A�DAJA�HA
bNA	�#A	\)AZA�AG�A/A��An�A$�A�HA�A{A�`A�-A ��A -A 1'A �@���@�33@��
@�O�@�K�@�%@�@��@��/@�1'@��H@��`@��@�|�@�&�@��@�~�@��#@���@��@�S�@���@�/@�I�@޸R@ݡ�@�bN@���@�|�@�o@�-@���@�Z@�(�@���@�;d@�=q@���@�p�@���@��;@��@��P@�S�@���@���@�v�@�v�@�-@���@�%@��@�I�@��@��@���@���@��P@��P@�|�@�\)@��h@���@�A�@��P@��!@�V@��@�hs@��@���@��/@�t�@�l�@�o@���@�M�@���@�p�@�O�@�/@�?}@�O�@�hs@�x�@���@���@��@�(�@�b@���@��w@��P@��P@�t�@�\)@�o@���@�~�@�^5@�$�@�J@���@�hs@�`B@�O�@�?}@�j@��@�  @��m@���@��@���@��P@�\)@�C�@��!@�{@�x�@�O�@�V@��j@��D@�Z@�(�@���@�ƨ@��F@���@�|�@�\)@�K�@��@���@�~�@�$�@�{@���@���@��@�O�@��@���@�Q�@���@�+@���@���@�^5@�^5@��#@��@���@��;@�l�@�K�@��R@�5?@���@�X@��@���@��9@�I�@�b@���@��
@���@��@��@���@�K�@��@��H@���@���@��+@�M�@�=q@�$�@��^@��h@�`B@��@��D@���@���@��@�t�@�\)@�K�@�;d@��@�@�;d@�ȴ@�V@���@��@��#@���@��@��@�A�@���@���@�dZ@�;d@�"�@���@�E�@���@�@��^@�@��^@��^@��h@�O�@���@�Ĝ@���@�z�@�r�@�Z@��@yzx11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
(�B
(�B
(�B
'�B
(�B
'�B
(�B
(�B
(�B
,B
-B
.B
0!B
0!B
,B
,B
/B
0!B
>wB
I�B
L�B
P�B
S�B
S�B
T�B
VB
XB
\)B
]/B
`BB
cTB
e`B
gmB
hsB
iyB
k�B
m�B
o�B
o�B
p�B
q�B
r�B
s�B
y�B
�B
��B
��B
��B
�wB
�HB
�B
��B
��BB1B{B�B5?BT�BdZB{�BĜB��B"�BK�Bk�Bp�Bp�B� B�B� Bm�BjBn�Bu�Bz�B{�Bq�BhsBQ�BG�B@�B:^B0!B"�B �B#�B$�B"�B�B�B�BuBJBB�HB��BȴBB�RB�3B�B��B�JBm�BL�B)�BJB
�B
��B
ƨB
�wB
�B
��B
��B
��B
�hB
|�B
ZB
E�B
A�B
:^B
-B
{B	��B	��B	�qB	�B	��B	��B	��B	�bB	�+B	�B	~�B	v�B	m�B	`BB	R�B	<jB	33B	,B	$�B	!�B	�B	�B	DB	B��B�B�sB�5B��BǮBÖB��B�}B�^B�'B��B�B��B��B��B��B��B��B�uB�\B�=B�1B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�+B�+B�1B�7B�7B�1B�1B�+B�+B�B�B�B�B�%B�JB�hB�bB�bB�hB�oB�hB��B��B��B��B��B��B��B��B��B��B�B�?B�'B�B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
VB�fB�B�B�B��B��B��B��B��B��B	B	B		7B	PB	\B	\B	oB	uB	�B	�B	�B	�B	 �B	#�B	$�B	&�B	)�B	,B	.B	1'B	2-B	2-B	2-B	1'B	33B	33B	33B	2-B	49B	8RB	<jB	@�B	E�B	I�B<~�B	� B	�B	�B	�1B	�=B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�?B	�?B	�FB	�FB	�RB	�jB	�}B	�}B	�}B	��B	B	ĜB	ŢB	ǮB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�HB	�TB	�ZB	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
%B
%B
+B
+B
1B
	B
C22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
(�B
(�B
(�B
'�B
(�B
'�B
(�B
(�B
(�B
,B
-B
.B
0!B
0!B
,B
,B
/B
0!B
>wB
I�B
L�B
P�B
S�B
S�B
T�B
VB
XB
\)B
]/B
`BB
cTB
e`B
gmB
hsB
iyB
k�B
m�B
o�B
o�B
p�B
q�B
r�B
s�B
y�B
�B
��B
��B
��B
�wB
�HB
�B
��B
��BB1B{B�B5?BT�BdZB{�BĜB��B"�BK�Bk�Bp�Bp�B� B�B� Bm�BjBn�Bu�Bz�B{�Bq�BhsBQ�BG�B@�B:^B0!B"�B �B#�B$�B"�B�B�B�BuBJBB�HB��BȴBB�RB�3B�B��B�JBm�BL�B)�BJB
�B
��B
ƨB
�wB
�B
��B
��B
��B
�hB
|�B
ZB
E�B
A�B
:^B
-B
{B	��B	��B	�qB	�B	��B	��B	��B	�bB	�+B	�B	~�B	v�B	m�B	`BB	R�B	<jB	33B	,B	$�B	!�B	�B	�B	DB	B��B�B�sB�5B��BǮBÖB��B�}B�^B�'B��B�B��B��B��B��B��B��B�uB�\B�=B�1B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�%B�+B�+B�1B�7B�7B�1B�1B�+B�+B�B�B�B�B�%B�JB�hB�bB�bB�hB�oB�hB��B��B��B��B��B��B��B��B��B��B�B�?B�'B�B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B
VB�fB�B�B�B��B��B��B��B��B��B	B	B		7B	PB	\B	\B	oB	uB	�B	�B	�B	�B	 �B	#�B	$�B	&�B	)�B	,B	.B	1'B	2-B	2-B	2-B	1'B	33B	33B	33B	2-B	49B	8RB	<jB	@�B	E�B	I�B<~�B	� B	�B	�B	�1B	�=B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�3B	�3B	�?B	�?B	�FB	�FB	�RB	�jB	�}B	�}B	�}B	��B	B	ĜB	ŢB	ǮB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�;B	�;B	�5B	�5B	�BB	�HB	�HB	�HB	�TB	�ZB	�`B	�fB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
%B
%B
+B
+B
1B
	B
C22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191651                              AO  ARCAADJP                                                                    20181005191651    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191651  QCP$                G�O�G�O�G�O�4F03E           AO  ARGQQCPL                                                                    20181005191651  QCF$                G�O�G�O�G�O�8000            