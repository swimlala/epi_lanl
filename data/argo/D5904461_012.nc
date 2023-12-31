CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:31Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221331  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_012                   2C  D   APEX                            6531                            072314                          846 @� ؙ� 1   @� �33?�@1i��l�D�c���"��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�C3D�s3D�ٚD�fD�FfD��fD��3D�3D�L�D�s3Dǣ3D�3D�@ D�l�D�� D��fD�<�D�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
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
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�8RB�k�B�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�8RB�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:��D;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�Dy�qD�#�D�I�D�y�D��RD�D�MD��D���D��D�S�D�y�Dǩ�D�	�D�F�D�s�D�ָD��D�C�D�s�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��A���A���A���A�  A���A���A���A���A�%A�A�A�  A���A���A��A��A��yA��mA��yA���Aؗ�A�A�  A�E�A�n�A���Aś�AļjAøRA��
A¡�A�v�A�G�A�$�A��A�ȴA��hA��RA��#A�^5A�1A��9A�VA�5?A���A���A�z�A�&�A��DA��
A�
=A�`BA��FA�&�A�C�A�jA�O�A�`BA�A�"�A��A�\)A��A���A��\A���A��HA�A� �A�;dA�5?A�p�A�  A��TA�VA�C�A�S�A}�Aw�^An�9Ah^5AcVA`��A_t�AZ��AZJAY�TAY��AX�/AV^5AR�+APz�AO�AK7LAG+AF�AE�FAD�RAA�A>��A=�#A=VA;�A;/A:A7&�A6 �A3��A1�
A/A-��A-O�A-/A-oA,$�A*�jA(�yA'33A&{A$��A#��A#x�A#O�A#A"�`A"~�A!�A��A��A�uAA|�AĜA�Ax�AS�A�A�jAhsAv�A �A�-A~�A�mAVA-AƨA/A�+A��A�HAVA�A�A\)AoA �A�^A��A�A+AI�A|�A
�uA	�wA	��A��A��AS�A�A(�A�A|�A{A�^AXAoAȴA�A��AdZA �!A (�@�\)@�9X@��@�;d@�(�@���@� �@��@��@��@�A�@���@웦@�F@�R@�n�@��H@�z�@��@���@��@�Q�@�S�@�
=@���@��@�@�x�@��@�!@�V@���@�dZ@�Ĝ@ާ�@�~�@�I�@�n�@�p�@؛�@���@���@֟�@�C�@�1@�A�@�  @ו�@��y@�p�@��`@�Q�@Ӯ@���@��@��@� �@Η�@�1@�p�@�I�@�S�@��y@Ƈ+@�=q@�$�@���@ēu@��@�t�@�dZ@�dZ@�@�ȴ@�ȴ@¸R@�@�ff@�@�G�@��u@�(�@��@���@��h@�%@��@���@�r�@�I�@�K�@���@�^5@�=q@�$�@��-@�X@�7L@���@�r�@�9X@��@��@�r�@�b@�"�@��R@�V@��@��@�%@���@�A�@�1@�t�@�33@�"�@�"�@�
=@��H@���@���@�V@�-@��@�@���@��^@���@��7@�x�@�p�@�&�@��@��@�Ĝ@��9@���@��u@��u@��u@�Q�@�b@�1@���@�;d@���@���@���@��!@���@�v�@���@���@���@��@�r�@�bN@�A�@��@��F@�t�@�dZ@�"�@��@��R@�^5@�J@��@�@��@��@�Z@�9X@� �@���@��@��
@���@��@��F@�\)@���@�{@��@���@��^@�hs@���@�z�@�I�@�b@��@�|�@�l�@�+@�^5@��-@�O�@��@���@�j@�(�@��
@��F@��P@���@���@���@�n�@�$�@�@��h@�hs@��@��u@���@�r�@�1@�ƨ@���@��P@�33@��y@�ȴ@�
=@��y@�@���@�@��h@��h@�`B@�?}@��@���@��D@�bN@�I�@��F@�C�@��@���@���@�~�@�V@��@��T@���@��^@���@��7@�hs@�7L@�%@��@���@�r�@�1'@�b@�b@�1@��@��;@��;@���@�l�@�K�@��@���@��+@�5?@�J@��T@���@��7@��@���@���@�9X@��@��
@��F@���@�|�@��@���@�\)@���@�@z-@n��@d(�@Y��@P��@HbN@B�H@<�j@4�D@-/@&��@ ��@9X@�@�F@7L@�/@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��A���A���A���A�  A���A���A���A���A�%A�A�A�  A���A���A��A��A��yA��mA��yA���Aؗ�A�A�  A�E�A�n�A���Aś�AļjAøRA��
A¡�A�v�A�G�A�$�A��A�ȴA��hA��RA��#A�^5A�1A��9A�VA�5?A���A���A�z�A�&�A��DA��
A�
=A�`BA��FA�&�A�C�A�jA�O�A�`BA�A�"�A��A�\)A��A���A��\A���A��HA�A� �A�;dA�5?A�p�A�  A��TA�VA�C�A�S�A}�Aw�^An�9Ah^5AcVA`��A_t�AZ��AZJAY�TAY��AX�/AV^5AR�+APz�AO�AK7LAG+AF�AE�FAD�RAA�A>��A=�#A=VA;�A;/A:A7&�A6 �A3��A1�
A/A-��A-O�A-/A-oA,$�A*�jA(�yA'33A&{A$��A#��A#x�A#O�A#A"�`A"~�A!�A��A��A�uAA|�AĜA�Ax�AS�A�A�jAhsAv�A �A�-A~�A�mAVA-AƨA/A�+A��A�HAVA�A�A\)AoA �A�^A��A�A+AI�A|�A
�uA	�wA	��A��A��AS�A�A(�A�A|�A{A�^AXAoAȴA�A��AdZA �!A (�@�\)@�9X@��@�;d@�(�@���@� �@��@��@��@�A�@���@웦@�F@�R@�n�@��H@�z�@��@���@��@�Q�@�S�@�
=@���@��@�@�x�@��@�!@�V@���@�dZ@�Ĝ@ާ�@�~�@�I�@�n�@�p�@؛�@���@���@֟�@�C�@�1@�A�@�  @ו�@��y@�p�@��`@�Q�@Ӯ@���@��@��@� �@Η�@�1@�p�@�I�@�S�@��y@Ƈ+@�=q@�$�@���@ēu@��@�t�@�dZ@�dZ@�@�ȴ@�ȴ@¸R@�@�ff@�@�G�@��u@�(�@��@���@��h@�%@��@���@�r�@�I�@�K�@���@�^5@�=q@�$�@��-@�X@�7L@���@�r�@�9X@��@��@�r�@�b@�"�@��R@�V@��@��@�%@���@�A�@�1@�t�@�33@�"�@�"�@�
=@��H@���@���@�V@�-@��@�@���@��^@���@��7@�x�@�p�@�&�@��@��@�Ĝ@��9@���@��u@��u@��u@�Q�@�b@�1@���@�;d@���@���@���@��!@���@�v�@���@���@���@��@�r�@�bN@�A�@��@��F@�t�@�dZ@�"�@��@��R@�^5@�J@��@�@��@��@�Z@�9X@� �@���@��@��
@���@��@��F@�\)@���@�{@��@���@��^@�hs@���@�z�@�I�@�b@��@�|�@�l�@�+@�^5@��-@�O�@��@���@�j@�(�@��
@��F@��P@���@���@���@�n�@�$�@�@��h@�hs@��@��u@���@�r�@�1@�ƨ@���@��P@�33@��y@�ȴ@�
=@��y@�@���@�@��h@��h@�`B@�?}@��@���@��D@�bN@�I�@��F@�C�@��@���@���@�~�@�V@��@��T@���@��^@���@��7@�hs@�7L@�%@��@���@�r�@�1'@�b@�b@�1@��@��;@��;@���@�l�@�K�@��@���@��+@�5?@�J@��T@���@��7@��@���@���@�9X@��@��
@��F@���@�|�@��G�O�@�\)@���@�@z-@n��@d(�@Y��@P��@HbN@B�H@<�j@4�D@-/@&��@ ��@9X@�@�F@7L@�/@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB`BB`BB`BB`BB`BB`BBaHBaHBaHBaHB`BBZBQ�BJ�BW
BffB��B��B�LB��BÖBĜBĜB��B�B�ZB�ZB�yBDBbB�B#�B#�B.B@�BD�BC�B7LBB��B�sB�)B��B�TB��B��B�9B��B��B��B�7B{�BhsBffB[#B7LB
=B
�B
�sB
�;B
��B
��B
��B
bNB
�B	��B	�BB	ǮB	��B	k�B	F�B	,B	�B	�B	B��B��B��B��B�sB�B��BȴB�jB�9B�-B�!B�B�B�B�B�B�B��B��B�B�B�B�-B�3B�RB�dB�^B�XB�jB�}B�qB�qB�wB��BÖBÖBÖBÖBÖBBÖBŢBĜB��B��B��B��B��B�B�
B�B�)B�;B�yB�B�B�B��B	B	oB	�B	�B	$�B	0!B	?}B	B�B	A�B	=qB	;dB	B�B	B�B	@�B	@�B	?}B	=qB	=qB	<jB	;dB	:^B	8RB	8RB	;dB	;dB	=qB	=qB	:^B	6FB	1'B	0!B	/B	1'B	1'B	.B	,B	,B	1'B	:^B	9XB	0!B	+B	$�B	�B	�B	$�B	.B	%�B	�B	�B	�B	�B	�B	�B	�B	#�B	0!B	D�B	D�B	C�B	A�B	A�B	C�B	C�B	C�B	@�B	>wB	;dB	9XB	9XB	8RB	33B	/B	+B	,B	&�B	#�B	"�B	!�B	"�B	(�B	,B	33B	<jB	B�B	B�B	A�B	B�B	H�B	L�B	N�B	M�B	L�B	I�B	F�B	C�B	?}B	:^B	;dB	=qB	B�B	C�B	E�B	H�B	L�B	O�B	S�B	VB	YB	ZB	[#B	_;B	bNB	cTB	cTB	dZB	e`B	hsB	n�B	s�B	t�B	u�B	y�B	�B	�B	�B	�B	�7B	�JB	�VB	�VB	�\B	�\B	�VB	�VB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�wB	��B	��B	ÖB	ƨB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�HB	�HB	�TB	�NB	�NB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�`B	�mB	�sB	�mB	�fB	�fB	�fB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�mB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
1B
oB
oB
�B
&�B
-B
33B
;dB
A�B
F�B
J�B
P�B
XB
^5B
cTB
gmB
k�B
p�B
r�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B_)B_)B_)B_)B_)B_,B_+B_,B_+B_,B_+B_)B_+B_)B_,B_)B_+B_)B_+B_)B_+B_+B_)B_+B_)B_)B_.B`5B`1B`4B`1B`1B`1B`4Ba:Ba8Ba8Ba5B`2BZBQ�BJ�BV�BfTB��B��B�;B�xBÄBČBĊB��B��B�GB�HB�iB1BPBuB#�B#�B.B@nBD�BC�B79B�B��B�aB�B��B�@B��B��B�%B��B��B�oB�"B{�Bh]BfTB[B7:B
+B
�B
�`B
�*B
��B
�rB
��B
bAB
�B	��B	�5B	ǣB	��B	k~B	F�B	+�B	�B	�B	B��B��B��B��B�rB�B��BȲB�hB�6B�)B�B�B� B� B�B��B�B��B��B�B�B�B�)B�.B�MB�aB�[B�SB�eB�vB�oB�mB�sB�BÐBÐBÑBÑBÒBBÑBŜBėB��B��B��B��B��B��B�B�B�#B�3B�sB�|B�B�B��B	B	dB	�B	�B	$�B	0B	?pB	B�B	A}B	=eB	;YB	B�B	B�B	@yB	@yB	?sB	=bB	=dB	<]B	;YB	:SB	8HB	8FB	;XB	;ZB	=fB	=fB	:QB	67B	1B	0B	/B	1B	1B	.	B	+�B	+�B	1B	:SB	9JB	0B	*�B	$�B	�B	�B	$�B	.B	%�B	�B	�B	�B	�B	�B	�B	�B	#�B	0B	D�B	D�B	C�B	A}B	A}B	C�B	C�B	C�B	@uB	>hB	;UB	9JB	9KB	8DB	3'B	/B	*�B	+�B	&�B	#�B	"�B	!�B	"�B	(�B	+�B	3&B	<\B	B�B	B�B	A}B	BB	H�B	L�B	N�B	M�B	L�B	I�B	F�B	C�B	?oB	:QB	;UB	=bB	B�B	C�B	E�B	H�B	L�B	O�B	S�B	U�B	Y	B	ZB	[B	_,B	b@B	cFB	cDB	dMB	eNB	hdB	n�B	s�B	t�B	u�B	y�B	��B	�B	�B	�B	�%B	�9B	�DB	�DB	�JB	�JB	�DB	�FB	�JB	�oB	�{B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�
B	�B	�B	�%B	�+B	�:B	�:B	�?B	�@B	�EB	�SB	�WB	�^B	�gB	�pB	�xB	ÅB	ƖB	ǝB	ǛB	ɨB	ʲB	ʯB	ʰB	̻B	��B	ͿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�"B	�$B	�"B	�#B	�"B	�$B	�'B	�/B	�3B	�4B	�?B	�:B	�9B	�AB	�?B	�CB	�HB	�GB	�MB	�LB	�SB	�LB	�WB	�^B	�YB	�RB	�TB	�RB	�LB	�KB	�NB	�MB	�KB	�LB	�LB	�[B	�`B	�aB	�`B	�_B	�^B	�_B	�]B	�[B	�]B	�`B	�fB	�fB	�fB	�gB	�\B	�gB	�qB	�B	�{B	�}B	�}B	�~B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �G�O�B
B
XB
WB
�B
&�B
,�B
3B
;MB
ArB
F�B
J�B
P�B
W�B
^B
c;B
gUB
kmB
p�B
r�B
v�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221331    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221331  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221331  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                