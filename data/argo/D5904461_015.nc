CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:35Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221335  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_015                   2C  D   APEX                            6531                            072314                          846 @�$˕m��1   @�$�5��@1p�`A�7�c�$�/�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DO��DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�3D�fD�<�D�vfD�ɚD�fD�FfD�� D�ɚD�3D�FfD���D�� D�fD�FfDڐ D��fD���D�<�D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@ÅA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B0�
B8�
BA=pBH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�8RB�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�CO]C5�C5�C5�C5�C 5�C"5�C$5�C&)C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�Dy��D�D�C�D�}D��RD�D�MD���D��RD�	�D�MD��RD�ƸD�D�MDږ�D��D� RD�C�D�mD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A�A�A�bA��A� �A� �A��A��A��A��A� �A�$�A�$�A�&�A�5?A�7LA�9XA�;dA�7LA�9XA�;dA�=qA�;dA�5?A�5?A�5?A�5?A�33A�&�A��mA׍PA�$�A֣�A�ZA��A�1A�K�A�n�A��A���A�&�AȬA��yA��mA�C�A���A��A���A�+A��!A��A��A�C�A�n�A�I�A��-A��
A��!A��A�-A��^A���A��TA�K�A��-A�l�A�z�A���A�33A�/A�=qA��A���A�-A���A���A�9XA�A�5?A�VA�XA�A�S�A� �A��A�  A}"�Az1At5?ArZAq�Aq�-Aq"�AoO�Am+AkAj��Ah�Ag��Ag7LAf�A`�A\(�AZbAV�HAQx�AP{AK��AH��AG�^AFjAD�9AC`BAB�AB�jABVAA�A@9XA>{A;�-A9��A97LA8�`A7ƨA7K�A6�jA4�RA4bA3��A3�mA3�A3+A1l�A//A-?}A,�RA,1'A*JA)+A'��A%��A%&�A$�+A#K�A"^5A"bA!�TA!��A ��A $�A�A��A�mA33A  A�AC�A��AbAx�A��A�A=qAAn�A|�A��A^5A(�AbA�AA�AS�A+A%A��A�A��A
�A
E�A	�A	�A`BAI�A�Av�AM�A��A�PA ff@�^5@�n�@�^5@��-@��@��@���@��j@���@��@�@�~�@�/@�\)@�ff@�@�hs@�p�@��@웦@��@�t�@��@�ȴ@�~�@��@��@�9@��
@�;d@�~�@�-@��@�1'@�\)@�~�@�@�z�@�1'@���@߮@�@�7L@�l�@�hs@���@�K�@�"�@֧�@�ff@�$�@�@պ^@���@� �@��;@�t�@�"�@��H@�~�@��@���@Ѻ^@�hs@��/@�Q�@���@ϥ�@Ώ\@���@͉7@�`B@�7L@�V@���@�Z@ˍP@�-@�%@�bN@ǶF@���@Ƈ+@�-@��@Ų-@őh@�/@ě�@å�@�ȴ@�ff@��#@��h@�hs@�7L@���@��u@��D@�Q�@��@��m@���@���@��@��@�(�@���@���@�;d@�@���@�K�@�o@��@�33@�-@��h@���@��@��@��j@�9X@���@��;@��
@�9X@�r�@��u@�A�@���@�b@�^5@�n�@��+@���@��#@��^@�X@�bN@���@�S�@�l�@�|�@��F@��m@��-@���@���@��R@�
=@�o@�"�@�"�@��H@���@�X@���@��-@���@�r�@�z�@�Z@�A�@��@�ƨ@���@���@�l�@�C�@��@���@��!@�5?@���@���@��^@�hs@�%@��/@���@��9@��@� �@��
@��+@�-@�@�@�?}@�/@�hs@�V@�Ĝ@��@���@�j@���@�|�@�t�@�\)@�
=@��H@���@�=q@�G�@���@���@��@�?}@��j@�1'@���@���@�@�@�7L@��@��j@�Z@��@��y@��\@�J@�J@��T@��7@�&�@��@��m@�|�@�S�@�"�@�
=@�o@�ȴ@���@�v�@�$�@��@�`B@���@�j@�I�@�b@��;@��F@�+@�~�@�V@�@���@��h@�V@�(�@��m@�ƨ@��@�C�@�o@���@��@�ȴ@��\@�ff@�@���@�hs@�V@��`@��j@��u@�A�@�(�@�(�@�b@�1@��@��
@��@�@��@�~�@�5?@��T@��-@��7@�`B@�&�@�Ĝ@�r�@�r�@��@�t�@�S�@�`B@�ƨ@}��@v@jn�@`A�@XbN@Pr�@K�F@D��@@��@:�@3@,�@%`B@!&�@�F@@��@9X@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�A���A�A�A�bA��A� �A� �A��A��A��A��A� �A�$�A�$�A�&�A�5?A�7LA�9XA�;dA�7LA�9XA�;dA�=qA�;dA�5?A�5?A�5?A�5?A�33A�&�A��mA׍PA�$�A֣�A�ZA��A�1A�K�A�n�A��A���A�&�AȬA��yA��mA�C�A���A��A���A�+A��!A��A��A�C�A�n�A�I�A��-A��
A��!A��A�-A��^A���A��TA�K�A��-A�l�A�z�A���A�33A�/A�=qA��A���A�-A���A���A�9XA�A�5?A�VA�XA�A�S�A� �A��A�  A}"�Az1At5?ArZAq�Aq�-Aq"�AoO�Am+AkAj��Ah�Ag��Ag7LAf�A`�A\(�AZbAV�HAQx�AP{AK��AH��AG�^AFjAD�9AC`BAB�AB�jABVAA�A@9XA>{A;�-A9��A97LA8�`A7ƨA7K�A6�jA4�RA4bA3��A3�mA3�A3+A1l�A//A-?}A,�RA,1'A*JA)+A'��A%��A%&�A$�+A#K�A"^5A"bA!�TA!��A ��A $�A�A��A�mA33A  A�AC�A��AbAx�A��A�A=qAAn�A|�A��A^5A(�AbA�AA�AS�A+A%A��A�A��A
�A
E�A	�A	�A`BAI�A�Av�AM�A��A�PA ff@�^5@�n�@�^5@��-@��@��@���@��j@���@��@�@�~�@�/@�\)@�ff@�@�hs@�p�@��@웦@��@�t�@��@�ȴ@�~�@��@��@�9@��
@�;d@�~�@�-@��@�1'@�\)@�~�@�@�z�@�1'@���@߮@�@�7L@�l�@�hs@���@�K�@�"�@֧�@�ff@�$�@�@պ^@���@� �@��;@�t�@�"�@��H@�~�@��@���@Ѻ^@�hs@��/@�Q�@���@ϥ�@Ώ\@���@͉7@�`B@�7L@�V@���@�Z@ˍP@�-@�%@�bN@ǶF@���@Ƈ+@�-@��@Ų-@őh@�/@ě�@å�@�ȴ@�ff@��#@��h@�hs@�7L@���@��u@��D@�Q�@��@��m@���@���@��@��@�(�@���@���@�;d@�@���@�K�@�o@��@�33@�-@��h@���@��@��@��j@�9X@���@��;@��
@�9X@�r�@��u@�A�@���@�b@�^5@�n�@��+@���@��#@��^@�X@�bN@���@�S�@�l�@�|�@��F@��m@��-@���@���@��R@�
=@�o@�"�@�"�@��H@���@�X@���@��-@���@�r�@�z�@�Z@�A�@��@�ƨ@���@���@�l�@�C�@��@���@��!@�5?@���@���@��^@�hs@�%@��/@���@��9@��@� �@��
@��+@�-@�@�@�?}@�/@�hs@�V@�Ĝ@��@���@�j@���@�|�@�t�@�\)@�
=@��H@���@�=q@�G�@���@���@��@�?}@��j@�1'@���@���@�@�@�7L@��@��j@�Z@��@��y@��\@�J@�J@��T@��7@�&�@��@��m@�|�@�S�@�"�@�
=@�o@�ȴ@���@�v�@�$�@��@�`B@���@�j@�I�@�b@��;@��F@�+@�~�@�V@�@���@��h@�V@�(�@��m@�ƨ@��@�C�@�o@���@��@�ȴ@��\@�ff@�@���@�hs@�V@��`@��j@��u@�A�@�(�@�(�@�b@�1@��@��
@��@�@��@�~�@�5?@��T@��-@��7@�`B@�&�@�Ĝ@�r�@�r�@��@�t�G�O�@�`B@�ƨ@}��@v@jn�@`A�@XbN@Pr�@K�F@D��@@��@:�@3@,�@%`B@!&�@�F@@��@9X@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBm�Bm�Bm�Bm�Bm�Bn�Bo�Bo�Bl�Bl�Bm�Bl�Bm�Bo�Bo�Bp�Bu�Bv�Bu�Bu�Bs�Bs�Bv�Bx�Bz�B� B� B� B� B� B�B�B�B�B�B�%B�7B�DB�{B��B�B��BVB�B7LBO�BQ�BN�BA�BI�B\)BR�B33B �BB��B�mB�)B�9BÖBÖB��By�BcTB&�BJB
�B
�ZB
�B
��B
��B
ǮB
�LB
��B
�{B
w�B
dZB
P�B
K�B
7LB
?}B
E�B
)�B
�B
bB
B	�sB	��B	�jB	��B	�1B	� B	�B	}�B	y�B	p�B	e`B	[#B	R�B	H�B	A�B	<jB	0!B	bB��B�B�5B��BÖB�wB�jB�^B�RB�LB�RB�LB�FB�?B�9B�9B�RB�XB�XB�XB�XB�9B�3B�-B�FB�LB�LB�FB�FB�FB�dB�qB�wB�qB�jB�qB�wB�}B��BBBǮB��B��B��B��B�
B�B�B�
B�/B�HB�`B�`B�`B�`B�fB�sB�sB�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	DB		7B	1B	%B	B��B�B�B�ZB�
B��B��B��B��B��B��B��B��BȴBȴB��B��B��B�#B�B�
B��B��B��B�
B�#B�;B�HB�`B�yB�B�B��B��B	B	B	1B	
=B	PB	PB	VB	hB	{B	�B	�B	�B	�B	�B	�B	 �B	�B	�B	%�B	&�B	%�B	&�B	.B	/B	/B	/B	/B	1'B	33B	33B	49B	7LB	9XB	9XB	:^B	=qB	=qB	=qB	>wB	?}B	@�B	@�B	@�B	@�B	@�B	A�B	A�B	A�B	A�B	B�B	F�B	H�B	I�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	K�B	K�B	L�B	Q�B	VB	W
B	ZB	[#B	\)B	\)B	^5B	_;B	_;B	aHB	aHB	aHB	`BB	bNB	dZB	k�B	l�B	l�B	n�B	p�B	q�B	t�B	v�B	w�B	|�B	� B	�B	�B	�7B	�PB	�bB	�oB	�oB	�oB	�{B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�'B	�-B	�3B	�-B	�'B	�!B	�?B	�XB	�dB	�qB	��B	ƨB	��B	ŢB	��B	��B	��B	�
B	�B	�B	�B	�
B	��B	�B	�#B	�B	�#B	�/B	�/B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
uB
�B
�B
%�B
-B
33B
8RB
=qB
C�B
G�B
M�B
T�B
ZB
_;B
cTB
hsB
n�B
q�B
w�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   Bm�Bm�Bm�Bm�Bm�Bn�Bo�Bo�Bl|Bl|Bm�BlzBm�Bo�Bo�Bp�Bu�Bv�Bu�Bu�Bs�Bs�Bv�Bx�Bz�B�B�B�B�B�B��B�B�B�B�	B�B�(B�4B�kB�qB�xB��BEB�B7<BO�BQ�BN�BAyBI�B\BR�B3#B �BB��B�VB�B�&BÂBÃB��By�BcBB&�B9B
�B
�EB
�	B
��B
��B
ǛB
�:B
��B
�kB
w�B
dKB
P�B
K�B
7=B
?lB
E�B
)�B
�B
VB
 �B	�iB	��B	�^B	��B	�'B	�B	��B	}�B	y�B	p�B	eUB	[B	R�B	H�B	A�B	<bB	0B	[B��B�B�1B��BÒB�vB�hB�]B�OB�HB�MB�JB�BB�=B�8B�5B�PB�UB�SB�TB�TB�7B�1B�)B�CB�IB�IB�BB�CB�AB�aB�lB�sB�mB�eB�jB�rB�xB��BBBǨB��B��B��B��B�B�B�
B�B�)B�BB�WB�WB�XB�XB�^B�jB�kB�|B�}B�B�B�vB�B��B��B��B��B��B��B	 �B	 �B	 �B	B	B	:B		-B	)B	B	B��B�B�B�QB�B��B��B��BʹB��B��B��B��BȬBȮB˾B˽B��B�B�B�B��B��B��B�B�B�2B�>B�VB�oB�B�B��B��B	B	B	&B	
0B	EB	EB	IB	]B	pB	tB	�B	�B	�B	�B	�B	 �B	�B	�B	%�B	&�B	%�B	&�B	.B	/B	/B	/B	/B	1B	3'B	3&B	4-B	7?B	9KB	9HB	:QB	=cB	=cB	=dB	>gB	?oB	@vB	@uB	@wB	@uB	@tB	A}B	A{B	A{B	A{B	B�B	F�B	H�B	I�B	I�B	J�B	J�B	J�B	K�B	K�B	K�B	K�B	K�B	L�B	Q�B	U�B	V�B	ZB	[B	\B	\B	^%B	_*B	_,B	a8B	a9B	a:B	`3B	b?B	dKB	kuB	lzB	l|B	n�B	p�B	q�B	t�B	v�B	w�B	|�B	�B	�B	�B	�'B	�>B	�TB	�\B	�[B	�^B	�lB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�+B	�DB	�SB	�^B	�pB	ƗB	�rB	ŏB	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	�B	�
B	�B	�B	�B	�"B	�'B	�(B	�-B	�5B	�4B	�:B	�9B	�:B	�9B	�@B	�RB	�XB	�ZB	�_B	�kB	�jB	�jB	�kB	�kB	�oB	�jB	�_B	�UB	�^B	�^B	�kB	�qB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�}B	�B	�{B	�sB	�rB	�hB	�wB	�~B	�B	�qB	�jB	�dB	�`B	�dB	�gB	�eB	�jB	�qB	�qB	�vB	�vB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
B
B
B

B
B
	B
B
	B

B
B
	B
B
G�O�B
	"B
_B
}B
�B
%�B
,�B
3B
8;B
=ZB
C}B
G�B
M�B
T�B
ZB
_$B
c<B
h[B
n�B
q�B
w�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221335    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221335  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221335  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                