CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:49Z AOML 3.0 creation; 2016-08-07T21:36:31Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221349  20160807143631  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_027                   2C  D   APEX                            6531                            072314                          846 @�4�e 1   @�4��/_�@2P�`A�7�d�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy� D�	�D�@ D���D�� D�fD�<�D��fD���D�  D�FfD�s3DǼ�D��D�I�DچfD�� D�fD�6fD�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
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
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B잸B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD��D�D�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(D(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.�D.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDts�Dy�qD�RD�F�D���D�ָD�D�C�D��D��D��D�MD�y�D�ÅD� RD�PRDڍD�ƸD�D�=D�RD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�(�A�+A�+A�/A�/A�33A�5?A�33A�/A�/A�G�A�\)A�XA�ffA�VA�ffA�ĜAͮA͉7A�bNA�A̼jA�r�A�7LA�A��HA���AˑhA�?}A�$�A��A�A���A���A���A�A�%A���A��;A���Aʰ!Aʟ�Aʥ�AʬAʺ^A�ĜA�v�A�9XA�/A�I�A�M�A�9XA�$�A�{A��`A�v�A�
=AƾwA�5?Aũ�A�S�A�"�A�A�I�A���A¬A���A��A�E�A�/A�C�A�1'A�n�A�"�A�9XA��A�JA�XA��A�1A���A��7A���A���A�&�A�bA���A��;A��yA�K�A���A�G�A���A��A���A��A���A�Q�A��-A�A�A��A|^5Au��As�Ap��Am�TAl$�AjffAi?}Ah1Af��Aet�Ab��A^��A[?}AVr�AP5?AI��AF�!ACK�AB�`ABE�A@=qA=|�A:~�A8��A7�A6E�A5�;A5oA3O�A0��A0M�A0�A/��A+ƨA+oA* �A)hsA(�jA'��A%��A"ĜA!�wA!K�A!&�A ��AK�A��A|�AE�A;dAz�A=qA+A`BAbNA1AdZA�mA�A�RAJA
=A~�A�wA��A"�A��A
ĜA
{A	G�A�^A�HAz�A9XAƨAhsAA�`A��A�^A�yA�TA7LA ff@�|�@�-@�K�@���@�X@��j@�9X@���@��@�V@���@�&�@��@�I�@�@�"�@�{@�I�@�S�@��@��^@��@�^@�@�ƨ@�o@�v�@�@�^@�h@�V@�P@�C�@�|�@�dZ@��@�K�@�5?@��@�X@���@ߍP@�X@�j@ۮ@��y@�5?@�hs@�@�7L@ԛ�@�9X@�1@�=q@Ѳ-@�b@��@�V@�{@���@�x�@�G�@��@̼j@�A�@˶F@˥�@ˍP@ʸR@��@�1'@ǅ@��@���@�
=@Ə\@Ƈ+@Ə\@Ɵ�@�5?@�{@��T@�p�@��@Ĵ9@�A�@� �@�ƨ@�C�@�"�@��H@�ff@�x�@�%@���@�9X@��
@���@��@�M�@��h@�Ĝ@��@�?}@��-@�M�@���@�33@�S�@��@�ȴ@�=q@��@�?}@��9@�z�@�b@��@�t�@�C�@�"�@�
=@�
=@�@��!@�{@��@��`@�Ĝ@��@��D@�j@��@�dZ@�@��H@�v�@���@��@��@�Ĝ@�Ĝ@���@�K�@��@�ff@�E�@�M�@�5?@�-@�J@��@�bN@�Z@�A�@�Q�@�A�@��w@��H@���@��R@���@�J@��#@��^@�hs@�?}@���@��@�r�@�Q�@�9X@��@�1@�  @��;@�ƨ@��P@�\)@�33@��@�-@��@��-@�G�@��@�V@���@�z�@�Z@�Q�@�1'@��
@�ƨ@�33@��R@��@�p�@�X@��@��D@�9X@��@��
@�|�@�C�@�"�@���@�M�@��@��#@��^@���@�x�@�%@��@��@�(�@���@��w@���@��P@��@�|�@�t�@�S�@�+@��@��\@�-@��@�@���@��-@�`B@��9@�z�@��@���@��9@��@�A�@�1@��w@�t�@�K�@�
=@���@��+@��@���@��^@���@�x�@��@���@��9@���@���@��u@�Z@��m@��P@�|�@�dZ@�C�@�C�@�"�@��y@�v�@��-@�?}@��@���@��u@��@�(�@�(�@�C�@�o@���@��@��H@��@��@��@�{@�@�&�@��D@�Z@���@��@�\)@�C�@��@���@���@���@��\@�v�@�M�@�  @�A�@��F@z�@p  @g;d@\z�@T�@K�
@E�@>�y@8��@1��@*�!@$j@;d@x�@�-@�#@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�(�A�+A�+A�/A�/A�33A�5?A�33A�/A�/A�G�A�\)A�XA�ffA�VA�ffA�ĜAͮA͉7A�bNA�A̼jA�r�A�7LA�A��HA���AˑhA�?}A�$�A��A�A���A���A���A�A�%A���A��;A���Aʰ!Aʟ�Aʥ�AʬAʺ^A�ĜA�v�A�9XA�/A�I�A�M�A�9XA�$�A�{A��`A�v�A�
=AƾwA�5?Aũ�A�S�A�"�A�A�I�A���A¬A���A��A�E�A�/A�C�A�1'A�n�A�"�A�9XA��A�JA�XA��A�1A���A��7A���A���A�&�A�bA���A��;A��yA�K�A���A�G�A���A��A���A��A���A�Q�A��-A�A�A��A|^5Au��As�Ap��Am�TAl$�AjffAi?}Ah1Af��Aet�Ab��A^��A[?}AVr�AP5?AI��AF�!ACK�AB�`ABE�A@=qA=|�A:~�A8��A7�A6E�A5�;A5oA3O�A0��A0M�A0�A/��A+ƨA+oA* �A)hsA(�jA'��A%��A"ĜA!�wA!K�A!&�A ��AK�A��A|�AE�A;dAz�A=qA+A`BAbNA1AdZA�mA�A�RAJA
=A~�A�wA��A"�A��A
ĜA
{A	G�A�^A�HAz�A9XAƨAhsAA�`A��A�^A�yA�TA7LA ff@�|�@�-@�K�@���@�X@��j@�9X@���@��@�V@���@�&�@��@�I�@�@�"�@�{@�I�@�S�@��@��^@��@�^@�@�ƨ@�o@�v�@�@�^@�h@�V@�P@�C�@�|�@�dZ@��@�K�@�5?@��@�X@���@ߍP@�X@�j@ۮ@��y@�5?@�hs@�@�7L@ԛ�@�9X@�1@�=q@Ѳ-@�b@��@�V@�{@���@�x�@�G�@��@̼j@�A�@˶F@˥�@ˍP@ʸR@��@�1'@ǅ@��@���@�
=@Ə\@Ƈ+@Ə\@Ɵ�@�5?@�{@��T@�p�@��@Ĵ9@�A�@� �@�ƨ@�C�@�"�@��H@�ff@�x�@�%@���@�9X@��
@���@��@�M�@��h@�Ĝ@��@�?}@��-@�M�@���@�33@�S�@��@�ȴ@�=q@��@�?}@��9@�z�@�b@��@�t�@�C�@�"�@�
=@�
=@�@��!@�{@��@��`@�Ĝ@��@��D@�j@��@�dZ@�@��H@�v�@���@��@��@�Ĝ@�Ĝ@���@�K�@��@�ff@�E�@�M�@�5?@�-@�J@��@�bN@�Z@�A�@�Q�@�A�@��w@��H@���@��R@���@�J@��#@��^@�hs@�?}@���@��@�r�@�Q�@�9X@��@�1@�  @��;@�ƨ@��P@�\)@�33@��@�-@��@��-@�G�@��@�V@���@�z�@�Z@�Q�@�1'@��
@�ƨ@�33@��R@��@�p�@�X@��@��D@�9X@��@��
@�|�@�C�@�"�@���@�M�@��@��#@��^@���@�x�@�%@��@��@�(�@���@��w@���@��P@��@�|�@�t�@�S�@�+@��@��\@�-@��@�@���@��-@�`B@��9@�z�@��@���@��9@��@�A�@�1@��w@�t�@�K�@�
=@���@��+@��@���@��^@���@�x�@��@���@��9@���@���@��u@�Z@��m@��P@�|�@�dZ@�C�@�C�@�"�@��y@�v�@��-@�?}@��@���@��u@��@�(�@�(�@�C�@�o@���@��@��H@��@��@��@�{@�@�&�@��D@�Z@���@��@�\)@�C�@��@���@���@���@��\@�v�G�O�@�  @�A�@��F@z�@p  @g;d@\z�@T�@K�
@E�@>�y@8��@1��@*�!@$j@;d@x�@�-@�#@�@	hs11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�fB	�fB	�fB	�mB	�mB	�sB	�mB	�mB	�fB	�fB	��B
hB
G�B
^5B
o�B
�B
ɺB
�B
�B
�/B
�B
��B	7B�B6FBF�BM�Bn�B��B��B��B�!B��B��B��B��B��B�;B��B%BVBuB�B�B49BM�BXB^5BffBt�B{�B�B�+B�\B��B��B�^BƨB��B��B�B�5B�BB�;B�)B��B�JBT�B9XB��B��B�DBt�BaHBE�B#�B{BB
�B
�B
�sB
�TB
�B
�-B
��B
��B
��B
��B
�VB
�7B
~�B
p�B
cTB
ZB
O�B
A�B
#�B
JB
B	��B	�;B	ĜB	��B	�{B	�+B	w�B	l�B	dZB	]/B	T�B	L�B	B�B	49B	 �B	PB�B�BŢB�qB�?B�9B�!B��B��B��B��B��B��B��B��B�B�FB�?B�3B�'B�XB�XB�XB�XB�XB�RB�LB��BŢBȴBǮBȴB��B�B��B��B�B�B�B��B��B��B��B��BɺB��B��B��B��B��B��B��B��B��B��B��B��B�B�/B�;B�;B�HB�NB�TB�TB�ZB�fB�fB�`B�ZB�HB�BB�/B�5B�/B�)B�)B�)B�#B�#B�#B�)B�/B�/B�5B�;B�;B�HB�`B�fB�mB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	1B		7B		7B	
=B	JB	VB	\B	bB	hB	uB	uB	hB	\B	VB	PB	oB	hB	hB	bB	bB	hB	uB	�B	�B	�B	"�B	"�B	"�B	$�B	,B	,B	.B	33B	49B	<jB	C�B	G�B	P�B	VB	XB	XB	ZB	\)B	]/B	`BB	aHB	bNB	ffB	o�B	r�B	r�B	q�B	w�B	|�B	~�B	}�B	}�B	}�B	�B	�B	�DB	�VB	�bB	�oB	�hB	��B	��B	��B	�B	�!B	�3B	�?B	�LB	�XB	�dB	�jB	�jB	�wB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ŢB	ƨB	ŢB	ŢB	ÖB	ĜB	ǮB	ɺB	��B	ɺB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�B	�#B	�B	�#B	�)B	�/B	�)B	�5B	�5B	�5B	�;B	�;B	�BB	�NB	�NB	�TB	�TB	�ZB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
VB
�B
�B
#�B
(�B
1'B
7LB
?}B
F�B
I�B
P�B
W
B
]/B
bNB
ffB
jB
n�B
q�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�`B	�`B	�^B	�fB	�dB	�lB	�dB	�bB	�^B	�`B	��B
`B
G�B
^*B
o�B
�B
ɪB
�B
�B
� B
�|B
��B	(B�B64BF�BM�Bn�B�rB��B��B�B�zB��B��B��B��B�)B��BBEBcB|B�B4)BM�BW�B^$BfWBt�B{�B��B�B�LB��B��B�OBƙB̼B��B�B�%B�2B�(B�B̾B�:BT�B9GB��B��B�4Bt�Ba4BE�B#�BiBB
�B
�tB
�eB
�CB
��B
�B
��B
��B
��B
�pB
�EB
�'B
~�B
p�B
cDB
ZB
O�B
AzB
#�B
;B
B	��B	�0B	ĒB	��B	�rB	�#B	w�B	l�B	dQB	]'B	T�B	L�B	B�B	42B	 �B	KB�B�BşB�mB�;B�6B�B��B��B��B��B��B��B��B��B��B�BB�=B�.B�#B�TB�SB�RB�RB�RB�NB�HB�~BśBȰBǨBȰB��B� B��B��B�B�B�B��B��BʽBʽBʻBɵB��B��B��B˿B��B��B��B��B��B��B��B��B�B�&B�5B�2B�BB�GB�KB�LB�SB�[B�]B�WB�RB�>B�:B�(B�-B�'B�!B�"B�!B�B�B�B�B�$B�&B�-B�2B�2B�>B�YB�]B�eB�qB�|B�B�{B�zB�vB�}B�B�B�B��B��B��B��B��B	B	B	%B		+B		+B	
0B	?B	KB	OB	YB	]B	hB	jB	[B	OB	MB	CB	aB	ZB	]B	WB	UB	]B	iB	yB	�B	�B	"�B	"�B	"�B	$�B	+�B	+�B	.B	3%B	4-B	<\B	C�B	G�B	P�B	U�B	W�B	X B	ZB	\B	] B	`2B	a:B	b?B	fVB	o�B	r�B	r�B	q�B	w�B	|�B	~�B	}�B	}�B	}�B	��B	�B	�2B	�CB	�QB	�^B	�WB	�yB	��B	��B	��B	�B	�!B	�,B	�:B	�EB	�RB	�XB	�XB	�eB	�fB	�kB	�qB	�wB	�vB	�wB	�uB	�uB	�wB	�wB	ÃB	ĉB	ŐB	ŐB	ŏB	ŏB	ŐB	őB	ƕB	ŏB	ƕB	ŏB	ŏB	ÃB	ċB	ǝB	ɩB	ʯB	ɧB	ǜB	ǜB	ȢB	ɩB	ɩB	ɩB	ɪB	ȠB	ȠB	ɧB	ɤB	ʰB	˴B	˶B	˳B	��B	̻B	̻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�
B	�B	�B	�B	�B	�"B	�"B	�"B	�$B	�&B	�-B	�8B	�7B	�AB	�@B	�DB	�RB	�XB	�dB	�rB	�tB	�xB	�yB	�wB	�~B	�~B	�}B	�wB	�|B	�zB	�yB	�vB	�sB	�pB	�iB	�rB	�pB	�pB	�rB	�qB	�rB	�rB	�rB	�pB	�pB	�qB	�rB	�pB	�pB	�qB	�xB	�}B	�B	�|B	�|B	�}B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
G�O�B
B
BB
qB
�B
#�B
(�B
1B
76B
?dB
F�B
I�B
P�B
V�B
]B
b8B
fQB
jhB
nB
q�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436312016080714363120160807143631  AO  ARCAADJP                                                                    20150226221349    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221349  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221349  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143631  IP                  G�O�G�O�G�O�                