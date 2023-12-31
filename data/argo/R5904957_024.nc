CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:08Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181024140808  20181024140808  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @ױ����1   @ױ�b���@3�� ě��c���Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!fD!�fD"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7y�D7��D8� D9  D9� D9��D:y�D:��D;y�D;��D<� D=  D=� D=��D>y�D?  D?� D@  D@� DA  DAy�DA��DB� DC  DC� DD  DD� DE  DE� DF  DF�fDGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D_��D`� Da  Da� Da��Db� DcfDc� Dd  Dd� De  De�fDf  Df� Dg  Dg� DhfDh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dl��DyuD�.fD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B0�
B9=pB@�
BH�
BP�
BX�
B`�
Bhp�Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B���B�k�B�k�BĞ�BȞ�B�k�B�k�B�k�B�k�B�k�B�k�B䞸B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C)C5�C5�C5�C 5�C"5�C$O]C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP)CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�CxO]Cz5�C|5�C~5�C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C�C�C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C�C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C�'�C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD ��D!�D!��D"�D"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3D3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�D8D8�qD9qD9�qD:D:�D;D;�D<D<�qD=qD=�qD>D>�D?qD?�qD@qD@�qDAqDA�DBDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF��DG�DG��DHqDH�qDIqDI�qDJqDJ�qDKqDK�DLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_�D_�qD`D`�qDaqDa�qDbDb�qDc�Dc�qDdqDd�qDeqDe��DfqDf�qDgqDg�qDh�Dh�qDiqDi�qDjqDj�qDkqDk�qDlDl�DmDy��D�5D�ҏ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�t�A�5?A�A��
Aҩ�A҃A�l�A�\)A�M�A�;dA�1'A� �A�%A���Aѕ�AЩ�A��A��AϺ^A�p�A�I�A�Q�A�5?A��A�Q�AΛ�A�9XAН�AЃA�;dA�AϸRA�A�VA�"�A�x�A���A�ĜA�9XAΰ!A�AμjA�(�A��#A͸RA͓uA�l�A�33A��A̩�A��mAˡ�A�E�A���A���A���A�|�A��A�jA�JAȟ�A�r�A�ȴAƬAũ�A�VA�?}A� �A�A���A��^A�?}A��wA�7LA��\A��A��A���A��A���A���A� �A��;A�ffA��TA��TA�JA�5?A�Q�A���A�p�A�ƨA�E�A�A�ĜA�VA�t�A��#A�?}A��A��HA�E�A�G�A�Q�A� �A�VA���A���A���A�7LA�&�A��;A�A���A�~�A~�A|v�A{�-AzA�AxĜAw�As�Ap��AnjAkx�Ah�9AgK�Ae�PAc��Ab��Aa�
A_33A\JAV~�AP��AO+AL�`AJ�jAJ-AI?}AG��AG%AF�RAF �AE�PAC�A@5?A?"�A>ĜA<ȴA;G�A;A:��A:-A7�A6��A5
=A3hsA2jA1�
A0v�A/�A.ffA-��A-oA+VA)XA(I�A'�PA&��A%��A#�-A"9XA��A�^A`BA33AjA�FA?}AoA�wA�HAQ�A��A7LA��Ap�A��A�yA��A  A33A�9AbNA�#A\)A��A��A�mA�A��A
�A
=qA	�A	��A	;dA�wAĜAE�A��AA�!AjA^5A �A��AhsAC�A�A�A��A ��A ��A 9X@��P@�ff@��@���@���@���@�n�@���@�9X@�;d@�@�n�@�G�@�  @�C�@�^5@���@�h@�x�@�G�@�j@�ƨ@�^@���@��;@��y@��#@��@�dZ@���@�5?@���@��;@�
=@�O�@�S�@��@�$�@�@ա�@���@�r�@��;@�$�@ϕ�@�J@��@�A�@���@�l�@�
=@��T@ț�@�1'@�\)@�^5@���@� �@�~�@���@�@��`@�dZ@�+@��@�5?@��@�@��-@�X@���@�b@�ƨ@���@��@���@�5?@��-@�X@���@���@�l�@��H@��\@�-@�@���@�1@�  @�  @��@��;@��F@�C�@�@�O�@�/@���@�Q�@�  @�33@�$�@��7@��7@�V@�I�@��@�|�@�;d@��H@�ȴ@�~�@�-@��#@��-@�x�@�&�@��D@��@�t�@��H@�ff@�$�@���@�p�@�`B@�O�@�?}@�?}@��@�ƨ@���@��P@�\)@�S�@�C�@�;d@��@��@���@�v�@���@��7@�hs@�hs@�G�@�7L@�V@���@��/@�Ĝ@�z�@�Q�@��m@��
@��w@���@�t�@�C�@�"�@�@�-@���@�7L@��@�bN@�b@��F@�@��\@�n�@�-@��-@��@���@�Z@���@��@��@��@��@�ƨ@�t�@�S�@�C�@�"�@�^5@�$�@�$�@��@���@���@�O�@���@�Ĝ@��u@�r�@�Z@�A�@�(�@�  @��@�ƨ@���@�|�@�33@��@��R@�v�@�M�@��-@���@��/@���@���@��/@�V@��@��@�1@��w@��P@�dZ@�;d@��y@�-@��@��^@��h@�hs@�?}@���@��j@�1@��
@��@���@���@�|�@�dZ@�C�@�"�@��H@���@�=q@��@���@�1'@w��@_�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�t�A�5?A�A��
Aҩ�A҃A�l�A�\)A�M�A�;dA�1'A� �A�%A���Aѕ�AЩ�A��A��AϺ^A�p�A�I�A�Q�A�5?A��A�Q�AΛ�A�9XAН�AЃA�;dA�AϸRA�A�VA�"�A�x�A���A�ĜA�9XAΰ!A�AμjA�(�A��#A͸RA͓uA�l�A�33A��A̩�A��mAˡ�A�E�A���A���A���A�|�A��A�jA�JAȟ�A�r�A�ȴAƬAũ�A�VA�?}A� �A�A���A��^A�?}A��wA�7LA��\A��A��A���A��A���A���A� �A��;A�ffA��TA��TA�JA�5?A�Q�A���A�p�A�ƨA�E�A�A�ĜA�VA�t�A��#A�?}A��A��HA�E�A�G�A�Q�A� �A�VA���A���A���A�7LA�&�A��;A�A���A�~�A~�A|v�A{�-AzA�AxĜAw�As�Ap��AnjAkx�Ah�9AgK�Ae�PAc��Ab��Aa�
A_33A\JAV~�AP��AO+AL�`AJ�jAJ-AI?}AG��AG%AF�RAF �AE�PAC�A@5?A?"�A>ĜA<ȴA;G�A;A:��A:-A7�A6��A5
=A3hsA2jA1�
A0v�A/�A.ffA-��A-oA+VA)XA(I�A'�PA&��A%��A#�-A"9XA��A�^A`BA33AjA�FA?}AoA�wA�HAQ�A��A7LA��Ap�A��A�yA��A  A33A�9AbNA�#A\)A��A��A�mA�A��A
�A
=qA	�A	��A	;dA�wAĜAE�A��AA�!AjA^5A �A��AhsAC�A�A�A��A ��A ��A 9X@��P@�ff@��@���@���@���@�n�@���@�9X@�;d@�@�n�@�G�@�  @�C�@�^5@���@�h@�x�@�G�@�j@�ƨ@�^@���@��;@��y@��#@��@�dZ@���@�5?@���@��;@�
=@�O�@�S�@��@�$�@�@ա�@���@�r�@��;@�$�@ϕ�@�J@��@�A�@���@�l�@�
=@��T@ț�@�1'@�\)@�^5@���@� �@�~�@���@�@��`@�dZ@�+@��@�5?@��@�@��-@�X@���@�b@�ƨ@���@��@���@�5?@��-@�X@���@���@�l�@��H@��\@�-@�@���@�1@�  @�  @��@��;@��F@�C�@�@�O�@�/@���@�Q�@�  @�33@�$�@��7@��7@�V@�I�@��@�|�@�;d@��H@�ȴ@�~�@�-@��#@��-@�x�@�&�@��D@��@�t�@��H@�ff@�$�@���@�p�@�`B@�O�@�?}@�?}@��@�ƨ@���@��P@�\)@�S�@�C�@�;d@��@��@���@�v�@���@��7@�hs@�hs@�G�@�7L@�V@���@��/@�Ĝ@�z�@�Q�@��m@��
@��w@���@�t�@�C�@�"�@�@�-@���@�7L@��@�bN@�b@��F@�@��\@�n�@�-@��-@��@���@�Z@���@��@��@��@��@�ƨ@�t�@�S�@�C�@�"�@�^5@�$�@�$�@��@���@���@�O�@���@�Ĝ@��u@�r�@�Z@�A�@�(�@�  @��@�ƨ@���@�|�@�33@��@��R@�v�@�M�@��-@���@��/@���@���@��/@�V@��@��@�1@��w@��P@�dZ@�;d@��y@�-@��@��^@��h@�hs@�?}@���@��j@�1@��
@��@���@���@�|�@�dZ@�C�@�"�@��H@���@�=q@��@���@�1'@w��@_�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
JB

=B
1B
+B
B
B
%B
+B
1B
1B
+B
+B
%B
B	��B	�B	�)B	�NB	�B	�yB	�mB	�B	��B	��B	��B
hB	��B
-B
�B
�B
u�B
jB
r�B
]/B
�B
��B
�jB
��B
��B
ƨB
�XB
ŢB
�BB
�
B
��B
��B
�BB�B]/BdZBp�Bu�Bu�B}�B��B��B��B�3B�LB�jB�B�/B�B�BB�B!�B:^BI�BT�B]/B�B��B��B�mB�yB�B��B��B��B�B�B�B�BdZB_;B]/BVBL�BA�B1'B)�B"�B�B��B�mB�/B��B�}B�3B��By�Bn�BiyBVB@�BuBB
�B
��B
ĜB
��B
y�B
I�B
8RB
(�B
�B
oB
	7B	��B	�B	�#B	ĜB	�'B	��B	�1B	|�B	p�B	dZB	]/B	T�B	D�B	0!B	�B��B�B�fB�;B�)B�B��B��B��B��B��BƨB��B��B�wB�wB�qB�jB�dB�RB�3B�!B�B�B��B��B��B��B��B��B��B�{B�oB�uB�{B�{B�oB��B��B��B�oB�oB�hB�oB�hB�oB�oB��B�{B�oB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�7B�1B�1B�7B�7B�1B�DB�\B�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�B�B�!B�!B�-B�3B�9B�?B�XB�dB�dB�}BBBÖBÖBÖBŢBƨBƨBǮBɺBɺB��B��B��B��B��B��B�B�/B�/B�)B�;B�;B�;B�HB�HB�NB�TB�TB�ZB�`B�`B�fB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	
=B	\B	bB	uB	�B	�B	�B	�B	 �B	!�B	&�B	+B	-B	,B	,B	-B	/B	0!B	33B	6FB	7LB	:^B	=qB	@�B	B�B	E�B	F�B	G�B	H�B	I�B	J�B	M�B	P�B	S�B	W
B	YB	[#B	aHB	cTB	dZB	e`B	ffB	gmB	k�B	r�B	s�B	t�B	u�B	u�B	u�B	v�B	w�B	x�B	y�B	|�B	�B	�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�\B	�oB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�RB	�^B	�qB	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�;B	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
 B
�B
8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
JB

=B
1B
+B
B
B
%B
+B
1B
1B
+B
+B
%B
B	��B	�B	�)B	�NB	�B	�yB	�mB	�B	��B	��B	��B
hB	��B
-B
�B
�B
u�B
jB
r�B
]/B
�B
��B
�jB
��B
��B
ƨB
�XB
ŢB
�BB
�
B
��B
��B
�BB�B]/BdZBp�Bu�Bu�B}�B��B��B��B�3B�LB�jB�B�/B�B�BB�B!�B:^BI�BT�B]/B�B��B��B�mB�yB�B��B��B��B�B�B�B�BdZB_;B]/BVBL�BA�B1'B)�B"�B�B��B�mB�/B��B�}B�3B��By�Bn�BiyBVB@�BuBB
�B
��B
ĜB
��B
y�B
I�B
8RB
(�B
�B
oB
	7B	��B	�B	�#B	ĜB	�'B	��B	�1B	|�B	p�B	dZB	]/B	T�B	D�B	0!B	�B��B�B�fB�;B�)B�B��B��B��B��B��BƨB��B��B�wB�wB�qB�jB�dB�RB�3B�!B�B�B��B��B��B��B��B��B��B�{B�oB�uB�{B�{B�oB��B��B��B�oB�oB�hB�oB�hB�oB�oB��B�{B�oB�bB�hB�uB��B��B��B��B��B��B��B��B��B��B��B�{B�hB�VB�7B�1B�1B�7B�7B�1B�DB�\B�hB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�B�B�!B�!B�-B�3B�9B�?B�XB�dB�dB�}BBBÖBÖBÖBŢBƨBƨBǮBɺBɺB��B��B��B��B��B��B�B�/B�/B�)B�;B�;B�;B�HB�HB�NB�TB�TB�ZB�`B�`B�fB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	
=B	\B	bB	uB	�B	�B	�B	�B	 �B	!�B	&�B	+B	-B	,B	,B	-B	/B	0!B	33B	6FB	7LB	:^B	=qB	@�B	B�B	E�B	F�B	G�B	H�B	I�B	J�B	M�B	P�B	S�B	W
B	YB	[#B	aHB	cTB	dZB	e`B	ffB	gmB	k�B	r�B	s�B	t�B	u�B	u�B	u�B	v�B	w�B	x�B	y�B	|�B	�B	�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�\B	�oB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�RB	�^B	�qB	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�;B	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
 B
�B
8�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140808                              AO  ARCAADJP                                                                    20181024140808    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140808  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140808  QCF$                G�O�G�O�G�O�0               