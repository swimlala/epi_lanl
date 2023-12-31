CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:49Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140849  20181024140849  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����w��1   @���ww�0@53�E����d�hr�!1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CO�fCQ�fCT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D
��D� D��Dy�D  D� D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D  D� D  D� D  Dy�D  D� DfD� D  D� D��Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)�fD*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D_��D`y�D`��Da� Da��Db� Dc  Dc� Dd  Ddy�De  De� Df  Df�fDgfDg�fDhfDh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� DofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dyz�D�B�D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@ƸRA\)A#\)AD��Ac\)A��A��A��A��A��AѮA�z�A�B �
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
B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�CO]CO]C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C2)C45�C65�C85�C:5�C<5�C>5�C@)CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP)CR)CT5�CV5�CXO]CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C��C�C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C�C��C�'�C��C��C��C��C��C��C��C�C��C��C��D qD �qDqD�qDqD�qDD�DqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�DD�qDD�DqD�qDqD�qDqD�qDqD�qDD�qDqD�qDqD��DqD�qDqD�qDqD�qDqD�qDqD�DqD�qD�D�qDqD�qDD�DqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)�D)��D*qD*�qD+qD+�qD,qD,��D-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3�D3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�D<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCDC�qDDqDD�qDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^�D^�qD_qD_�qD`D`�DaDa�qDbDb�qDcqDc�qDdqDd�DeqDe�qDfqDf��Dg�Dg��Dh�Dh�qDiqDi�qDjDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDo�Do�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuqDu�qDvqDv�qDwqDw�qDw�>Dy�RD�IGD��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�?}A�M�A�VA�VA�XA�VA�VA�VA�Q�A�K�A�I�A�I�A�=qA�5?A�+A��A���A��A��AɮA�jA�A���A�AȮAȑhA�t�A�I�A��TA���A���A�x�A�O�A���A�A�A�
=A�ffA�~�A��A��`A�1'A��HA���A��yA���A�p�A��/A�1'A��!A�|�A���A���A��TA�I�A���A�bNA���A��wA�A� �A��\A��TA�?}A��A���A�I�A��TA��\A�hsA�VA�jA�^5A���A��wA�"�A���A�r�A�bA��FA��A�VA���A��7A�ZA�l�A�|�A��yA��;A��A��/A�;dA��!A��#A�1A�=qA�\)A���A�TA|��Ay"�AwO�ArjAol�AnĜAnM�Am��Al�/Ai��AfZAb^5A]�TA[�AZ �AY
=AVjAS|�AQ�AQ�-AP��AL=qAJ�jAJ$�AI�AHv�AG�AE�AE|�AE
=AD^5AC;dAB��AA��A>v�A<��A<JA;l�A9hsA81A6��A6  A5��A5hsA4��A3/A21A0�A/��A/%A.9XA,�HA+p�A*��A*ZA* �A)�mA)��A)7LA(v�A'l�A&��A&$�A#�^A"M�A!��A Q�AK�AAS�A�/A�AS�A�A�A(�AK�A^5A�;A��A�wA��A�uA�+A=qA�A��AƨA�\A�RAhsAZA��A
r�A	��A	|�A	�A��A�RAVAE�A5?A-A1Ap�A�A7LA��AC�A�uA�A�-Ax�A7LA �yA �uA   @�o@�ȴ@�X@�-@�A�@�=q@�7L@�K�@�@�@�$�@�@�;d@�-@��@�P@��@�K�@�^5@䛦@�o@�x�@��@���@��@���@�o@ާ�@۾w@ٙ�@�7L@��@ӕ�@��@���@�o@�~�@�$�@ѡ�@�V@мj@�9X@�33@�E�@�`B@�?}@�7L@�7L@�p�@���@���@�33@�M�@�@�`B@�9X@��
@ǥ�@�K�@�
=@Ɵ�@Ų-@�Q�@ÍP@¸R@+@�-@��@�hs@�`B@��/@���@���@�Ĝ@��D@���@��m@�C�@�Ĝ@���@�X@�z�@��@�
=@���@�33@�+@���@�^5@��!@���@��+@�v�@�M�@�@�(�@��H@�$�@��@��@� �@�|�@�"�@�M�@���@�x�@�Ĝ@�1'@���@�l�@�\)@�dZ@��P@�ƨ@��P@�@��+@�ff@��@���@���@��-@�7L@��@���@�(�@��@�C�@��R@���@�V@���@���@��7@�X@�&�@�/@�7L@�&�@��9@��@��@�C�@�n�@�^5@�^5@�V@�@��D@�ƨ@��@�t�@��@��w@��\@�@���@�z�@��F@��@��H@��@���@���@�=q@�p�@���@�bN@� �@��w@�C�@��y@���@���@�=q@��!@�z�@�&�@��`@��@��@��@��w@���@�|�@�\)@�C�@�+@�"�@�o@�ȴ@�ff@�=q@��@��^@�p�@���@���@�Ĝ@��@��j@���@��@��@��
@��@�t�@�S�@�;d@��@�o@���@��y@��@���@�^5@�{@��#@��-@���@�p�@�`B@�`B@�7L@�%@��@�bN@�9X@�(�@�b@�  @���@���@��m@�ƨ@���@�\)@�o@��R@�E�@��@�@��-@���@��#@��@���@���@�G�@�V@���@�j@�1'@�b@��;@��w@��@�.I@u�@d�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�M�A�VA�VA�XA�VA�VA�VA�Q�A�K�A�I�A�I�A�=qA�5?A�+A��A���A��A��AɮA�jA�A���A�AȮAȑhA�t�A�I�A��TA���A���A�x�A�O�A���A�A�A�
=A�ffA�~�A��A��`A�1'A��HA���A��yA���A�p�A��/A�1'A��!A�|�A���A���A��TA�I�A���A�bNA���A��wA�A� �A��\A��TA�?}A��A���A�I�A��TA��\A�hsA�VA�jA�^5A���A��wA�"�A���A�r�A�bA��FA��A�VA���A��7A�ZA�l�A�|�A��yA��;A��A��/A�;dA��!A��#A�1A�=qA�\)A���A�TA|��Ay"�AwO�ArjAol�AnĜAnM�Am��Al�/Ai��AfZAb^5A]�TA[�AZ �AY
=AVjAS|�AQ�AQ�-AP��AL=qAJ�jAJ$�AI�AHv�AG�AE�AE|�AE
=AD^5AC;dAB��AA��A>v�A<��A<JA;l�A9hsA81A6��A6  A5��A5hsA4��A3/A21A0�A/��A/%A.9XA,�HA+p�A*��A*ZA* �A)�mA)��A)7LA(v�A'l�A&��A&$�A#�^A"M�A!��A Q�AK�AAS�A�/A�AS�A�A�A(�AK�A^5A�;A��A�wA��A�uA�+A=qA�A��AƨA�\A�RAhsAZA��A
r�A	��A	|�A	�A��A�RAVAE�A5?A-A1Ap�A�A7LA��AC�A�uA�A�-Ax�A7LA �yA �uA   @�o@�ȴ@�X@�-@�A�@�=q@�7L@�K�@�@�@�$�@�@�;d@�-@��@�P@��@�K�@�^5@䛦@�o@�x�@��@���@��@���@�o@ާ�@۾w@ٙ�@�7L@��@ӕ�@��@���@�o@�~�@�$�@ѡ�@�V@мj@�9X@�33@�E�@�`B@�?}@�7L@�7L@�p�@���@���@�33@�M�@�@�`B@�9X@��
@ǥ�@�K�@�
=@Ɵ�@Ų-@�Q�@ÍP@¸R@+@�-@��@�hs@�`B@��/@���@���@�Ĝ@��D@���@��m@�C�@�Ĝ@���@�X@�z�@��@�
=@���@�33@�+@���@�^5@��!@���@��+@�v�@�M�@�@�(�@��H@�$�@��@��@� �@�|�@�"�@�M�@���@�x�@�Ĝ@�1'@���@�l�@�\)@�dZ@��P@�ƨ@��P@�@��+@�ff@��@���@���@��-@�7L@��@���@�(�@��@�C�@��R@���@�V@���@���@��7@�X@�&�@�/@�7L@�&�@��9@��@��@�C�@�n�@�^5@�^5@�V@�@��D@�ƨ@��@�t�@��@��w@��\@�@���@�z�@��F@��@��H@��@���@���@�=q@�p�@���@�bN@� �@��w@�C�@��y@���@���@�=q@��!@�z�@�&�@��`@��@��@��@��w@���@�|�@�\)@�C�@�+@�"�@�o@�ȴ@�ff@�=q@��@��^@�p�@���@���@�Ĝ@��@��j@���@��@��@��
@��@�t�@�S�@�;d@��@�o@���@��y@��@���@�^5@�{@��#@��-@���@�p�@�`B@�`B@�7L@�%@��@�bN@�9X@�(�@�b@�  @���@���@��m@�ƨ@���@�\)@�o@��R@�E�@��@�@��-@���@��#@��@���@���@�G�@�V@���@�j@�1'@�b@��;@��w@��@�.I@u�@d�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B �B!�B!�B&�B)�B.B5?BA�BE�BG�BQ�B\)BiyBo�Bp�Br�Bt�Bw�By�B~�B�7B�\B�VB��B�;B\B �B!�B(�B7LBO�BZBm�BffBgmB�B�=B�bB�+B\)B�B��B��B��B��B��B��B��B��B��B��B�hB�DB�Bv�Bl�BhsBcTB`BB]/BXBO�BD�B/BB�5B��B��B�?B��Bp�B6FB�BbB
�B
�#B
�BB
��B
�XB
��B
�!B
�jB
��B
��B
�JB
z�B
m�B
`BB
G�B
/B
�B
B	�fB	�B	��B	��B	��B	ƨB	�dB	��B	�DB	k�B	YB	O�B	I�B	>wB	2-B	,B	)�B	$�B	�B	�B	�B	oB	bB	PB	
=B		7B	+B	B	  B��B��B�B�sB�`B�NB�)B�
B��B��B��B��BɺBÖB�}B�jB�RB�?B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�VB�=B�1B�B�B~�B}�B|�B{�Bz�By�Bx�Bv�Bu�Bt�Bt�Bz�B� B|�B|�B{�B�B�B�B�%B�=B�1B�=B�=B�DB�%B�B�B�+B�+B�+B�1B�7B�7B�7B�1B�1B�B�B�B�1B�7B�=B�DB�=B�DB�DB�PB�bB�hB�bB�JB�+B�B�B�B~�B�B�7B�PB�{B��B�{B�{B��B�oB�bB�hB�\B�=B�+B�7B�JB�JB�JB�=B�+B�B|�B}�B~�B{�B�B�JB�7B�7B�1B�=B�DB�JB�bB�uB��B��B��B��B��B��B�B�3B�LB�RB�XB�RB�dB�jB�qB�wB�wB�}B��B��BÖB��B��B��B�B�#B�)B�BB�HB�HB�HB�HB�TB�ZB�ZB�BB�B��B��B��B��B�B�#B�HB�fB�yB�B��B��B��B��B	B	
=B	JB	VB	{B	�B	�B	�B	�B	"�B	$�B	'�B	+B	.B	1'B	33B	49B	6FB	9XB	A�B	E�B	H�B	J�B	K�B	M�B	R�B	S�B	XB	]/B	]/B	]/B	`BB	bNB	dZB	ffB	ffB	hsB	k�B	m�B	m�B	n�B	p�B	t�B	v�B	w�B	{�B	� B	�B	�B	�B	�B	�+B	�1B	�1B	�%B	�+B	�1B	�=B	�PB	�VB	�PB	�PB	�PB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B#�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�HB	�HB	�HB	�TB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B
PB

�B
#�B
($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B �B!�B!�B&�B)�B.B5?BA�BE�BG�BQ�B\)BiyBo�Bp�Br�Bt�Bw�By�B~�B�7B�\B�VB��B�;B\B �B!�B(�B7LBO�BZBm�BffBgmB�B�=B�bB�+B\)B�B��B��B��B��B��B��B��B��B��B��B�hB�DB�Bv�Bl�BhsBcTB`BB]/BXBO�BD�B/BB�5B��B��B�?B��Bp�B6FB�BbB
�B
�#B
�BB
��B
�XB
��B
�!B
�jB
��B
��B
�JB
z�B
m�B
`BB
G�B
/B
�B
B	�fB	�B	��B	��B	��B	ƨB	�dB	��B	�DB	k�B	YB	O�B	I�B	>wB	2-B	,B	)�B	$�B	�B	�B	�B	oB	bB	PB	
=B		7B	+B	B	  B��B��B�B�sB�`B�NB�)B�
B��B��B��B��BɺBÖB�}B�jB�RB�?B�-B�B��B��B��B��B��B��B��B��B��B��B�{B�VB�=B�1B�B�B~�B}�B|�B{�Bz�By�Bx�Bv�Bu�Bt�Bt�Bz�B� B|�B|�B{�B�B�B�B�%B�=B�1B�=B�=B�DB�%B�B�B�+B�+B�+B�1B�7B�7B�7B�1B�1B�B�B�B�1B�7B�=B�DB�=B�DB�DB�PB�bB�hB�bB�JB�+B�B�B�B~�B�B�7B�PB�{B��B�{B�{B��B�oB�bB�hB�\B�=B�+B�7B�JB�JB�JB�=B�+B�B|�B}�B~�B{�B�B�JB�7B�7B�1B�=B�DB�JB�bB�uB��B��B��B��B��B��B�B�3B�LB�RB�XB�RB�dB�jB�qB�wB�wB�}B��B��BÖB��B��B��B�B�#B�)B�BB�HB�HB�HB�HB�TB�ZB�ZB�BB�B��B��B��B��B�B�#B�HB�fB�yB�B��B��B��B��B	B	
=B	JB	VB	{B	�B	�B	�B	�B	"�B	$�B	'�B	+B	.B	1'B	33B	49B	6FB	9XB	A�B	E�B	H�B	J�B	K�B	M�B	R�B	S�B	XB	]/B	]/B	]/B	`BB	bNB	dZB	ffB	ffB	hsB	k�B	m�B	m�B	n�B	p�B	t�B	v�B	w�B	{�B	� B	�B	�B	�B	�B	�+B	�1B	�1B	�%B	�+B	�1B	�=B	�PB	�VB	�PB	�PB	�PB	�VB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B#�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�/B	�/B	�5B	�5B	�;B	�HB	�HB	�HB	�TB	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B
+B
1B
	7B
	7B
	7B

=B

=B
PB

�B
#�B
($1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140849                              AO  ARCAADJP                                                                    20181024140849    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140849  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140849  QCF$                G�O�G�O�G�O�0               