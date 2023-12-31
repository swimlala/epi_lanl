CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-15T06:42:33Z creation;2022-07-15T06:42:34Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220715064233  20220715070253  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @��w=��1   @��y���@3<j~��#�dfM���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&�C(�C*  C+�fC.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  D   D � D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQy�DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZy�DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�3D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D��3D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�<�DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D���D�<�Dƀ D�� D�  D�@ Dǃ3D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ Dμ�D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�  D�@ DҀ D�� D�3D�C3DӀ D�� D�3D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�|�D��D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�|�D��D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
B�
B!�
B)�
B1�
B9�
BA�
BI�
BQ�
BY�
Ba�
Bi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B̸RB��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�C�]Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&�]C(�]C*u�C,\)C.u�C0u�C2\)C4u�C6u�C8u�C:u�C<u�C>u�C@u�CB�]CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cp�]Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�G�C�:�D qD �qDqD�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"#�D"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�D+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�D7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=#�D=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�DJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQDQ�DRqDR�qDSqDS��DT#�DT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZDZ�D[D[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�DbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDg#�Dg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDp#�Dp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuqDu��DvqDv�qDwqDw�qDxqDx�qDyqDy�qDzqDz�qD{#�D{�qD|qD|�qD}qD}�qD~qD~�qDqD�qD��D�N�D���D�θD��D�N�D���D�˅D��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�K�D���D�θD��D�K�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�˅D��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�˅D��D�N�D���D�θD��D�Q�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�K�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D���D��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�K�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�Q�D���D�θD��D�N�D���D�θD��D�K�D���D�θD��D�N�D���D�θD��D�Q�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D���D��D�N�D���D�θD��D�N�D���D�θD��D�N�D�D�θD��D�K�DÎ�D�θD��D�N�DĎ�D�θD��D�N�DŎ�D�θD��D�K�DƎ�D�θD��D�N�DǑ�D���D��D�N�DȎ�D�θD��D�N�DɎ�D�θD��D�N�Dʎ�D�θD��D�N�Dˎ�D�θD��D�N�D̎�D�θD��D�N�D͎�D�θD��D�N�DΎ�D�˅D��D�N�Dώ�D�θD��D�N�DЎ�D�θD��D�N�Dю�D���D��D�N�DҎ�D�θD��D�Q�Dӎ�D�θD��D�N�DԎ�D�θD��D�N�DՎ�D�θD��D�N�D֎�D�θD��D�N�D׎�D�θD��D�Q�D؎�D�θD��D�N�Dَ�D�θD��D�N�Dڎ�D�θD��D�N�Dێ�D�θD��D�N�D܎�D�θD��D�N�Dݎ�D�θD��D�Q�Dޑ�D�θD��D�N�Dߎ�D�θD��D�N�D���D�θD��D�N�DᎸD�θD��D�N�D⎸D�θD��D�N�D㎸D�θD��D�N�D䎸D�θD��D�N�D��D�θD��D�N�D��D���D��D�N�D玸D�θD��D�N�D莸D�θD��D�N�D鎸D�θD��D�N�DꎸD�θD��D�N�D뎸D�θD��D�N�D쎸D�θD��D�N�D편D�θD��D�N�DD�˅D��D�N�DD�θD��D�N�D���D�θD��D�N�D�D�˅D��D�K�D�D�θD��D�N�D�D�θD��D�N�D�D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�N�D���D�θD��D�>�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�K�A�I�A�?}A�?}A�;dA�A�A�C�A�9XA�5?A�5?A�5?A�5?A�5?A�7LA�5?A�5?A�33A�33A�33A�33A�1'A�/A�$�A�VA�A��#A�XA�$�A��A��A�bNA��/A�oAͮA�K�A�1A��A�ĜA̋DA�M�A�"�A�"�A�A���A�jA�?}A�+A�&�A�  A��yA��Aʲ-Aʥ�Aʲ-A�ĜA���AʶFAʃA�;dA�{A���A���A�\)A�1'A��A���AȮA�S�A��A�O�A��A�/A���AŋDAĶFA�p�A���A�1A�1A�jA�bNA��TA��uA���A���A��FA��A���A��A�v�A��HA��+A��A�-A��mA���A�bA��HA�VA�|�A��;A�7LA��A�I�A�jA��+A�1'A�bA�x�A�  A�E�A��RA�A��A�VA�A��A��A��A��9A���A�ȴA�r�A��A���A��^A�VA�S�A���A���A�A��!A��yA���A�JA{%Au�FAq�An�AnAmS�Aj�AfĜAb1'A`5?A_hsA^�jA]��A\�\AZ$�AUASS�AR�RAPVAL �AJE�AI"�AG�#AF�`AE�FAC�;ABQ�AAoA@JA=��A<(�A9�;A6�\A5��A4��A3��A2z�A0�HA.�\A-7LA+��A+oA*=qA)��A)|�A(�!A&�!A%XA#+A!�FA!S�A Q�A��A��A�PA�A�A�An�A�A�yA�^A�AȴA1A+AdZA�A��AA�A"�A  A�A33AA�A~�A�;A�AJA
{A	�PA	�Az�AE�AJA/A  A|�A"�AM�A��A|�A�A�hA�A  A�A ȴA ~�A b@�t�@��-@���@�+@�J@�(�@�E�@��@��@�$�@��@���@�\)@���@�V@�=q@�=q@�J@�@�j@�p�@�t�@��T@�R@�X@��/@��@�ȴ@��@�R@㕁@�l�@�
=@�$�@���@�1@�j@�o@�E�@�`B@�I�@���@ְ!@�^5@�x�@��`@ԛ�@ӶF@�t�@�+@ҟ�@с@д9@�1'@�\)@ΰ!@�ff@�{@ͺ^@͉7@��@�ƨ@�33@ʏ\@�V@�=q@�{@ɉ7@�z�@�\)@�~�@��@š�@�hs@�O�@�?}@��@ă@���@��H@�-@��-@�O�@��@���@���@�z�@�bN@�Q�@�S�@��\@�n�@�V@�E�@��@��@��@�%@��u@�I�@��
@��@��@��@�dZ@��@���@���@��\@��@���@�&�@��@��@�Z@�1'@���@�S�@�@���@�ff@�-@�@�X@��@��;@�+@���@��@��@���@��@�O�@�?}@�V@��j@�j@��P@�C�@�ȴ@�n�@���@�x�@�?}@��@���@�I�@� �@��F@�l�@�33@���@���@��T@�p�@�O�@�?}@�&�@��/@��
@�@�v�@��@�%@�9X@��;@���@��/@�1@�@�I�@���@�5?@�x�@���@���@��9@���@�A�@���@���@�33@��@��@��H@�K�@��w@��@�l�@���@�n�@�=q@��@��7@��@�j@�1'@��m@��P@�o@���@��+@�n�@�^5@�-@�@��T@�p�@�7L@��@�%@��`@���@��u@�9X@��m@��
@���@��P@��P@�t�@�\)@�33@���@���@�ȴ@���@��!@�=q@�`B@�9X@��P@�o@��@��H@��!@�n�@�M�@���@���@�J@�@���@��^@���@�hs@�G�@�%@��@�b@��;@�dZ@�\)@��@�ȴ@�-@��7@�Ĝ@�Z@��@��m@���@��F@��m@�b@�9X@��@��
@�ƨ@��P@�+@���@��T@�J@���@��#@�x�@�G�@�&�@��/@���@��@� �@��@��m@��w@��m@�ƨ@��P@�t�@�33@�o@��y@��!@�E�@�@��T@���@�7L@��@���@�Z@�(�@�w@\)@
=@~�@~ff@~5?@~@}`B@|�/@|�D@|j@|9X@{C�@z�!@zM�@y��@y�7@y7L@x�`@x�u@w�;@w�P@w
=@v�R@vff@v$�@v@u��@u@u`B@t�@t�j@t�j@t��@s�F@so@r��@r�\@rn�@r^5@rM�@r=q@r-@q��@q�#@q�^@q�7@qX@qG�@p��@p��@p�@pA�@p  @o�@o\)@n��@nV@nE�@n$�@n{@m�T@m��@mO�@l�@l��@l�@lj@lZ@k�m@kdZ@j��@jM�@i��@i��@iX@i7L@i%@h�9@h�@hb@g�w@g|�@g
=@fȴ@fv�@f{@e�@e@e�@e�@d��@d�@dZ@d1@c��@ct�@ct�@c"�@b�H@bM�@a�@a��@a7L@`Ĝ@`�u@`A�@`  @_�P@_;d@^�y@^V@^$�@^{@^{@]�@]�h@]/@\��@\��@\I�@\1@[�F@[��@[dZ@[33@[o@Z�\@Y�#@Y�7@YG�@Y%@X��@X�9@Xr�@W�P@V��@V$�@U�T@U��@U`B@T�@T(�@St�@S@Rn�@R-@Q�#@Q��@Q7L@Q�@P��@PĜ@P�9@P��@P�@Pr�@PA�@O�w@O
=@N$�@N@M��@M�@M`B@M?}@M�@L��@LZ@L�@Kƨ@K��@KdZ@KdZ@KC�@J�@Jn�@J-@JJ@I��@I�@I��@I�7@IG�@I&�@H�`@HĜ@H�u@Hr�@HA�@H  @G�w@G\)@F�y@F�+@F5?@F@E@E�h@E�@E?}@E�@D�@D�j@DZ@D1@C�F@CC�@B�@B�!@B~�@B�@A�^@A%@@��@@�u@@b@?\)@>�y@>V@>E�@>5?@=�@=��@=O�@<��@<��@;�m@;C�@:��@:�\@:^5@:=q@:-@9��@9hs@9�@8��@8��@8�u@7�;@7;d@6�R@65?@6@5�@5��@5`B@5/@4�@4�D@4(�@4(�@4�@3��@3�
@3��@3S�@2�H@2^5@2�@1��@1��@1X@0��@0bN@0A�@/�@/\)@/�@.�R@.ff@.5?@-�T@-�@-`B@-/@,�/@,�j@,j@,(�@,�@+�
@+t�@+C�@+o@*��@*�\@*M�@*J@)�#@)�7@(��@(��@(��@(r�@(A�@'�;@'�P@'l�@';d@&�y@&�+@&V@&$�@&@%@%��@%�h@%`B@$��@$z�@$1@#�F@#dZ@#"�@"��@"M�@"�@!�@!x�@!7L@ �`@ �9@ �@ bN@ A�@   @�P@+@�@�@
=@�y@v�@5?@$�@{@�@��@p�@?}@�@��@�D@I�@1@�m@�F@dZ@33@@�H@��@��@=q@��@x�@x�@X@%@Ĝ@�u@�u@�u@�u@Q�@�@�w@|�@ȴ@v�@E�@5?@$�@$�@�@�-@�@p�@O�@O�@��@�/@�@z�@9X@1@�m@�F@��@t�@C�@@��@��@�@�#@��@G�@%@�`@Ĝ@�9@��@�u@r�@1'@  @�@��@��@�P@\)@;d@;d@
=@
=@�y@��@E�@{@�@�T@@��@�@p�@`B@O�@/@/@�@��@��@I�@�@�
@��@��@��@t�@dZ@33@"�@o@@@
�@
�@@
�H@
��@
��@
�\@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�K�A�K�A�I�A�?}A�?}A�;dA�A�A�C�A�9XA�5?A�5?A�5?A�5?A�5?A�7LA�5?A�5?A�33A�33A�33A�33A�1'A�/A�$�A�VA�A��#A�XA�$�A��A��A�bNA��/A�oAͮA�K�A�1A��A�ĜA̋DA�M�A�"�A�"�A�A���A�jA�?}A�+A�&�A�  A��yA��Aʲ-Aʥ�Aʲ-A�ĜA���AʶFAʃA�;dA�{A���A���A�\)A�1'A��A���AȮA�S�A��A�O�A��A�/A���AŋDAĶFA�p�A���A�1A�1A�jA�bNA��TA��uA���A���A��FA��A���A��A�v�A��HA��+A��A�-A��mA���A�bA��HA�VA�|�A��;A�7LA��A�I�A�jA��+A�1'A�bA�x�A�  A�E�A��RA�A��A�VA�A��A��A��A��9A���A�ȴA�r�A��A���A��^A�VA�S�A���A���A�A��!A��yA���A�JA{%Au�FAq�An�AnAmS�Aj�AfĜAb1'A`5?A_hsA^�jA]��A\�\AZ$�AUASS�AR�RAPVAL �AJE�AI"�AG�#AF�`AE�FAC�;ABQ�AAoA@JA=��A<(�A9�;A6�\A5��A4��A3��A2z�A0�HA.�\A-7LA+��A+oA*=qA)��A)|�A(�!A&�!A%XA#+A!�FA!S�A Q�A��A��A�PA�A�A�An�A�A�yA�^A�AȴA1A+AdZA�A��AA�A"�A  A�A33AA�A~�A�;A�AJA
{A	�PA	�Az�AE�AJA/A  A|�A"�AM�A��A|�A�A�hA�A  A�A ȴA ~�A b@�t�@��-@���@�+@�J@�(�@�E�@��@��@�$�@��@���@�\)@���@�V@�=q@�=q@�J@�@�j@�p�@�t�@��T@�R@�X@��/@��@�ȴ@��@�R@㕁@�l�@�
=@�$�@���@�1@�j@�o@�E�@�`B@�I�@���@ְ!@�^5@�x�@��`@ԛ�@ӶF@�t�@�+@ҟ�@с@д9@�1'@�\)@ΰ!@�ff@�{@ͺ^@͉7@��@�ƨ@�33@ʏ\@�V@�=q@�{@ɉ7@�z�@�\)@�~�@��@š�@�hs@�O�@�?}@��@ă@���@��H@�-@��-@�O�@��@���@���@�z�@�bN@�Q�@�S�@��\@�n�@�V@�E�@��@��@��@�%@��u@�I�@��
@��@��@��@�dZ@��@���@���@��\@��@���@�&�@��@��@�Z@�1'@���@�S�@�@���@�ff@�-@�@�X@��@��;@�+@���@��@��@���@��@�O�@�?}@�V@��j@�j@��P@�C�@�ȴ@�n�@���@�x�@�?}@��@���@�I�@� �@��F@�l�@�33@���@���@��T@�p�@�O�@�?}@�&�@��/@��
@�@�v�@��@�%@�9X@��;@���@��/@�1@�@�I�@���@�5?@�x�@���@���@��9@���@�A�@���@���@�33@��@��@��H@�K�@��w@��@�l�@���@�n�@�=q@��@��7@��@�j@�1'@��m@��P@�o@���@��+@�n�@�^5@�-@�@��T@�p�@�7L@��@�%@��`@���@��u@�9X@��m@��
@���@��P@��P@�t�@�\)@�33@���@���@�ȴ@���@��!@�=q@�`B@�9X@��P@�o@��@��H@��!@�n�@�M�@���@���@�J@�@���@��^@���@�hs@�G�@�%@��@�b@��;@�dZ@�\)@��@�ȴ@�-@��7@�Ĝ@�Z@��@��m@���@��F@��m@�b@�9X@��@��
@�ƨ@��P@�+@���@��T@�J@���@��#@�x�@�G�@�&�@��/@���@��@� �@��@��m@��w@��m@�ƨ@��P@�t�@�33@�o@��y@��!@�E�@�@��T@���@�7L@��@���@�Z@�(�@�w@\)@
=@~�@~ff@~5?@~@}`B@|�/@|�D@|j@|9X@{C�@z�!@zM�@y��@y�7@y7L@x�`@x�u@w�;@w�P@w
=@v�R@vff@v$�@v@u��@u@u`B@t�@t�j@t�j@t��@s�F@so@r��@r�\@rn�@r^5@rM�@r=q@r-@q��@q�#@q�^@q�7@qX@qG�@p��@p��@p�@pA�@p  @o�@o\)@n��@nV@nE�@n$�@n{@m�T@m��@mO�@l�@l��@l�@lj@lZ@k�m@kdZ@j��@jM�@i��@i��@iX@i7L@i%@h�9@h�@hb@g�w@g|�@g
=@fȴ@fv�@f{@e�@e@e�@e�@d��@d�@dZ@d1@c��@ct�@ct�@c"�@b�H@bM�@a�@a��@a7L@`Ĝ@`�u@`A�@`  @_�P@_;d@^�y@^V@^$�@^{@^{@]�@]�h@]/@\��@\��@\I�@\1@[�F@[��@[dZ@[33@[o@Z�\@Y�#@Y�7@YG�@Y%@X��@X�9@Xr�@W�P@V��@V$�@U�T@U��@U`B@T�@T(�@St�@S@Rn�@R-@Q�#@Q��@Q7L@Q�@P��@PĜ@P�9@P��@P�@Pr�@PA�@O�w@O
=@N$�@N@M��@M�@M`B@M?}@M�@L��@LZ@L�@Kƨ@K��@KdZ@KdZ@KC�@J�@Jn�@J-@JJ@I��@I�@I��@I�7@IG�@I&�@H�`@HĜ@H�u@Hr�@HA�@H  @G�w@G\)@F�y@F�+@F5?@F@E@E�h@E�@E?}@E�@D�@D�j@DZ@D1@C�F@CC�@B�@B�!@B~�@B�@A�^@A%@@��@@�u@@b@?\)@>�y@>V@>E�@>5?@=�@=��@=O�@<��@<��@;�m@;C�@:��@:�\@:^5@:=q@:-@9��@9hs@9�@8��@8��@8�u@7�;@7;d@6�R@65?@6@5�@5��@5`B@5/@4�@4�D@4(�@4(�@4�@3��@3�
@3��@3S�@2�H@2^5@2�@1��@1��@1X@0��@0bN@0A�@/�@/\)@/�@.�R@.ff@.5?@-�T@-�@-`B@-/@,�/@,�j@,j@,(�@,�@+�
@+t�@+C�@+o@*��@*�\@*M�@*J@)�#@)�7@(��@(��@(��@(r�@(A�@'�;@'�P@'l�@';d@&�y@&�+@&V@&$�@&@%@%��@%�h@%`B@$��@$z�@$1@#�F@#dZ@#"�@"��@"M�@"�@!�@!x�@!7L@ �`@ �9@ �@ bN@ A�@   @�P@+@�@�@
=@�y@v�@5?@$�@{@�@��@p�@?}@�@��@�D@I�@1@�m@�F@dZ@33@@�H@��@��@=q@��@x�@x�@X@%@Ĝ@�u@�u@�u@�u@Q�@�@�w@|�@ȴ@v�@E�@5?@$�@$�@�@�-@�@p�@O�@O�@��@�/@�@z�@9X@1@�m@�F@��@t�@C�@@��@��@�@�#@��@G�@%@�`@Ĝ@�9@��@�u@r�@1'@  @�@��@��@�P@\)@;d@;d@
=@
=@�y@��@E�@{@�@�T@@��@�@p�@`B@O�@/@/@�@��@��@I�@�@�
@��@��@��@t�@dZ@33@"�@o@@@
�@
�@@
�H@
��@
��@
�\@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�PB
�=B
�%B
z�B
e`B
F�B
;dB
<jB
=qB
<jB
<jB
=qB
>wB
?}B
B�B
I�B
O�B
O�B
R�B
W
B
_;B
hsB
l�B
jB
hsB
gmB
ffB
jB
r�B
y�B
~�B
�=B
��B
��B
�-B
B
�/B
�sB
�B
�B%B�B,B?}BE�BW
BhsBu�B�oB�3BB��B�NB�B��BB
=BuB$�B(�B(�B)�B)�B.B1'B2-B/B&�B"�B$�B�B#�B$�B�B�B{BVBB��B�B�B�HB�B�
B��BɺBĜB�LB��B��B�BffB`BBXBH�B8RB5?B�BB
�
B
�dB
��B
�B
`BB
D�B
33B
'�B
#�B
�B
  B	�;B	ƨB	�?B	�B	��B	��B	�PB	r�B	hsB	gmB	ffB	cTB	ZB	J�B	7LB	+B	'�B	%�B	�B	DB	+B	  B��B�B�B�B�sB�fB�TB�5B�/B�B��B��B��BȴBŢBĜB��B��B��B��B��B��BBÖBĜBȴB��B��B��B�)B�;B�;B�HB�HB�TB�fB�B�B		7B	!�B	49B	6FB	9XB	33B	+B	&�B	&�B	"�B	"�B	!�B	 �B	�B	uB	VB	+B	B��B��B��B��B��B	B	B	B	B	%B	%B	B		7B	JB	�B	�B	$�B	%�B	)�B	,B	33B	6FB	8RB	>wB	A�B	B�B	D�B	B�B	A�B	;dB	;dB	8RB	:^B	9XB	<jB	<jB	<jB	>wB	A�B	C�B	C�B	C�B	M�B	K�B	E�B	D�B	@�B	?}B	@�B	?}B	>wB	I�B	T�B	T�B	Q�B	D�B	<jB	;dB	B�B	F�B	F�B	E�B	C�B	B�B	A�B	B�B	B�B	@�B	@�B	@�B	B�B	D�B	E�B	G�B	G�B	I�B	I�B	K�B	M�B	L�B	J�B	I�B	G�B	E�B	C�B	C�B	C�B	D�B	D�B	F�B	G�B	M�B	P�B	T�B	XB	XB	XB	YB	ZB	^5B	bNB	iyB	l�B	o�B	q�B	r�B	v�B	x�B	z�B	|�B	|�B	�B	�+B	�+B	�1B	�1B	�7B	�7B	�JB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�LB	�XB	�dB	�wB	��B	��B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�
B	��B	�B	�/B	�;B	�HB	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�;B	�`B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
1B

=B

=B
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
VB
PB
PB
VB
VB
VB
\B
\B
VB
PB
	7B
+B
%B
%B
B
B
B
B
%B
%B
1B

=B

=B
PB
VB
\B
VB
VB
VB
PB
bB
bB
oB
oB
oB
uB
{B
bB
\B
\B
\B
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
$�B
$�B
$�B
%�B
%�B
'�B
(�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�PB
�=B
�%B
z�B
e`B
F�B
;dB
<jB
=qB
<jB
<jB
=qB
>wB
?}B
B�B
I�B
O�B
O�B
R�B
W
B
_;B
hsB
l�B
jB
hsB
gmB
ffB
jB
r�B
y�B
~�B
�=B
��B
��B
�-B
B
�/B
�sB
�B
�B%B�B,B?}BE�BW
BhsBu�B�oB�3BB��B�NB�B��BB
=BuB$�B(�B(�B)�B)�B.B1'B2-B/B&�B"�B$�B�B#�B$�B�B�B{BVBB��B�B�B�HB�B�
B��BɺBĜB�LB��B��B�BffB`BBXBH�B8RB5?B�BB
�
B
�dB
��B
�B
`BB
D�B
33B
'�B
#�B
�B
  B	�;B	ƨB	�?B	�B	��B	��B	�PB	r�B	hsB	gmB	ffB	cTB	ZB	J�B	7LB	+B	'�B	%�B	�B	DB	+B	  B��B�B�B�B�sB�fB�TB�5B�/B�B��B��B��BȴBŢBĜB��B��B��B��B��B��BBÖBĜBȴB��B��B��B�)B�;B�;B�HB�HB�TB�fB�B�B		7B	!�B	49B	6FB	9XB	33B	+B	&�B	&�B	"�B	"�B	!�B	 �B	�B	uB	VB	+B	B��B��B��B��B��B	B	B	B	B	%B	%B	B		7B	JB	�B	�B	$�B	%�B	)�B	,B	33B	6FB	8RB	>wB	A�B	B�B	D�B	B�B	A�B	;dB	;dB	8RB	:^B	9XB	<jB	<jB	<jB	>wB	A�B	C�B	C�B	C�B	M�B	K�B	E�B	D�B	@�B	?}B	@�B	?}B	>wB	I�B	T�B	T�B	Q�B	D�B	<jB	;dB	B�B	F�B	F�B	E�B	C�B	B�B	A�B	B�B	B�B	@�B	@�B	@�B	B�B	D�B	E�B	G�B	G�B	I�B	I�B	K�B	M�B	L�B	J�B	I�B	G�B	E�B	C�B	C�B	C�B	D�B	D�B	F�B	G�B	M�B	P�B	T�B	XB	XB	XB	YB	ZB	^5B	bNB	iyB	l�B	o�B	q�B	r�B	v�B	x�B	z�B	|�B	|�B	�B	�+B	�+B	�1B	�1B	�7B	�7B	�JB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�LB	�XB	�dB	�wB	��B	��B	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�
B	��B	�B	�/B	�;B	�HB	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�;B	�`B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
1B

=B

=B
DB
JB
JB
JB
JB
JB
JB
JB
PB
PB
PB
VB
PB
PB
VB
VB
VB
\B
\B
VB
PB
	7B
+B
%B
%B
B
B
B
B
%B
%B
1B

=B

=B
PB
VB
\B
VB
VB
VB
PB
bB
bB
oB
oB
oB
uB
{B
bB
\B
\B
\B
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
$�B
$�B
$�B
%�B
%�B
'�B
(�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
-B
-B
.B
.B
/B
/B
/B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
gmB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20220715154138  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220715064233  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220715064233  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220715064234                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220715154238  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220715154238  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220715070253                      G�O�G�O�G�O�                