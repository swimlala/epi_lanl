CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:35Z AOML 3.0 creation; 2016-08-07T21:51:14Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221435  20160807145114  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  5287_9017_032                   2C  D   APEX                            6529                            072314                          846 @�6W�t�1   @�6X`� @1��+J�d�|�hs1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D�3D�FfD���D��fD�  D�@ D���D��3D�fD�L�D�� D���D�  D�C3D�y�D��fD���D��D�VfD�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
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
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
�]Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@�]CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDt�qDyФD��D�UD��RD��D��D�N�D���D���D�D�[�D���D�ۅD��D�Q�DڈRD��D���D�+�D�eD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A̡�A̙�A̅A��A��`A���A˸RA˰!A˟�Aˣ�AˬA˰!A˸RA���A� �A�r�A̋DA�~�A�bNA�33A˴9A�^5A�O�A��A�A�  A��#Aʕ�Aʉ7Aʇ+A�dZA�7LA���A��#Aɩ�A�r�A�^5A�I�A�/A���AȾwAȟ�AȍPA�+A��HA���Aǲ-AǇ+A�&�A�z�A��#AŬAŗ�A�v�A�dZA�ZA��A��A��#AĴ9AāA�l�A�S�A�Q�A��A�ȴA�1'A�-A���A��A��FA�"�A�bNA��A��
A���A��hA��`A�`BA��^A��#A�$�A�dZA���A���A��wA�oA�ȴA�C�A��jA�33A��mA���A��A�33A��\A�ȴA�VA�Q�A��yA�ffA��A�z�A��A�K�A���A�33A��jA�bNA��jA���A�O�A��A�JA�S�A�hsA�?}A��+A�A��A|ĜAy�Aw|�At��An��AhbNAc��A^�jAW�AU|�AS`BAQ\)AP��AP�AO�hAN��AM�FAL^5AI?}AF��AD9XA?��A>�+A>{A<�A:�+A8  A5��A3�wA3|�A2��A1��A0��A/��A.A�A,��A,�A+��A)��A(-A'�FA&ĜA%p�A#��A!x�A��A�!A$�A�AA��A��A�TAr�A �AS�A��A-A�!A�#A�
A�^A�hA�AbA7LA"�A�HA�!AbNA�#A
�A	�PA�Ar�Av�A��A�A�
Al�Ax�At�Ax�Ax�A\)AG�A"�A�AZA�A��AdZA ȴA A�@�
=@���@��@�@��-@��u@�K�@�V@�p�@�bN@�M�@��`@�Z@��;@�;d@�~�@��@�7L@�z�@��@��@�R@�v�@��@�/@�A�@�ƨ@�
=@�~�@�-@�/@���@�Z@�@�o@��H@�\@�$�@���@���@�/@� �@߶F@�"�@�v�@��@���@�p�@��@�9X@��@�|�@��H@�v�@�=q@١�@�p�@���@��@ج@؃@�A�@� �@�t�@���@�ff@թ�@��@�j@���@�|�@���@��@���@҇+@���@д9@Гu@Гu@Ѓ@�z�@��@��@Χ�@Ο�@�~�@�J@ͩ�@�7L@�Ĝ@�I�@���@˾w@˕�@�\)@���@�$�@ɲ-@���@��m@Ǯ@Ǖ�@�@��@Ƨ�@�=q@š�@Ĵ9@ēu@�Q�@��m@�C�@��@���@���@��@���@��j@��D@�A�@�\)@�
=@�@���@�E�@�@�hs@�7L@�V@�j@��@��@�"�@���@�n�@�5?@��@��@�&�@�Ĝ@�r�@��@��@��@��!@��+@�-@��@��7@���@���@�A�@��m@���@�|�@�"�@��R@�5?@��T@��h@�Ĝ@�Z@�1@��
@���@��@��@��P@�t�@�C�@�;d@�;d@�C�@�"�@���@��-@�&�@��@���@�z�@�(�@�C�@���@��!@�{@���@���@�G�@�Ĝ@��u@�Z@�  @��
@�\)@�"�@���@�M�@��#@��h@�x�@�p�@�O�@�&�@�V@��9@��@�j@�1@�;d@�$�@�&�@�A�@���@��P@��P@��P@�\)@�o@��@��!@���@��+@�^5@�@�p�@�?}@��j@�(�@� �@��m@��@��y@��R@���@��\@�ff@�V@�-@��T@�@�x�@�V@�Ĝ@���@�j@���@�dZ@���@��R@�^5@��^@�hs@�?}@��@��/@���@�j@�Z@�A�@�1'@� �@�  @��@�
=@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@��R@���@�v�@�M�@��F@���@��@�w@t�j@i��@`b@Z~�@Q��@G��@AG�@9�@4Z@/+@)%@#�
@�+@~�@�u@`B@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A̡�A̙�A̅A��A��`A���A˸RA˰!A˟�Aˣ�AˬA˰!A˸RA���A� �A�r�A̋DA�~�A�bNA�33A˴9A�^5A�O�A��A�A�  A��#Aʕ�Aʉ7Aʇ+A�dZA�7LA���A��#Aɩ�A�r�A�^5A�I�A�/A���AȾwAȟ�AȍPA�+A��HA���Aǲ-AǇ+A�&�A�z�A��#AŬAŗ�A�v�A�dZA�ZA��A��A��#AĴ9AāA�l�A�S�A�Q�A��A�ȴA�1'A�-A���A��A��FA�"�A�bNA��A��
A���A��hA��`A�`BA��^A��#A�$�A�dZA���A���A��wA�oA�ȴA�C�A��jA�33A��mA���A��A�33A��\A�ȴA�VA�Q�A��yA�ffA��A�z�A��A�K�A���A�33A��jA�bNA��jA���A�O�A��A�JA�S�A�hsA�?}A��+A�A��A|ĜAy�Aw|�At��An��AhbNAc��A^�jAW�AU|�AS`BAQ\)AP��AP�AO�hAN��AM�FAL^5AI?}AF��AD9XA?��A>�+A>{A<�A:�+A8  A5��A3�wA3|�A2��A1��A0��A/��A.A�A,��A,�A+��A)��A(-A'�FA&ĜA%p�A#��A!x�A��A�!A$�A�AA��A��A�TAr�A �AS�A��A-A�!A�#A�
A�^A�hA�AbA7LA"�A�HA�!AbNA�#A
�A	�PA�Ar�Av�A��A�A�
Al�Ax�At�Ax�Ax�A\)AG�A"�A�AZA�A��AdZA ȴA A�@�
=@���@��@�@��-@��u@�K�@�V@�p�@�bN@�M�@��`@�Z@��;@�;d@�~�@��@�7L@�z�@��@��@�R@�v�@��@�/@�A�@�ƨ@�
=@�~�@�-@�/@���@�Z@�@�o@��H@�\@�$�@���@���@�/@� �@߶F@�"�@�v�@��@���@�p�@��@�9X@��@�|�@��H@�v�@�=q@١�@�p�@���@��@ج@؃@�A�@� �@�t�@���@�ff@թ�@��@�j@���@�|�@���@��@���@҇+@���@д9@Гu@Гu@Ѓ@�z�@��@��@Χ�@Ο�@�~�@�J@ͩ�@�7L@�Ĝ@�I�@���@˾w@˕�@�\)@���@�$�@ɲ-@���@��m@Ǯ@Ǖ�@�@��@Ƨ�@�=q@š�@Ĵ9@ēu@�Q�@��m@�C�@��@���@���@��@���@��j@��D@�A�@�\)@�
=@�@���@�E�@�@�hs@�7L@�V@�j@��@��@�"�@���@�n�@�5?@��@��@�&�@�Ĝ@�r�@��@��@��@��!@��+@�-@��@��7@���@���@�A�@��m@���@�|�@�"�@��R@�5?@��T@��h@�Ĝ@�Z@�1@��
@���@��@��@��P@�t�@�C�@�;d@�;d@�C�@�"�@���@��-@�&�@��@���@�z�@�(�@�C�@���@��!@�{@���@���@�G�@�Ĝ@��u@�Z@�  @��
@�\)@�"�@���@�M�@��#@��h@�x�@�p�@�O�@�&�@�V@��9@��@�j@�1@�;d@�$�@�&�@�A�@���@��P@��P@��P@�\)@�o@��@��!@���@��+@�^5@�@�p�@�?}@��j@�(�@� �@��m@��@��y@��R@���@��\@�ff@�V@�-@��T@�@�x�@�V@�Ĝ@���@�j@���@�dZ@���@��R@�^5@��^@�hs@�?}@��@��/@���@�j@�Z@�A�@�1'@� �@�  @��@�
=@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@��R@���@�v�G�O�@��F@���@��@�w@t�j@i��@`b@Z~�@Q��@G��@AG�@9�@4Z@/+@)%@#�
@�+@~�@�u@`B@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��BB
=B\B�B�B!�B'�B1'B:^BN�B}�B�!B�B\B%�B8RBL�BXBl�Bt�Bu�Bu�Bu�By�B{�B|�B�B�1B�JB�PB�\B�\B�\B�oB��B��B��B��B��B�'B�LB�RB�dB�}BƨBĜBŢBƨBƨBȴB��B�TB�fB�ZB�TB�;B�/B�`B�B��B��B  B��B�yB�5B�
B�B��B��B�B�B�B��B��B��B��B��B�B��B�TB��B	7B�HB�B��B�DB�B�B�B�B|�Bs�BcTBo�Be`BZBVBQ�BE�B�B��B��B�Bw�Bt�BaHBB
��B
��B
�B
�bB
~�B
gmB
G�B
.B
�B

=B	�B	�HB	��B	��B	u�B	[#B	<jB	�B	\B	B��B��B��B��B��B��B�B�mB�HB�B��B��B��B��BǮBŢBĜBŢBĜBĜBŢBŢBĜBŢBŢBĜBÖBƨBƨBŢBŢBƨBƨBŢBÖBB��B�wB�^B�FB�-B��B�B�?B�?B�?B�LB�wB��B�B�B��B	DB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	'�B	$�B	)�B	)�B	)�B	.B	5?B	7LB	;dB	@�B	F�B	J�B	L�B	M�B	R�B	T�B	T�B	S�B	VB	VB	T�B	VB	W
B	XB	YB	YB	[#B	[#B	]/B	_;B	dZB	gmB	hsB	iyB	k�B	m�B	n�B	p�B	s�B	t�B	y�B	z�B	{�B	|�B	� B	�B	�B	�+B	�=B	�DB	�PB	�\B	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�RB	�XB	�^B	�^B	�^B	�^B	�^B	�^B	�dB	�jB	�qB	�}B	�}B	�}B	�wB	��B	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�BB	�BB	�BB	�BB	�;B	�;B	�BB	�BB	�;B	�;B	�;B	�;B	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�`B	�fB	�mB	�mB	�fB	�fB	�mB	�sB	�yB	�yB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B
JB
oB
�B
�B
%�B
+B
6FB
:^B
@�B
K�B
Q�B
XB
[#B
aHB
ffB
iyB
k�B
n�B
q�B
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B�B
B8BnB~B!�B'�B1B::BN�B}�B��B�B6B%�B8-BL�BW�BlhBt�Bu�Bu�Bu�By�B{�B|�B��B�B�%B�,B�7B�7B�8B�GB�ZB�iB��B��B��B� B�'B�2B�@B�XBƃB�tB�{BƂBƂBȐBͮB�/B�BB�4B�-B�B�B�;B�eB��B��B��B��B�VB�B��B�mB��B��B�B�eB��B��B��B��BͬB��B��B��B�.B��B	B� B��B�[B�B��B��B��B��B|�Bs�Bc*BoqBe4BY�BU�BQ�BEuB�B��BйB��Bw�Bt�BaB�B
��B
��B
��B
�9B
~�B
gEB
G�B
-�B
�B

B	�qB	�#B	̨B	��B	u�B	Z�B	<IB	pB	<B	�B��B��B��B��B��B��B��B�MB�&B��B��BϿBʹB˨BǌBłB�}BŁB�}B�{BŁBŀB�{BŁB�B�}B�uBƇBƅB�B�BƄBƆB�~B�sB�lB�bB�VB�=B�$B�
B��B��B�B�B�B�'B�SBͮB��B�cB��B	B	UB	xB	sB	�B	�B	�B	�B	�B	cB	�B	'�B	$�B	)�B	)�B	)�B	-�B	5B	7"B	;;B	@YB	F}B	J�B	L�B	M�B	R�B	T�B	T�B	S�B	U�B	U�B	T�B	U�B	V�B	W�B	X�B	X�B	Z�B	Z�B	]B	_B	d0B	gAB	hFB	iJB	kWB	mcB	nkB	puB	s�B	t�B	y�B	z�B	{�B	|�B	�B	��B	��B	��B	�B	�B	�"B	�-B	�2B	�9B	�GB	�KB	�PB	�VB	�[B	�ZB	�kB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�$B	�/B	�/B	�,B	�.B	�0B	�.B	�3B	�9B	�?B	�MB	�KB	�KB	�GB	�XB	�qB	�vB	ȄB	ɊB	ʐB	̛B	̚B	̛B	̜B	̜B	ͣB	вB	бB	гB	гB	гB	гB	ѼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�B	�B	�B	�B	�
B	�B	�B	�B	�
B	�	B	�B	�	B	�B	�B	�!B	�'B	�(B	�'B	�(B	�.B	�,B	�*B	�1B	�2B	�1B	�+B	�*B	�2B	�<B	�:B	�1B	�3B	�8B	�@B	�EB	�EB	�>B	�@B	�?B	�>B	�?B	�@B	�>B	�?B	�=B	�>B	�DB	�KB	�QB	�QB	�QB	�XB	�WB	�YB	�XB	�VB	�YB	�YB	�YB	�WB	�ZB	�TB	�VB	�]B	�_B	�dB	�cB	�iB	�lB	�iB	�jB	�hB	�^B	�WB	�XB	�VB	�WB	�^B	�fB	�oB	�qB	�vB	�oB	�oB	�iB	�iB	�kB	�qB	�wB	�uB	�pB	�tB	�wB	�xB	�oB	�oB	�qB	�iB	�iB	�qB	�oB	�nB	�kB	�kB	�nB	�oB	�vB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	G�O�B
B
7B
RB
}B
%�B
*�B
6B
:'B
@IB
K�B
Q�B
W�B
Z�B
aB
f.B
iAB
kLB
n_B
qsB
s�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451142016080714511420160807145114  AO  ARCAADJP                                                                    20150226221435    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221435  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221435  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145114  IP                  G�O�G�O�G�O�                