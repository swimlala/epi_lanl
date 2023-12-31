CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-04T09:16:04Z AOML 3.0 creation; 2016-08-07T21:51:17Z UW 3.1 conversion     
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150504091604  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               2A   AO  5287_9017_050                   2C  D   APEX                            6529                            072314                          846 @�N	,/�1   @�N	�Q�@0������d�~��"�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    2A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dy�fD��D�C3D�vfD��fD��fD�L�D�|�D���D�fD�0 D�y�D�� D�fD�P D�|�D��3D��D�FfD�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
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
Bb=pBi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��RB��RB��B��B��B��B��B��B��B��B��BиRB��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CV�]CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*#�D*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDDD�DEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDt
>Dy��D��D�Q�D��D��D��D�[�D���D�˅D�D�>�D��RD�θD�D�^�Dڋ�D���D��D�UD��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�VA�VA�VA�bA�VA�VA�VA�A�
=A�bA�VA�JA�{A�VA���A���A��A��HA�jA���A�?}A�bA��AУ�A�|�A�`BAω7AΟ�A��Aͺ^A�|�A��A�S�A���A�jA��A�ZA�jA��AƉ7A�A�{A�1'A���A�9XA��A���A��uA�^5A�x�A���A���A�dZA�  A���A���A���A�O�A�|�A�bA� �A���A���A�\)A�n�A�A�A��;A�/A�z�A���A��A���A��9A���A�|�A�G�A���A�5?A�bNA�7LA�&�A�1'A�9XA�{A�^5A�A�A��A���A���A���A�A�bA��A�/A��\A��A���A��^A��A��jA���AC�A~E�A|�Az��Aw��Au�PArbAm+Ai
=Ag�Ag&�AdQ�Aa\)A^��A\�uAW��AT�yARE�AP��AOO�AM��AL��AL �AK�wAJ�\AI�
AHĜAF��AD�RADM�ACƨACO�AC�ABȴAA��AA�A?��A>=qA<�9A;S�A8��A7`BA6z�A6^5A5��A5oA4-A2�A/��A.$�A-?}A,��A,(�A+?}A)�#A(�+A'��A'"�A&z�A%�7A"��A!�mA!33A bAhsA�A��A1'Ax�An�AoA�!AG�A��A  A1AƨA
ȴA
9XA	%A �A�AVA5?A5?A1'A9XA^5AA/A&�AJAhsAA�AȴAAS�A&�A ��A Z@��y@���@�v�@�7L@�|�@���@�$�@�j@��@���@��y@�  @�;d@�!@�M�@�$�@�K�@�@��@���@�-@@�33@�ȴ@�A�@�E�@��/@�S�@�5?@�A�@ە�@�t�@��@�5?@��T@ٺ^@ٙ�@�?}@���@��@֟�@��@�x�@��/@�b@ӕ�@�-@��T@�@�hs@���@�j@϶F@ΰ!@���@�@�J@��#@́@��@�z�@˶F@˅@ʟ�@�M�@���@�p�@Ȭ@�Z@�l�@Ƨ�@�^5@��#@�%@��`@�Ĝ@ă@�A�@�b@�K�@���@�"�@�n�@��/@���@�l�@�"�@�ȴ@�ff@�5?@���@���@��@�hs@�V@��@� �@��;@���@��F@�@�ȴ@�5?@�@��#@��-@��h@�G�@�%@��j@�bN@�r�@��`@��9@��@�|�@��P@�dZ@�n�@��T@��-@�X@��@��D@�Z@�A�@��m@��@�K�@��@�ff@���@��@�G�@��@�Z@�1@��;@��;@��@��@���@�o@�ff@�{@��@���@���@�x�@�X@�G�@�&�@���@�  @��w@���@�dZ@�V@��@��#@�hs@�G�@���@��`@�Ĝ@�1'@��F@��@�K�@�
=@��+@�~�@�v�@�-@�{@�X@��@���@��D@� �@���@�;d@�
=@��H@�ȴ@��R@���@���@�5?@���@���@���@��h@�?}@�/@�7L@�?}@�?}@��@���@�z�@���@��9@�r�@�  @�ƨ@�C�@�o@��R@�ȴ@���@���@���@��@��h@�&�@�Ĝ@�z�@�9X@� �@�  @��@���@��@��@��#@�@�p�@�V@��@���@��D@��D@�r�@�  @�l�@��@��\@�ff@���@��7@�X@���@�Q�@�b@�  @��@�S�@�;d@��@���@�~�@�E�@�@���@�hs@��@���@��/@�j@��
@��@���@��@���@��
@�ƨ@�t�@�K�@�"�@�@��y@��!@�ff@��@���@�7L@��@�Ĝ@�I�@��@��F@�"�@�ȴ@�(�@�ƨ@l�@w�P@rJ@f{@]�@SS�@J=q@D�D@>5?@7�w@0�`@)G�@!x�@��@  @��@��@V@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�
=A�VA�VA�VA�bA�VA�VA�VA�A�
=A�bA�VA�JA�{A�VA���A���A��A��HA�jA���A�?}A�bA��AУ�A�|�A�`BAω7AΟ�A��Aͺ^A�|�A��A�S�A���A�jA��A�ZA�jA��AƉ7A�A�{A�1'A���A�9XA��A���A��uA�^5A�x�A���A���A�dZA�  A���A���A���A�O�A�|�A�bA� �A���A���A�\)A�n�A�A�A��;A�/A�z�A���A��A���A��9A���A�|�A�G�A���A�5?A�bNA�7LA�&�A�1'A�9XA�{A�^5A�A�A��A���A���A���A�A�bA��A�/A��\A��A���A��^A��A��jA���AC�A~E�A|�Az��Aw��Au�PArbAm+Ai
=Ag�Ag&�AdQ�Aa\)A^��A\�uAW��AT�yARE�AP��AOO�AM��AL��AL �AK�wAJ�\AI�
AHĜAF��AD�RADM�ACƨACO�AC�ABȴAA��AA�A?��A>=qA<�9A;S�A8��A7`BA6z�A6^5A5��A5oA4-A2�A/��A.$�A-?}A,��A,(�A+?}A)�#A(�+A'��A'"�A&z�A%�7A"��A!�mA!33A bAhsA�A��A1'Ax�An�AoA�!AG�A��A  A1AƨA
ȴA
9XA	%A �A�AVA5?A5?A1'A9XA^5AA/A&�AJAhsAA�AȴAAS�A&�A ��A Z@��y@���@�v�@�7L@�|�@���@�$�@�j@��@���@��y@�  @�;d@�!@�M�@�$�@�K�@�@��@���@�-@@�33@�ȴ@�A�@�E�@��/@�S�@�5?@�A�@ە�@�t�@��@�5?@��T@ٺ^@ٙ�@�?}@���@��@֟�@��@�x�@��/@�b@ӕ�@�-@��T@�@�hs@���@�j@϶F@ΰ!@���@�@�J@��#@́@��@�z�@˶F@˅@ʟ�@�M�@���@�p�@Ȭ@�Z@�l�@Ƨ�@�^5@��#@�%@��`@�Ĝ@ă@�A�@�b@�K�@���@�"�@�n�@��/@���@�l�@�"�@�ȴ@�ff@�5?@���@���@��@�hs@�V@��@� �@��;@���@��F@�@�ȴ@�5?@�@��#@��-@��h@�G�@�%@��j@�bN@�r�@��`@��9@��@�|�@��P@�dZ@�n�@��T@��-@�X@��@��D@�Z@�A�@��m@��@�K�@��@�ff@���@��@�G�@��@�Z@�1@��;@��;@��@��@���@�o@�ff@�{@��@���@���@�x�@�X@�G�@�&�@���@�  @��w@���@�dZ@�V@��@��#@�hs@�G�@���@��`@�Ĝ@�1'@��F@��@�K�@�
=@��+@�~�@�v�@�-@�{@�X@��@���@��D@� �@���@�;d@�
=@��H@�ȴ@��R@���@���@�5?@���@���@���@��h@�?}@�/@�7L@�?}@�?}@��@���@�z�@���@��9@�r�@�  @�ƨ@�C�@�o@��R@�ȴ@���@���@���@��@��h@�&�@�Ĝ@�z�@�9X@� �@�  @��@���@��@��@��#@�@�p�@�V@��@���@��D@��D@�r�@�  @�l�@��@��\@�ff@���@��7@�X@���@�Q�@�b@�  @��@�S�@�;d@��@���@�~�@�E�@�@���@�hs@��@���@��/@�j@��
@��@���@��@���@��
@�ƨ@�t�@�K�@�"�@�@��y@��!@�ff@��@���@�7L@��@�Ĝ@�I�@��@��F@�"�G�O�@�(�@�ƨ@l�@w�P@rJ@f{@]�@SS�@J=q@D�D@>5?@7�w@0�`@)G�@!x�@��@  @��@��@V@	�#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B
+B
\B
�B
�B
�B
$�B
;dB
��B
��B0!BT�B�'B��B��B�;B�sB�B�BB1BJBVBPB
=B
=B\B�B2-B0!B2-B'�B6FB�B+B1Bq�B�!B��B��B~�Bq�B`BBE�BD�B:^B�B%B��B�HB��B�}B�B��B�PBy�BT�BVB\)BdZBZB2-B.BoBPB�B�BB
��B
�fB
�B
ƨB
�9B
��B
�%B
u�B
jB
gmB
cTB
aHB
[#B
M�B
A�B
�B
PB
  B	��B	�B	�yB	�B	�dB	��B	��B	�oB	� B	l�B	[#B	J�B	2-B	 �B	oB		7B	B��B��B��B��B��B��B�B�B�sB�fB�fB�`B�ZB�TB�ZB�ZB�NB�/B�
B��B��BǮBƨBĜBÖBĜBŢBB�wB�dB�XB�LB�FB�9B�'B�!B�'B�-B�9B�3B�B�?B�RB�RB�RB�RB�RB�RB�LB�FB�FB�XB�9B�{B|�Bw�B~�B�B�B|�Bx�B�B�bB�uB�{B��B��B��B�!B�!B�9B�?B�XB�jB�jB�jB�}B�}B�}B�}B�wB�}B�wB�qB�qB�qB�jB�^B�^B�^B�^B�XB�^B�jB�jBÖB�
B�B�B�yB�B�yB�B�sB�
B��B�
B�B�)B�TB�`B�TB�ZB�fB�B�B�B�B�B�B��B��B��B��B	B	%B	
=B	{B	�B	�B	�B	�B	�B	!�B	(�B	.B	49B	;dB	<jB	=qB	>wB	@�B	C�B	D�B	I�B	K�B	P�B	S�B	XB	YB	ZB	^5B	_;B	aHB	dZB	dZB	dZB	ffB	iyB	l�B	n�B	o�B	q�B	o�B	l�B	m�B	q�B	s�B	u�B	v�B	x�B	z�B	|�B	}�B	~�B	�B	�%B	�1B	�1B	�1B	�7B	�PB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�3B	�9B	�FB	�XB	�dB	�qB	��B	��B	B	ÖB	ÖB	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�BB	�TB	�TB	�`B	�ZB	�ZB	�TB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
	7B
	7B

=B
DB
DB
\B
hB
oB
oB
oB
uB
uB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
(�B
.B
5?B
9XB
B�B
I�B
N�B
R�B
XB
^5B
dZB
iyB
p�B
s�B
w�B
{�B
~�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B
 �B
 �B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
�B
 �B
�B
�B
�B
 �B	��B	��B
B
>B
cB
kB
kB
$�B
;FB
��B
��B/�BT�B�B�dBͭB�B�PB�kB�sB �B	B"B,B(B
B
B2B�B2B/�B2B'�B6 B�B B	Bq�B��B��B��B~�Bq�B`BExBDqB:5B�B�B��B�B��B�TB��B�|B�%By�BT�BU�B[�Bd-BY�B2B-�BEB#B�B[B�B
��B
�<B
��B
�B
�B
�oB
��B
u�B
jXB
gDB
c*B
aB
Z�B
M�B
AdB
`B
(B	��B	��B	�B	�QB	��B	�>B	��B	�{B	�KB	�B	lfB	[B	J�B	2B	 �B	MB		B	 �B��B��B��B��B��B��B�B�xB�RB�EB�HB�AB�;B�5B�:B�;B�/B�B��B��BʟBǌBƅB�|B�uB�|B�B�pB�UB�EB�6B�,B�'B�B�B� B�B�B�B�B��B�B�0B�/B�.B�0B�/B�/B�*B�#B�$B�3B�B�XB|�Bw�B~�B��B��B|�Bx�B��B�AB�PB�YB�\B�nB��B��B��B�B�B�4B�DB�CB�EB�WB�XB�XB�ZB�QB�WB�SB�KB�JB�IB�BB�9B�9B�9B�:B�2B�9B�DB�DB�oB��B�iB�oB�QB��B�OB�vB�KB��B��B��B��B��B�)B�6B�(B�1B�<B�WB�]B�aB�iB�lB�sB��B��B��B��B	�B	�B	
B	PB	YB	gB	�B	�B	�B	!�B	(�B	-�B	4B	;7B	<=B	=DB	>HB	@UB	CfB	DpB	I�B	K�B	P�B	S�B	W�B	X�B	Y�B	^B	_B	aB	d)B	d)B	d,B	f7B	iJB	l\B	ngB	onB	qyB	onB	l\B	mcB	qxB	s�B	u�B	v�B	x�B	z�B	|�B	}�B	~�B	��B	��B	�B	�B	�B	�B	�B	�3B	�EB	�PB	�]B	�`B	�uB	�}B	�uB	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�2B	�?B	�SB	�VB	�\B	�aB	�dB	�cB	�oB	�{B	ȂB	ɇB	̘B	̘B	͢B	͠B	ΤB	ϫB	бB	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�B	�+B	�%B	�&B	�!B	�,B	�2B	�7B	�<B	�?B	�DB	�FB	�JB	�QB	�]B	�VB	�\B	�ZB	�VB	�\B	�\B	�ZB	�cB	�nB	�oB	�gB	�oB	�oB	�oB	�sB	�{B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B
	B

B
B
B
%B
1B
9B
8B
8B
>B
@B
5B
>B
DB
DB
IB
LB
KB
PB
QB
PB
RG�O�B
]B
�B
%�B
(�B
-�B
5B
9B
BZB
I�B
N�B
R�B
W�B
^ B
d B
iBB
plB
s~B
w�B
{�B
~�B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150504091604    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150504091604  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150504091604  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                