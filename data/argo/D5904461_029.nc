CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:51Z AOML 3.0 creation; 2016-08-07T21:36:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221351  20160807143632  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_029                   2C  D   APEX                            6531                            072314                          846 @�7>�	�1   @�7?y\��@2�V��d	V�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr�fDs  Ds� Dt  DtffDys3D�fD�<�D�vfD�� D��D�9�D�i�D���D�  D�VfD�|�D��3D���D�<�D�p D�ɚD�3D�I�D�fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @3�
@��R@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
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
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(O]C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDr�Dr��DsqDs�qDtqDts�Dy��D�D�C�D�}D��D�#�D�@RD�pRD�ӅD��D�]D���D���D��D�C�D�v�D��RD�	�D�PRD�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AξwAΓuA�C�A���A�ĜA͝�AͅA�t�A�p�A�jA�bNA�bNA�^5A�\)A�\)A�O�A�I�A�VA�JA�A̸RA�G�A� �A�  A�bA�/A�%A��
A˼jAˁA�9XA��A�JA�A���A��HAʩ�A�ĜAț�A�jA�1'A���Aė�AÉ7A�v�A�\)A��FA�A���A�A�A��mA�G�A��FA���A�-A�z�A�I�A�33A��!A�?}A��#A��9A��wA�%A��DA�bA�7LA���A��A��FA�bNA�n�A�-A��DA�{A�%A���A�33A���A�ĜA�dZA�&�A��A��A��jA�n�A��A��A���A��!A��A���A�|�A���A��HA�C�A��A�r�A�{A�bA�oA�ȴA�^5A�n�A�p�AhsAx�AvAt�Aq`BAo�#Am�hAh��Ac��Ab�\A_p�A[�mAXjAT�jAO33AM�FAM7LALI�AH{AD��AC��AB�!A@��A=|�A:z�A9�A9;dA8 �A7��A6�A5��A3|�A3/A1�A0��A/\)A-?}A,�/A,^5A+�A+S�A+33A*M�A)oA'�#A&��A&�A$VA#7LA!��AVAdZA�AbNAƨA33A�A��Av�A��A��A�
A%A5?A��A�wAO�A�AJAJA�TA�PAdZA/AVAK�A��A
bA	%A�A�!A��AQ�A$�A��A�A�HAffA;dAjA��AhsA�HA�A ��A bN@���@���@��@��@��
@�$�@��`@��;@��^@�@��
@�-@�
=@��@��@�|�@���@�h@��H@柾@�5?@�@�(�@䛦@�p�@�z�@���@��@�V@�j@�|�@�~�@��;@��H@�^5@ٙ�@���@�9X@� �@���@���@�X@��@���@��@�M�@��#@с@�?}@���@��@�@��@Χ�@�V@ͺ^@��@�A�@˶F@���@���@�x�@�Q�@ǝ�@Ǯ@���@Ǖ�@�@Ƨ�@�M�@��@�@�{@�@ź^@ŉ7@�X@�&�@��`@�V@�7L@Ł@��@�@�@���@�`B@���@ļj@�b@ÍP@�o@öF@���@�\)@�@��-@�?}@��@��/@��/@��u@���@�l�@���@�;d@���@���@�b@�1'@�?}@�@���@�hs@�%@��/@��T@�^5@�V@�@���@��7@�?}@��9@��u@�ƨ@�S�@�"�@���@�n�@�E�@���@��@�V@�Ĝ@�r�@�Q�@��@�;d@�@��H@���@�-@��@�`B@���@���@�I�@���@�C�@��\@�ff@�^5@�-@��@���@�7L@�%@��j@��D@��@���@�;d@�"�@��y@���@��R@���@�{@�`B@�/@��9@��@�dZ@�|�@�33@�ȴ@�n�@�=q@�J@��-@�Ĝ@�r�@�Q�@���@���@�S�@��!@�V@�@�O�@��@�%@��@�Ĝ@�bN@�  @�l�@�33@�o@�@��@��R@��+@�V@��@�@��h@��`@��
@�o@��y@��R@�=q@���@���@��@�&�@���@��j@��@�(�@���@���@�t�@�;d@�o@���@��@�ȴ@�~�@�M�@��@��@�@��@�&�@��`@��@�bN@�1'@� �@���@��@��@���@��!@���@�V@�G�@���@�%@��@��@��u@��@��D@�j@��@���@�dZ@�C�@�33@�"�@��@�^5@�M�@��#@��/@�j@��
@��w@��@�K�@��@���@�E�@���@��T@��^@��h@�?}@���@��/@��9@�Z@�(�@�b@���@�  @�;d@|j@uV@j��@e/@\��@W�@Nv�@F��@>�@7�@1%@+C�@%�-@ b@��@��@�^@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AξwAΓuA�C�A���A�ĜA͝�AͅA�t�A�p�A�jA�bNA�bNA�^5A�\)A�\)A�O�A�I�A�VA�JA�A̸RA�G�A� �A�  A�bA�/A�%A��
A˼jAˁA�9XA��A�JA�A���A��HAʩ�A�ĜAț�A�jA�1'A���Aė�AÉ7A�v�A�\)A��FA�A���A�A�A��mA�G�A��FA���A�-A�z�A�I�A�33A��!A�?}A��#A��9A��wA�%A��DA�bA�7LA���A��A��FA�bNA�n�A�-A��DA�{A�%A���A�33A���A�ĜA�dZA�&�A��A��A��jA�n�A��A��A���A��!A��A���A�|�A���A��HA�C�A��A�r�A�{A�bA�oA�ȴA�^5A�n�A�p�AhsAx�AvAt�Aq`BAo�#Am�hAh��Ac��Ab�\A_p�A[�mAXjAT�jAO33AM�FAM7LALI�AH{AD��AC��AB�!A@��A=|�A:z�A9�A9;dA8 �A7��A6�A5��A3|�A3/A1�A0��A/\)A-?}A,�/A,^5A+�A+S�A+33A*M�A)oA'�#A&��A&�A$VA#7LA!��AVAdZA�AbNAƨA33A�A��Av�A��A��A�
A%A5?A��A�wAO�A�AJAJA�TA�PAdZA/AVAK�A��A
bA	%A�A�!A��AQ�A$�A��A�A�HAffA;dAjA��AhsA�HA�A ��A bN@���@���@��@��@��
@�$�@��`@��;@��^@�@��
@�-@�
=@��@��@�|�@���@�h@��H@柾@�5?@�@�(�@䛦@�p�@�z�@���@��@�V@�j@�|�@�~�@��;@��H@�^5@ٙ�@���@�9X@� �@���@���@�X@��@���@��@�M�@��#@с@�?}@���@��@�@��@Χ�@�V@ͺ^@��@�A�@˶F@���@���@�x�@�Q�@ǝ�@Ǯ@���@Ǖ�@�@Ƨ�@�M�@��@�@�{@�@ź^@ŉ7@�X@�&�@��`@�V@�7L@Ł@��@�@�@���@�`B@���@ļj@�b@ÍP@�o@öF@���@�\)@�@��-@�?}@��@��/@��/@��u@���@�l�@���@�;d@���@���@�b@�1'@�?}@�@���@�hs@�%@��/@��T@�^5@�V@�@���@��7@�?}@��9@��u@�ƨ@�S�@�"�@���@�n�@�E�@���@��@�V@�Ĝ@�r�@�Q�@��@�;d@�@��H@���@�-@��@�`B@���@���@�I�@���@�C�@��\@�ff@�^5@�-@��@���@�7L@�%@��j@��D@��@���@�;d@�"�@��y@���@��R@���@�{@�`B@�/@��9@��@�dZ@�|�@�33@�ȴ@�n�@�=q@�J@��-@�Ĝ@�r�@�Q�@���@���@�S�@��!@�V@�@�O�@��@�%@��@�Ĝ@�bN@�  @�l�@�33@�o@�@��@��R@��+@�V@��@�@��h@��`@��
@�o@��y@��R@�=q@���@���@��@�&�@���@��j@��@�(�@���@���@�t�@�;d@�o@���@��@�ȴ@�~�@�M�@��@��@�@��@�&�@��`@��@�bN@�1'@� �@���@��@��@���@��!@���@�V@�G�@���@�%@��@��@��u@��@��D@�j@��@���@�dZ@�C�@�33@�"�@��@�^5@�M�@��#@��/@�j@��
@��w@��@�K�@��@���@�E�@���@��T@��^@��h@�?}@���@��/@��9@�Z@�(�G�O�@���@�  @�;d@|j@uV@j��@e/@\��@W�@Nv�@F��@>�@7�@1%@+C�@%�-@ b@��@��@�^@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
!�B
!�B
 �B
�B
�B
 �B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
)�B
H�B
gmB
�PB
ĜB
��B
ƨB
��B
�sB
��B-BR�B�bB��B�RB��B��B��B��B�B�B�NB�NB�fB�TBȴB��BĜB��B%�BT�B[#B\)B_;BffBhsBgmBiyB�DB�oB��B��B��B��B��B��B��B�-B�-B�B�uB��B{�Bp�Bs�Bk�Bm�Bm�BdZBbNBe`B_;B_;BbNB_;BW
BR�BL�B9XB�BhB	7BB��B�B�B�?B��BiyB@�B/B#�BVB
�B
�ZB
��B
�!B
�JB
r�B
R�B
;dB
+B	�#B	ǮB	�XB	��B	��B	�\B	v�B	_;B	W
B	?}B	!�B	+B��B�`B�NB�BB�
BȴBɺB��B��B��B��B��B��BɺBĜB��B�}B�wB�jB�dB�^B�?B�B��B��B��B�B�B�B�9B�RB�^B�jB�dB�RB�-B�B�?B�FB�?B�9B�?B�}B��B��B��B��B��BB��BBBBBĜBŢBŢBŢBǮBǮBǮBŢBÖBǮBĜB�}B�wB�qB�qB�dB�^B�XB�LB�9B�-B�B�B�B�B�B�!B�'B�FBBÖBŢBȴB��B��B��B��B��B��B�B��B��B��B��B��B��BȴBÖBÖBÖBȴB��B�BB�B�B�B�sB�fB�B�B�B�B�B�B�B�yB�mB�mB�fB�mB�yB�B�B��B��B��B��B��B��B	B	%B	+B		7B	PB	oB	{B	�B	�B	�B	 �B	#�B	)�B	0!B	2-B	49B	7LB	7LB	:^B	A�B	F�B	L�B	O�B	P�B	Q�B	Q�B	R�B	R�B	VB	[#B	^5B	bNB	iyB	jB	k�B	l�B	l�B	m�B	p�B	o�B	o�B	n�B	s�B	x�B	x�B	v�B	t�B	t�B	u�B	u�B	v�B	v�B	t�B	t�B	v�B	{�B	�B	�+B	�DB	�VB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�9B	�9B	�FB	�LB	�FB	�LB	�RB	�RB	�^B	�dB	�dB	�jB	�qB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	ÖB	ĜB	ĜB	ÖB	ÖB	B	B	ÖB	ŢB	ȴB	ȴB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�;B	�;B	�;B	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�`B	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
PB
{B
�B
#�B
+B
1'B
33B
9XB
@�B
E�B
J�B
Q�B
VB
ZB
aHB
cTB
hsB
m�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
!�B
!�B
 �B
�B
�B
 �B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
$�B
)�B
H�B
g`B
�CB
ĐB
̿B
ƚB
��B
�dB
��B- BR�B�QB��B�CBʰB��B��B��B��B�B�;B�;B�UB�@BȢB�rBĊB��B%�BT�B[B\B_'BfVBhbBgZBiiB�2B�^B�}B��B��B��B�tB��B��B�B�B��B�_B�yB{�Bp�Bs�BkqBm�Bm~BdFBb;BeLB_+B_+Bb=B_'BV�BR�BL�B9DB�BWB	 B �B��B�sB��B�(B��BidB@rB/
B#�BEB
�B
�DB
��B
�B
�:B
r�B
R�B
;TB
"B	�B	ǤB	�KB	��B	��B	�RB	v�B	_1B	WB	?wB	!�B	&B��B�[B�IB�?B�BȲBɷB��B��B��B��B��B��BɶBĚB��B�{B�sB�hB�`B�ZB�;B�B��B��B��B��B�B�B�5B�NB�[B�dB�^B�MB�&B�B�;B�@B�:B�3B�;B�xB��B��B��B��B��BB��BBBBBėBŘBŜBŜBǨBǧBǧBśBÑBǦBėB�uB�qB�jB�kB�^B�ZB�OB�HB�4B�&B�B�B��B�B�B�B�"B�ABBÌBŜBȬB˾B��B��BʺBʽB��B�B��B��B��B��B��B��BȯBÎBÐBÏBȬB˿B�9B�B�B�B�iB�^B�tB�B�B�B�B�~B�{B�qB�bB�bB�[B�cB�pB�B�B��B��B��B��B��B��B	B	B	B		-B	CB	cB	oB	}B	�B	�B	 �B	#�B	)�B	0B	2!B	4*B	7>B	7=B	:PB	A|B	F�B	L�B	O�B	P�B	Q�B	Q�B	R�B	R�B	U�B	[B	^&B	b@B	iiB	jpB	kvB	l}B	l|B	m�B	p�B	o�B	o�B	n�B	s�B	x�B	x�B	v�B	t�B	t�B	u�B	u�B	v�B	v�B	t�B	t�B	v�B	{�B	�B	�B	�2B	�EB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�'B	�'B	�4B	�9B	�2B	�7B	�?B	�?B	�LB	�RB	�SB	�XB	�_B	�_B	�eB	�jB	�kB	�kB	�qB	�vB	�wB	ÂB	ĊB	ĈB	ÄB	ÂB	�}B	�~B	ÃB	ōB	ȢB	ȠB	ȡB	ɩB	ȞB	˴B	̺B	ͿB	��B	��B	��B	̹B	˳B	˲B	˴B	˴B	̹B	��B	��B	ͿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�&B	�)B	�,B	�5B	�:B	�9B	�AB	�AB	�@B	�MB	�SB	�QB	�aB	�_B	�fB	�jB	�tB	�rB	�~B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
B
:B
eB
�B
#�B
*�B
1B
3B
9CB
@mB
E�B
J�B
Q�B
U�B
ZB
a0B
c@B
h[B
mzB
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436322016080714363220160807143632  AO  ARCAADJP                                                                    20150226221351    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221351  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221351  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143632  IP                  G�O�G�O�G�O�                