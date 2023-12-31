CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-28T19:16:15Z AOML 3.0 creation; 2016-06-01T00:08:27Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151028191615  20160531170827  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_131                   2C  D   APEX                            5374                            041511                          846 @�za�%�1   @�za���@9�\(���d(��E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B/��B8  B@ffBHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D�fD�I�D�� D�� D��D�VfD�y�D��fD�	�D�FfD���DǼ�D��fD�6fD�\�D�ٚD�fD�<�D�fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B0p�B8�
BA=pBI=pBPp�BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�CO]C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDy��D�D�PRD���D�ƸD�#�D�]D��RD��D�RD�MD��RD�ÅD��D�=D�c�D��RD�D�C�D�D�Ƹ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��HA��TA��`A��TA��TA��`A��mA��mA��yA��yA��`A��;A��HA��;A��A���A�ĜẠ�A�XAʩ�A���A�(�A�O�A�5?A���A���A��
A�;dA��A��A� �A�z�A�{A�A���A�7LA��#A���A��jA�7LA�E�A�VA�p�A��7A�E�A�n�A�;dA�E�A���A��A�  A�p�A��A��DA���A�O�A��A���A��TA�r�A��\A���A��;A���A���A��hA�XA��-A��^A�=qA;dA}��A|�Az~�Ax�RAwXAv�DAu�As�ArZAp��AoS�AnȴAnM�An  Am"�Akl�Af�Ad1'Ac��AcO�Ac�AbȴAbz�AbE�Ab �Aa��Aa�^Aa��Aa\)A`I�A^-A]�A]�A\��A[|�AZAY��AY�AX�/AW�AW�AU�mATr�ASAS`BAS�ARĜARbNAQ�FAP�AOoANz�AMO�ALv�AJn�AHffAHVAHI�AH�AG?}AFr�AFVAF5?AE�
AD�9ABbNAA�A@jA?�A?7LA>��A>�A<��A<~�A<A;�A:�/A9�
A9XA8�A8  A7��A7XA6  A3|�A29XA1��A0  A/`BA.��A.1'A-��A-S�A,��A,JA+A*9XA)��A(�A(��A(r�A(I�A(�A'��A'hsA&n�A%\)A$�DA#�^A"��A"M�A"  A!O�A��AA��A�A�A��A�DAbNAQ�AJAI�A\)Az�A��A/A�/A��A��A�A5?A��Al�A�/AƨA(�A\)A�A%A�A��A{A`BA+AĜA��A�PA	%A �At�AS�A/A�A=qA��A�-AbNA V@�t�@��!@��/@��@��y@���@�ff@�M�@�M�@�=q@�5?@�-@�$�@�$�@��@���@�p�@�%@�z�@��m@�=q@��@�1@�M�@�z�@�l�@�^5@�X@�r�@ޗ�@�@ݑh@�G�@���@���@�
=@�{@�1'@��T@�{@�~�@��#@���@̋D@�9X@���@˾w@�+@ʗ�@���@�`B@��@ȴ9@�b@š�@�Z@�=q@�7L@���@���@�J@�j@��^@�  @���@�X@�/@�  @���@��+@�^5@�{@�p�@���@��@�v�@�O�@�z�@��;@���@��w@��@�dZ@�
=@��@���@�M�@��^@�p�@�?}@��@���@��@��D@���@�n�@��7@���@��@�\)@�;d@�"�@���@���@�hs@��@�dZ@�V@���@��@�j@��@��@�t�@��+@�`B@��9@��@���@��R@���@��@�r�@�1'@�b@�  @��@��m@��m@��m@��m@��
@�ƨ@���@�dZ@�+@��y@�ff@���@�%@���@���@��j@��D@�r�@�Z@�(�@�  @��;@��@�dZ@�"�@�
=@��H@��!@��\@�n�@��@���@�G�@�O�@�`B@�`B@�`B@�7L@���@��u@�  @��@���@��@���@���@��@��^@�hs@�?}@�%@��`@��/@�Ĝ@��@���@��u@�z�@�Q�@� �@��;@�dZ@��@��@���@�ff@�J@�@���@��7@�`B@�hs@��@�x�@�`B@�O�@�G�@�/@��@�Ĝ@���@���@��@�j@�bN@�Z@�bN@�j@�r�@�bN@�A�@�A�@�(�@���@���@���@�@�
=@��@�33@�;d@�K�@��H@�v�@�5?@�J@��#@�@��^@��^@���@���@��h@�&�@��D@�j@�Q�@�(�@�;@~��@|�j@|9X@|1@|1@|1@{��@{�F@{�@{33@y�@p�`@h��@c�F@\�j@S��@MO�@G�@?;d@8��@/;d@'��@!��@E�@�@�@��@x�@��@K�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��HA��HA��TA��`A��TA��TA��`A��mA��mA��yA��yA��`A��;A��HA��;A��A���A�ĜẠ�A�XAʩ�A���A�(�A�O�A�5?A���A���A��
A�;dA��A��A� �A�z�A�{A�A���A�7LA��#A���A��jA�7LA�E�A�VA�p�A��7A�E�A�n�A�;dA�E�A���A��A�  A�p�A��A��DA���A�O�A��A���A��TA�r�A��\A���A��;A���A���A��hA�XA��-A��^A�=qA;dA}��A|�Az~�Ax�RAwXAv�DAu�As�ArZAp��AoS�AnȴAnM�An  Am"�Akl�Af�Ad1'Ac��AcO�Ac�AbȴAbz�AbE�Ab �Aa��Aa�^Aa��Aa\)A`I�A^-A]�A]�A\��A[|�AZAY��AY�AX�/AW�AW�AU�mATr�ASAS`BAS�ARĜARbNAQ�FAP�AOoANz�AMO�ALv�AJn�AHffAHVAHI�AH�AG?}AFr�AFVAF5?AE�
AD�9ABbNAA�A@jA?�A?7LA>��A>�A<��A<~�A<A;�A:�/A9�
A9XA8�A8  A7��A7XA6  A3|�A29XA1��A0  A/`BA.��A.1'A-��A-S�A,��A,JA+A*9XA)��A(�A(��A(r�A(I�A(�A'��A'hsA&n�A%\)A$�DA#�^A"��A"M�A"  A!O�A��AA��A�A�A��A�DAbNAQ�AJAI�A\)Az�A��A/A�/A��A��A�A5?A��Al�A�/AƨA(�A\)A�A%A�A��A{A`BA+AĜA��A�PA	%A �At�AS�A/A�A=qA��A�-AbNA V@�t�@��!@��/@��@��y@���@�ff@�M�@�M�@�=q@�5?@�-@�$�@�$�@��@���@�p�@�%@�z�@��m@�=q@��@�1@�M�@�z�@�l�@�^5@�X@�r�@ޗ�@�@ݑh@�G�@���@���@�
=@�{@�1'@��T@�{@�~�@��#@���@̋D@�9X@���@˾w@�+@ʗ�@���@�`B@��@ȴ9@�b@š�@�Z@�=q@�7L@���@���@�J@�j@��^@�  @���@�X@�/@�  @���@��+@�^5@�{@�p�@���@��@�v�@�O�@�z�@��;@���@��w@��@�dZ@�
=@��@���@�M�@��^@�p�@�?}@��@���@��@��D@���@�n�@��7@���@��@�\)@�;d@�"�@���@���@�hs@��@�dZ@�V@���@��@�j@��@��@�t�@��+@�`B@��9@��@���@��R@���@��@�r�@�1'@�b@�  @��@��m@��m@��m@��m@��
@�ƨ@���@�dZ@�+@��y@�ff@���@�%@���@���@��j@��D@�r�@�Z@�(�@�  @��;@��@�dZ@�"�@�
=@��H@��!@��\@�n�@��@���@�G�@�O�@�`B@�`B@�`B@�7L@���@��u@�  @��@���@��@���@���@��@��^@�hs@�?}@�%@��`@��/@�Ĝ@��@���@��u@�z�@�Q�@� �@��;@�dZ@��@��@���@�ff@�J@�@���@��7@�`B@�hs@��@�x�@�`B@�O�@�G�@�/@��@�Ĝ@���@���@��@�j@�bN@�Z@�bN@�j@�r�@�bN@�A�@�A�@�(�@���@���@���@�@�
=@��@�33@�;d@�K�@��H@�v�@�5?@�J@��#@�@��^@��^@���@���@��h@�&�@��D@�j@�Q�@�(�@�;@~��@|�j@|9X@|1@|1@|1@{��@{�F@{�@{33@y�@p�`@h��@c�F@\�j@S��@MO�@G�@?;d@8��@/;d@'��@!��@E�@�@�@��@x�@��@K�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�7B�1B�1B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�7B�=B�=B�=B�Bw�B33B�B
=B�B��B��B��BÖB�B��B�VB�%Bu�Bk�B^5BS�BO�BK�BB�B&�B�B1B��B�B�BB��BÖB�3B�{B�B{�Bs�Bk�BdZB[#BN�B;dB%�B�B\B  B
�B
�ZB
�B
��B
��B
ǮB
�jB
�B
��B
��B
�bB
�B
q�B
bNB
W
B
O�B
D�B
7LB
-B
�B
uB
\B

=B
%B	��B	�B	��B	��B	��B	�}B	�qB	�^B	�LB	�?B	�9B	�-B	�!B	�!B	�B	��B	��B	�uB	�uB	�oB	�DB	�B	�B	�B	� B	{�B	t�B	n�B	hsB	dZB	bNB	aHB	`BB	^5B	ZB	T�B	N�B	J�B	B�B	<jB	1'B	'�B	&�B	&�B	%�B	"�B	�B	�B	�B	�B	{B	1B	B��B��B��B��B��B��B�B�B�B�B�B�B�mB�fB�`B�NB�B��BŢBB�wB�jB�dB�^B�^B�RB�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�\B�DB�%B�B~�B}�B~�B~�B~�B~�B|�Bz�Bx�Bu�Bs�Br�Bq�Bq�Bp�Bo�Bn�Bm�Bk�BhsBe`BaHB^5B]/B\)B[#BYBW
BW
BVBT�BR�BM�BH�BF�BE�BE�BD�BB�B?}B<jB9XB6FB49B2-B0!B,B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B)�B)�B)�B)�B(�B'�B%�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B �B!�B$�B'�B-B-B2-B33B33B6FB8RB9XB9XB9XB:^B<jB=qBD�BJ�BN�BS�BS�BT�BVBYB[#B\)B]/B_;BdZBgmBjBn�Bs�Bx�Bx�B|�B�+B�DB�bB�oB�{B�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�?B�jB��BBÖBÖBĜBĜBĜBĜBĜBŢBŢBǮBɺB��B��B�
B�B�BB�HB�NB�ZB�fB�fB�fB�fB�mB�sB�yB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	B	%B		7B	PB	VB	uB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	&�B	&�B	'�B	(�B	+B	-B	/B	2-B	49B	5?B	7LB	9XB	<jB	?}B	C�B	F�B	H�B	I�B	K�B	L�B	N�B	O�B	O�B	P�B	R�B	YB	[#B	[#B	]/B	_;B	aHB	bNB	cTB	cTB	e`B	e`B	gmB	iyB	iyB	iyB	hsB	hsB	k�B	l�B	l�B	m�B	m�B	s�B	t�B	t�B	u�B	w�B	y�B	}�B	�B	�B	�B	�B	�B	�=B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�ZB	�B	��B
\B
�B
"�B
.B
6FB
A�B
H�B
Q�B
T�B
YB
[#B
aHB
dZB
jB
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�+B�%B�%B�-B�+B�+B�-B�+B�-B�)B�)B�-B�-B�+B�+B�-B�2B�/B�4B�Bw�B3$B|B
-B�nB˷B��B̺BÃB��B�pB�BB�Bu�BkwB^#BS�BO�BK�BB}B&�BBB��B�B�-B��BÀB�B�cB�B{�Bs�BktBdEB[BN�B;PB%�B}BHB
��B
�B
�FB
��B
��B
��B
ǞB
�ZB
�	B
��B
��B
�QB
��B
q�B
b?B
V�B
O�B
D�B
7<B
- B
�B
gB
OB

/B
B	��B	�B	̿B	�}B	�xB	�oB	�dB	�QB	�@B	�1B	�-B	�!B	�B	�B	�B	��B	��B	�lB	�kB	�cB	�8B	�B	�B	�B	�B	{�B	t�B	n�B	hjB	dRB	bDB	a@B	`6B	^,B	ZB	T�B	N�B	J�B	B�B	<_B	1B	'�B	&�B	&�B	%�B	"�B	�B	�B	�B	�B	uB	+B	B��B��B��B��B��B��B�B�B�B�B�B�~B�gB�_B�XB�JB�B��BşBB�sB�fB�`B�YB�[B�MB�.B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB�mB�fB�[B�BB�#B�B~�B}�B~�B~�B~�B~�B|�Bz�Bx�Bu�Bs�Br�Bq�Bq�Bp�Bo�Bn�Bm�Bk�BhsBe_BaFB^4B]+B\'B[!BYBWBW
BVBT�BR�BM�BH�BF�BE�BE�BD�BBtB?{B<jB9XB6DB48B2-B0#B,B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B)�B)�B)�B)�B(�B'�B%�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BlB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B �B!�B$�B'�B-B-B2+B3/B31B6CB8PB9VB9UB9VB:[B<hB=nBD�BJ�BN�BS�BS�BT�BU�BYB[B\%B])B_6BdSBgjBjxBn�Bs�Bx�Bx�B|�B�%B�<B�ZB�gB�sB�tB�rB�qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�6B�_B�yBBËBÌBēBēBēBēBĒBřBŕBǣBɯB˻B��B��B�B�7B�;B�EB�NB�ZB�[B�ZB�ZB�`B�fB�oB�xB�B�B�B�B�B�B��B��B��B��B��B	 �B	�B	B	B		+B	CB	IB	fB	rB	}B	�B	�B	�B	�B	 �B	"�B	#�B	$�B	%�B	&�B	&�B	'�B	(�B	*�B	,�B	/B	2B	4+B	5-B	7<B	9HB	<YB	?pB	C�B	F�B	H�B	I�B	K�B	L�B	N�B	O�B	O�B	P�B	R�B	YB	[B	[B	]B	_,B	a8B	b=B	cCB	cEB	ePB	eNB	g\B	iiB	ihB	ifB	hcB	hbB	kuB	lyB	l{B	m�B	mB	s�B	t�B	t�B	u�B	w�B	y�B	}�B	��B	�B	�B	�B	�B	�+B	�[B	�cB	�iB	�uB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ʬB	�FB	�B	��B
GB
�B
"�B
. B
62B
AqB
H�B
Q�B
T�B
Y B
[
B
a1B
dDB
jfB
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708272016053117082720160531170827  AO  ARCAADJP                                                                    20151028191615    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151028191615  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151028191615  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170827  IP                  G�O�G�O�G�O�                