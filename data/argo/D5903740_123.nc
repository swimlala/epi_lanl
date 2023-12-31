CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-07T19:16:38Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150807191638  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               {A   AO  4055_7112_123                   2C  D   APEX                            5374                            041511                          846 @�eۉ�@ 1   @�e�&��@9��1&��dV��O�;1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    {A   A   B   @333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A�  A�33B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy��D�3D�L�D�vfD��fD��fD�Y�D�� D�� D�fD�@ D�� D�� D�3D�C3D�p D��fD��3D�9�D� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@��@��R@��A\)A#\)AC\)Ac\)A��A��A��A��A��A�z�A�z�A�B p�Bp�B�
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
B���B���B�8RB�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�CrO]CtO]Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt`�Dy�D��D�S�D�}D��D��D�`RD���D��D�D�F�D���D�ָD�	�D�I�D�v�D��D���D�@RD�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��#A��/A��;A��TA��mA��yA��yA��mA��mA��mA��mA�ƨA֬A�n�A�+AԑhA�1'A�;dA�n�Aʡ�AɼjAǴ9A��A�A�A�7LAĉ7A�5?AþwA��/A��#A�A�A��yA�bA�
=A�VA�\)A�t�A�$�A���A�33A�hsA�jA��FA��A���A��A��;A��A��-A�ƨA���A��RA���A��
A���A���A�\)A��jA��
A��RA���A�O�A���A��-A���A��A�VA�XA�ȴA�(�A��A��A��7A�l�A�t�A��A�z�A�7LA��;A�I�A�hsA�1A��A�O�A��A�ȴA�VA��A�\)A�(�A��A~ �A|�!A{VAx1'AvjAt��As&�Ap��Am��Al�DAi�Af�Ae�^Ae�AdȴAdn�AdAc��AbVAaAbM�Ab�yAbbNAa�;Aa7LA`��A`��A`�DA^~�A]K�A[p�AY�AW��AU�AT��AS��ARJAPz�ANI�ALA�AKp�AJ�HAJz�AI��AI;dAFĜAE��AE��AE��AEXAE"�AD��AD5?AC�AB��AA��A@A�A?hsA>��A=�hA<ĜA<�A;t�A:A8�+A7/A6��A6VA6-A5�A5hsA4��A3�mA2�/A1��A1�A0A�A.~�A-�PA-&�A,~�A+�TA+l�A*{A)�PA)&�A(�uA'�A'?}A&�uA&�A%dZA$(�A#�A"�uA!A!K�A M�A|�AbNA�Ap�A�mAG�A
=A�uA��A�Ap�AhsAl�A`BAG�A�`A��A
=AȴA�jA�A�DA-A\)A�+A1A33Av�A�AoA%A��A��Ar�AjAZAI�A  A��A
��A
��A
�+A
r�A
A�A	oA�mA��A\)A�/Av�A9XA1A&�A�RA1A�^AO�A ��A V@��#@�x�@��@��D@���@��
@�@���@�~�@�@�@��;@�I�@�p�@�I�@�P@�\@�E�@�-@��#@�7@�+@�&�@���@��;@ڧ�@���@ם�@�S�@��H@թ�@�Ĝ@Ӿw@��y@�^5@�`B@ϥ�@̋D@���@�t�@��H@ɲ-@� �@�;d@��@�V@��T@�j@��`@��w@�
=@�^5@��T@��-@�hs@���@��R@�$�@��#@�ƨ@��@��`@�Q�@�;d@��@�t�@���@���@���@�|�@��@�E�@�%@��w@���@�$�@���@�&�@���@��9@���@�b@�ƨ@���@�l�@�33@��y@��+@�=q@���@���@�1@��y@�=q@�@�G�@���@��@��@�Q�@�(�@�b@��;@��F@�dZ@�"�@��@��!@�$�@��@��@�r�@���@���@���@��@��w@�|�@�K�@�"�@��@�5?@�@�@���@�`B@�?}@�G�@�O�@�X@�hs@�p�@�hs@��@�Z@�I�@� �@�"�@�v�@��+@�v�@�5?@���@��@���@��7@�Ĝ@���@�j@�A�@�9X@���@�;d@�"�@�@�~�@�{@�{@���@�&�@���@���@��y@�M�@�J@���@��@�z�@�1@�ƨ@�|�@��y@�ff@��@��+@��@��@��@��@���@�Z@�@~�@~��@�@+@~�y@|�@�P@~��@}�-@|��@{�
@{�@{@z�H@z�!@y�@zJ@y�7@yhs@yhs@y7L@y%@x�u@xĜ@xr�@x�`@x��@x��@w��@wK�@w�@v��@v{@u�@t�@t�@tz�@tj@s�m@s33@r�H@r��@r^5@r^5@rM�@rM�@rn�@rJ@q��@r�@q��@q%@p�u@p�@pA�@pb@p �@p  @lj@d��@`b@V�+@NE�@K�@Cƨ@>5?@6�y@0Ĝ@*�H@%�-@�@Z@
=@�H@��@�@
M�@\)@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��#A��#A��/A��;A��TA��mA��yA��yA��mA��mA��mA��mA�ƨA֬A�n�A�+AԑhA�1'A�;dA�n�Aʡ�AɼjAǴ9A��A�A�A�7LAĉ7A�5?AþwA��/A��#A�A�A��yA�bA�
=A�VA�\)A�t�A�$�A���A�33A�hsA�jA��FA��A���A��A��;A��A��-A�ƨA���A��RA���A��
A���A���A�\)A��jA��
A��RA���A�O�A���A��-A���A��A�VA�XA�ȴA�(�A��A��A��7A�l�A�t�A��A�z�A�7LA��;A�I�A�hsA�1A��A�O�A��A�ȴA�VA��A�\)A�(�A��A~ �A|�!A{VAx1'AvjAt��As&�Ap��Am��Al�DAi�Af�Ae�^Ae�AdȴAdn�AdAc��AbVAaAbM�Ab�yAbbNAa�;Aa7LA`��A`��A`�DA^~�A]K�A[p�AY�AW��AU�AT��AS��ARJAPz�ANI�ALA�AKp�AJ�HAJz�AI��AI;dAFĜAE��AE��AE��AEXAE"�AD��AD5?AC�AB��AA��A@A�A?hsA>��A=�hA<ĜA<�A;t�A:A8�+A7/A6��A6VA6-A5�A5hsA4��A3�mA2�/A1��A1�A0A�A.~�A-�PA-&�A,~�A+�TA+l�A*{A)�PA)&�A(�uA'�A'?}A&�uA&�A%dZA$(�A#�A"�uA!A!K�A M�A|�AbNA�Ap�A�mAG�A
=A�uA��A�Ap�AhsAl�A`BAG�A�`A��A
=AȴA�jA�A�DA-A\)A�+A1A33Av�A�AoA%A��A��Ar�AjAZAI�A  A��A
��A
��A
�+A
r�A
A�A	oA�mA��A\)A�/Av�A9XA1A&�A�RA1A�^AO�A ��A V@��#@�x�@��@��D@���@��
@�@���@�~�@�@�@��;@�I�@�p�@�I�@�P@�\@�E�@�-@��#@�7@�+@�&�@���@��;@ڧ�@���@ם�@�S�@��H@թ�@�Ĝ@Ӿw@��y@�^5@�`B@ϥ�@̋D@���@�t�@��H@ɲ-@� �@�;d@��@�V@��T@�j@��`@��w@�
=@�^5@��T@��-@�hs@���@��R@�$�@��#@�ƨ@��@��`@�Q�@�;d@��@�t�@���@���@���@�|�@��@�E�@�%@��w@���@�$�@���@�&�@���@��9@���@�b@�ƨ@���@�l�@�33@��y@��+@�=q@���@���@�1@��y@�=q@�@�G�@���@��@��@�Q�@�(�@�b@��;@��F@�dZ@�"�@��@��!@�$�@��@��@�r�@���@���@���@��@��w@�|�@�K�@�"�@��@�5?@�@�@���@�`B@�?}@�G�@�O�@�X@�hs@�p�@�hs@��@�Z@�I�@� �@�"�@�v�@��+@�v�@�5?@���@��@���@��7@�Ĝ@���@�j@�A�@�9X@���@�;d@�"�@�@�~�@�{@�{@���@�&�@���@���@��y@�M�@�J@���@��@�z�@�1@�ƨ@�|�@��y@�ff@��@��+@��@��@��@��@���@�Z@�@~�@~��@�@+@~�y@|�@�P@~��@}�-@|��@{�
@{�@{@z�H@z�!@y�@zJ@y�7@yhs@yhs@y7L@y%@x�u@xĜ@xr�@x�`@x��@x��@w��@wK�@w�@v��@v{@u�@t�@t�@tz�@tj@s�m@s33@r�H@r��@r^5@r^5@rM�@rM�@rn�@rJ@q��@r�@q��@q%@p�u@p�@pA�@pb@p �@p  @lj@d��@`b@V�+@NE�@K�@Cƨ@>5?@6�y@0Ĝ@*�H@%�-@�@Z@
=@�H@��@�@
M�@\)@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B �B#�B2-B<jBN�BiyB�B��B�B.B0!B/B7LB7LB7LB9XB9XB8RB8RB:^B:^B<jB=qBE�BD�BD�B@�B6FB(�B$�B)�B%�B�B\B
=B�B�dB��A�x�A�A�v�A��A�O�A�/A�%A��A��/A���A�  A���A�|�A�G�A�I�A�r�A�^5A��`A�A��PA�XA�;dA�C�A��HBB�sB��Bz�Bm�BcTB]/BS�BF�B6FB-B�B
�B
ƨB
�B
��B
��B
�{B
�VB
�B
ffB
]/B
L�B
#�B
DB	��B	�B	�B	��B	��B	�'B	�uB	�1B	�hB	�DB	�B	~�B	}�B	x�B	x�B	�%B	�{B	�uB	�\B	�DB	�+B	�+B	�B	u�B	iyB	ZB	P�B	K�B	=qB	2-B	)�B	�B	�B	�B	JB	+B	B	  B��B��B�B�B�B�B�B�B��B��B�B�B�HB��BȴBŢBB��B�wB�dB�LB�3B�!B�B�B�B�B�B�B��B��B��B��B��B�{B��B��B�uB�hB�\B�VB�\B�hB�\B�VB�PB�VB�PB�JB�1B�B�B�B~�Bz�Bw�Bs�Br�Bp�Bp�Bo�Bn�Bm�BhsBe`BdZBdZBdZBdZBcTBaHB`BB`BB`BB_;B_;B^5B]/B\)B[#BZBXBVBS�BR�BQ�BQ�BP�BO�BN�BN�BL�BI�BD�BB�BA�BA�BA�B?}B?}BA�BA�BB�BA�BA�BA�B@�B=qB9XB7LB6FB33B1'B-B(�B(�B'�B$�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B�B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B&�B'�B'�B)�B/B33B6FB;dB@�BI�BN�BP�BP�BP�BQ�BR�BS�BVBXBXBXB\)B^5B`BBaHBcTBdZBffBffBiyBn�Bs�Bz�B~�B�B�B�B�B�%B�+B�+B�+B�1B�7B�=B�DB�DB�DB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�RB�dB�wBÖBƨBɺBɺBɺB��B��B��B��B��B�B�B�B�B�B�B�5B�BB�NB�sB�B�B��B��B��B��B��B��B��B��B	  B	B	VB	\B	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	-B	0!B	2-B	49B	5?B	7LB	;dB	=qB	<jB	<jB	A�B	B�B	F�B	G�B	J�B	K�B	L�B	P�B	R�B	VB	[#B	]/B	^5B	^5B	aHB	aHB	cTB	cTB	e`B	ffB	gmB	gmB	gmB	jB	n�B	o�B	o�B	q�B	r�B	s�B	s�B	r�B	s�B	s�B	u�B	u�B	w�B	z�B	{�B	~�B	�B	�B	�7B	��B	�B	ĜB	�
B	�B
\B
�B
'�B
2-B
:^B
C�B
I�B
Q�B
VB
]/B
bNB
hsB
jB
m�B
q�B
v�111111111111111111111111111111111111111111111444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B �B#�B2B<ZBN�BihB��B��BB.B0B/B7AB7>B7?B9LB9LB8DB8DB:RB:QB<]B=aBE�BD�BD�B@tB68B(�B$�B)�B%�B}BHB
)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bz�Bm}BcAB]BS�BF�B62B,�BtB
�B
ƔB
��B
��B
�tB
�iB
�DB
��B
fTB
]B
L�B
#�B
6B	��B	�B	�B	�B	�wB	�B	�kB	�%B	�]B	�:B	�B	~�B	}�B	x�B	x�B	�B	�rB	�iB	�QB	�;B	�B	� B	�B	u�B	ipB	ZB	P�B	K�B	=jB	2&B	)�B	�B	�B	{B	BB	$B	B��B��B��B�B�zB�B�B�B�B��B��B�B�yB�DB��BȰBŠBB�B�rB�^B�JB�0B�B�B�	B�B�B�	B��B��B��B��B��B��B�wB�~B�~B�pB�fB�ZB�SB�[B�gB�[B�SB�NB�SB�MB�FB�/B�B�B�B~�Bz�Bw�Bs�Br�Bp�Bp�Bo�Bn�Bm�BhqBe_BdXBdYBdXBdVBcQBaIB`>B`AB`AB_:B_<B^3B],B\)B["BZBXBVBS�BR�BQ�BQ�BP�BO�BN�BN�BL�BI�BD�BB�BA�BA�BA�B?}B?|BA�BA�BB�BA�BA�BA�B@�B=qB9YB7MB6EB33B1B-B(�B(�B'�B$�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B�B�B�BdB�B�B�B�BxB�B�B�B�BmB_B{B�B�B{B}B{B{B�B�BmBnB~B�B�B�B�B�B�B�B�B�B�B�B!�B&�B'�B'�B)�B/B31B6BB;`B@~BI�BN�BP�BP�BP�BQ�BR�BS�BU�BXBXBXB\#B^-B`<BaDBcNBdTBf^Bf`BisBn�Bs�Bz�B~�B�B�	B�B�B�B�#B�#B�%B�)B�/B�5B�=B�;B�=B�HB�ZB�hB�mB�xB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�	B�B�	B�EB�ZB�mBÌBƜBɮBɭBɱBʶB��B��B��B��B��B��B�B�B�B�B�+B�7B�BB�gB�B�B��B��B��B��B��B��B��B��B��B	B	HB	PB	nB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	)�B	,�B	0B	2B	4)B	51B	7<B	;TB	=cB	<ZB	<YB	AyB	B�B	F�B	G�B	J�B	K�B	L�B	P�B	R�B	U�B	[B	]B	^%B	^$B	a7B	a8B	cCB	cDB	eOB	fTB	g]B	g\B	g\B	jmB	n�B	o�B	o�B	q�B	r�B	s�B	s�B	r�B	s�B	s�B	u�B	u�B	w�B	z�B	{�B	~�B	��B	�B	�'B	�pB	�	B	ćB	��B	�B
FB
�B
'�B
2B
:HB
C�B
I�B
Q�B
U�B
]B
b8B
hZB
jjB
myB
q�B
v�111111111111111111111111111111111111111111111444444444444444444444444444444111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150807191638    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150807191638  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150807191638  QCF$                G�O�G�O�G�O�4000            UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                