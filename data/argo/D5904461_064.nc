CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-08-05T19:19:14Z AOML 3.0 creation; 2016-08-07T21:36:37Z UW 3.1 conversion     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150805191914  20160807143637  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               @A   AO  5286_8897_064                   2C  D   APEX                            6531                            072314                          846 @�eg�1   @�eg�� @3\(���c1O�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    @A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B.��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  Dy�D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds�fDy�3D��D�<�D�� D�Y�D�	�D�6fD�i�D��3D�fD�C3D��3D�� D�	�D�C3D�FfD��3D�fD�C3D�fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@ƸRA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B/��B8p�B@�
BH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C O]C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDD�qDqD�qDqD�qD�D�qDqD�qDqD�qDqD�DqD�qDqD�qDqD�qDqD�qDqD�D qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDs��Dy��D��D�C�D���D�`RD�RD�=D�pRD���D�D�I�D���D�ָD�RD�I�D�MD���D�D�I�D�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�Q�A�S�A�VA�ZA�ZA�XA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�Q�A�O�A�M�A�E�A�bA�A�hsA���A���A�x�AˋDA˼jA�S�A�ffA���A�9XA��Aʡ�A�JA���A�1'AȬA���A�`BA��A���A�=qA�bNAĬAÍPA�1A�9XA�dZA��A�%A��^A�
=A�n�A���A�+A��A�%A�ffA���A��DA���A��DA��A�Q�A�ZA��`A�n�A�-A�r�A�+A���A�(�A���A�(�A��7A�ĜA���A�
=A�&�A�oA�"�A��uA��A�9XA��A�|�A��HA�I�A���A�7LA��A���A�Q�A���A��hA��A�?}A��A��`A��FA�|�A��A�bA���A��TA�A�
=A���A��A��!A���A��
A��A�-A|��Az�AwC�As��Ao��AkG�AfȴAe?}Ac�
A`�A\��AZ=qAX~�AT�uAO/AJ�+AF�!AE`BACG�A@�A?
=A=O�A;�TA;�A8�RA7K�A5�hA5�A3��A2�uA29XA1;dA0�DA/��A/�A,M�A(��A#�TA"��A"E�A"M�A"VA!�A!�A��AA�yA��A��AK�A�A|�A�wAQ�A"�An�AK�A�/AoAE�A��At�A�A�\AffA��A��AG�A+AA��A�jA?}At�A�
A1A�A	��A	x�A�HA�A?}Av�AXAK�A7LA��An�A^5A��A�A��A �R@�C�@�G�@�|�@��+@�?}@��j@�V@�R@���@��T@�M�@�J@�@�t�@�C�@�S�@�/@���@���@��@�%@�I�@���@��@��#@�|�@�^@�G�@�@�^5@�w@�V@䛦@�I�@�@�|�@��
@�?}@�I�@���@���@�33@�7L@�@�D@�~�@��;@�R@�  @�/@�Q�@�;d@�@���@�V@��@�dZ@��m@��H@Χ�@Ώ\@��@�x�@�Ĝ@�bN@̬@�V@��@��@ɩ�@��`@��@��@���@�9X@ǥ�@�@�?}@ļj@�bN@Å@�ff@�O�@��@�V@��@��@�%@��F@�$�@��@��F@��y@�=q@���@���@�"�@���@�"�@��#@���@�bN@�z�@���@�x�@�?}@�O�@�V@�r�@��F@�+@���@���@�"�@��y@��!@�v�@��\@�{@���@��7@��@��h@�%@�I�@�S�@��@��@�j@�r�@�{@�33@�+@�+@�\)@���@�o@��w@�(�@��@��7@�I�@��@���@���@��P@�C�@��R@�x�@���@���@��#@�@�M�@�-@��^@�X@�?}@���@�  @��P@�ff@�/@�|�@���@�@��9@�9X@��+@��+@�@�p�@��@��/@�j@��F@�t�@�K�@���@��\@�ff@�E�@�5?@�$�@���@��#@���@���@��^@���@�p�@�O�@��@��`@���@��j@��@���@��D@��@�z�@�I�@�1@��@�ƨ@���@�Q�@��#@��y@���@��#@��j@�(�@��m@�t�@���@�z�@��@�(�@�r�@�@�{@�v�@��@�ȴ@���@�M�@�-@�@�@���@�x�@�%@��j@���@�r�@��@��@���@��@�9X@��@�;d@�"�@��@��@��\@�n�@�{@��h@�7L@�&�@���@�Z@��@�  @��P@�;d@�@�v�@�-@���@���@���@��h@�`B@��j@��@�bN@�Q�@�9X@��@��;@��F@���@���@��@�|�@�t�@�dZ@�S�@�;d@�%@�Q�@��m@��@v5?@o�;@d�@a��@V��@QG�@H�`@>��@7�@0��@*�H@'K�@"J@O�@��@�@�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111A�Q�A�S�A�VA�ZA�ZA�XA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�Q�A�O�A�M�A�E�A�bA�A�hsA���A���A�x�AˋDA˼jA�S�A�ffA���A�9XA��Aʡ�A�JA���A�1'AȬA���A�`BA��A���A�=qA�bNAĬAÍPA�1A�9XA�dZA��A�%A��^A�
=A�n�A���A�+A��A�%A�ffA���A��DA���A��DA��A�Q�A�ZA��`A�n�A�-A�r�A�+A���A�(�A���A�(�A��7A�ĜA���A�
=A�&�A�oA�"�A��uA��A�9XA��A�|�A��HA�I�A���A�7LA��A���A�Q�A���A��hA��A�?}A��A��`A��FA�|�A��A�bA���A��TA�A�
=A���A��A��!A���A��
A��A�-A|��Az�AwC�As��Ao��AkG�AfȴAe?}Ac�
A`�A\��AZ=qAX~�AT�uAO/AJ�+AF�!AE`BACG�A@�A?
=A=O�A;�TA;�A8�RA7K�A5�hA5�A3��A2�uA29XA1;dA0�DA/��A/�A,M�A(��A#�TA"��A"E�A"M�A"VA!�A!�A��AA�yA��A��AK�A�A|�A�wAQ�A"�An�AK�A�/AoAE�A��At�A�A�\AffA��A��AG�A+AA��A�jA?}At�A�
A1A�A	��A	x�A�HA�A?}Av�AXAK�A7LA��An�A^5A��A�A��A �R@�C�@�G�@�|�@��+@�?}@��j@�V@�R@���@��T@�M�@�J@�@�t�@�C�@�S�@�/@���@���@��@�%@�I�@���@��@��#@�|�@�^@�G�@�@�^5@�w@�V@䛦@�I�@�@�|�@��
@�?}@�I�@���@���@�33@�7L@�@�D@�~�@��;@�R@�  @�/@�Q�@�;d@�@���@�V@��@�dZ@��m@��H@Χ�@Ώ\@��@�x�@�Ĝ@�bN@̬@�V@��@��@ɩ�@��`@��@��@���@�9X@ǥ�@�@�?}@ļj@�bN@Å@�ff@�O�@��@�V@��@��@�%@��F@�$�@��@��F@��y@�=q@���@���@�"�@���@�"�@��#@���@�bN@�z�@���@�x�@�?}@�O�@�V@�r�@��F@�+@���@���@�"�@��y@��!@�v�@��\@�{@���@��7@��@��h@�%@�I�@�S�@��@��@�j@�r�@�{@�33@�+@�+@�\)@���@�o@��w@�(�@��@��7@�I�@��@���@���@��P@�C�@��R@�x�@���@���@��#@�@�M�@�-@��^@�X@�?}@���@�  @��P@�ff@�/@�|�@���@�@��9@�9X@��+@��+@�@�p�@��@��/@�j@��F@�t�@�K�@���@��\@�ff@�E�@�5?@�$�@���@��#@���@���@��^@���@�p�@�O�@��@��`@���@��j@��@���@��D@��@�z�@�I�@�1@��@�ƨ@���@�Q�@��#@��y@���@��#@��j@�(�@��m@�t�@���@�z�@��@�(�@�r�@�@�{@�v�@��@�ȴ@���@�M�@�-@�@�@���@�x�@�%@��j@���@�r�@��@��@���@��@�9X@��@�;d@�"�@��@��@��\@�n�@�{@��h@�7L@�&�@���@�Z@��@�  @��P@�;d@�@�v�@�-@���@���@���@��h@�`B@��j@��@�bN@�Q�@�9X@��@��;@��F@���@���@��@�|�@�t�@�dZ@�S�G�O�@�%@�Q�@��m@��@v5?@o�;@d�@a��@V��@QG�@H�`@>��@7�@0��@*�H@'K�@"J@O�@��@�@�P1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�yB
��B
��B
ÖB
�ZBJ�B��BÖB��BVBbB�B&�B)�B5?B=qBG�BW
BiyB�B�bB�JB�bB��B��B�B��B��B�B�#B�TB�B��BB+B\B�B�B"�B'�B'�B(�B(�B�B�B�B#�B'�B)�B&�B$�B+B&�B�B�B\B��B�yB�;B�;B�B��B�FB��B}�Br�BjB^5BI�B;dB2-BuB�B�sB�NB�
BƨB�9B��B�B�B|�Bs�B`BB-BB
�B
�'B
��B
q�B
A�B
uB	�B	�B	ȴB	�B	�\B	t�B	R�B	:^B	0!B	$�B	hB��B�B�5BȴB�B��B�7B�B~�Bw�Bt�Bu�B{�B�bB�B�\B�{B�-B�FB�LB�FB�LB�LB�RB�9B��B�1BiyBgmBgmBn�B�B�B�Bv�BffB^5Bo�B|�B�VB�LBɺB��B�B�B��B��BɺB��B�wB�wB�}B��B��BÖBǮBȴB��B�B�`B��B		7B	B��B�B��B	  B��B��B�B�yB�B�B�B�B�B��B��B��B	B	B	B��B�B�HB�)B�B�B�#B�B��BÖBƨB��BŢBŢBŢBǮB��B�B�B��B��B	B	B	  B��B��B��B��B��B��B	B	DB	�B	�B	�B	�B	�B	$�B	1'B	H�B	J�B	F�B	Q�B	gmB	iyB	ffB	^5B	S�B	L�B	C�B	6FB	2-B	�B	�B	)�B	49B	/B	�B	bB	DB	PB	hB	{B	�B	�B	�B	�B	�B	!�B	 �B	�B	�B	"�B	$�B	%�B	&�B	(�B	&�B	%�B	$�B	"�B	 �B	�B	�B	�B	$�B	&�B	&�B	$�B	!�B	�B	�B	�B	�B	�B	�B	%�B	33B	9XB	<jB	;dB	9XB	<jB	=qB	J�B	N�B	Q�B	VB	[#B	]/B	]/B	_;B	cTB	hsB	l�B	p�B	q�B	r�B	w�B	v�B	u�B	v�B	}�B	�B	�B	~�B	� B	�%B	�+B	�VB	�bB	��B	�B	�B	�B	�B	�B	�-B	�RB	�dB	B	ƨB	ĜB	ĜB	ƨB	ȴB	ɺB	ɺB	ȴB	ŢB	ŢB	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	ȴB	ÖB	��B	�wB	�dB	�RB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�LB	��B	ȴB	ȴB	ƨB	ÖB	��B	��B	�wB	�jB	�LB	�FB	�LB	�^B	ȴB	��B	��B	�B	�B	�B	�5B	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
\B
{B
bB
uB
�B
"�B
-B
1'B
5?B
<jB
B�B
I�B
P�B
S�B
]/B
_;B
bNB
iyB
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111B
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
�B
�B
�lB
��B
��B
ÅB
�JBJ�B�yBÅB��BEBRB�B&�B)�B5-B=`BG�BV�BihB�B�RB�9B�QB��B��B�	B�vB��B��B�B�CB�uB��B �BBLBrB�B"�B'�B'�B(�B(�B�B�B�B#�B'�B)�B&�B$�B*�B&�B�B�BMB��B�fB�+B�)B��B̻B�3B��B}�Br�BjqB^ BI�B;QB2BbB�B�]B�9B��BƔB�'B��B�B�B|�Bs�B`/B,�B B
��B
�B
�vB
q�B
AyB
jB	�yB	�B	ȪB	�B	�SB	t�B	R�B	:VB	0B	$�B	bB��B�zB�3BȱB�B��B�6B�B~�Bw�Bt�Bu�B{�B�aB�B�\B�yB�)B�CB�IB�AB�FB�HB�OB�7B��B�/BiwBgnBgmBn�B�B�B�Bv�BfeB^3Bo�B|�B�SB�EBɵB��B�	B��B��B��BɴB��B�pB�qB�vB��B��BÐBǦBȭBʺB��B�VB��B		-B	 �B��B�B��B��B��B��B�B�nB�vB�B�uB�B�B��B��B��B	 �B	B	 �B��B�B�@B�B�B�	B�B��B��BÎBƞB��BŚBřBśBǢB��B�{B�B��B��B	B	 �B��B��B��B��B��B��B��B	 �B	8B	uB	tB	wB	{B	�B	$�B	1B	H�B	J�B	F�B	Q�B	g]B	ijB	fWB	^)B	S�B	L�B	C�B	69B	2 B	�B	�B	)�B	4,B	/B	�B	VB	6B	BB	\B	nB	�B	�B	�B	�B	�B	!�B	 �B	�B	�B	"�B	$�B	%�B	&�B	(�B	&�B	%�B	$�B	"�B	 �B	�B	�B	�B	$�B	&�B	&�B	$�B	!�B	�B	�B	�B	�B	~B	�B	%�B	3#B	9KB	<ZB	;WB	9JB	<\B	=cB	J�B	N�B	Q�B	U�B	[B	] B	]B	_+B	cEB	hcB	l{B	p�B	q�B	r�B	w�B	v�B	u�B	v�B	}�B	�B	��B	~�B	�B	�B	�B	�CB	�SB	��B	��B	��B	��B	� B	�B	�B	�@B	�SB	�|B	ƘB	ĉB	ĊB	ƗB	ȡB	ɨB	ɩB	ȣB	ŏB	ŏB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	ȡB	ÃB	�uB	�dB	�RB	�@B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�;B	�wB	ȢB	ȣB	ƖB	ÄB	�wB	�qB	�fB	�YB	�:B	�0B	�9B	�NB	ȟB	��B	��B	��B	��B	�B	�#B	�5B	�6B	�2B	�:B	�8B	�9B	�BB	�=B	�AB	�CB	�?B	�LB	�gB	�rB	�}B	�yB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
�B
GB
eB
OB
^B
xB
"�B
,�B
1B
5+B
<SB
ByB
I�B
P�B
S�B
]B
_$B
b8B
ibB
n�B
r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436372016080714363720160807143637  AO  ARCAADJP                                                                    20150805191914    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150805191914  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150805191914  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143637  IP                  G�O�G�O�G�O�                