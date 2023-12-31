CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-23T19:16:17Z AOML 3.0 creation; 2016-05-31T19:14:45Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150923191617  20160531121445  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               }A   AO  4051_7090_125                   2C  D   APEX                            5368                            041511                          846 @�q�b:v�1   @�q��0��@3��t�j�di����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    }A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy��D��D�@ D���D���D��D�S3D�vfD���D�3D�0 D�y�D�� D�fD�9�DچfD�s3D�	�D�P D�l�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��]@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�u�B��)B�\B�\B�\B�\B�\B�\B�u�B��)B�\B�\B�\B�\B�\B�\B�\B�B�B��)B�\B�\B��)B�\B�\B�\B�\B�B�B�B�B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C�C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtUDy��D��D�@�D���D���D��D�T)D�w\D���D�)D�0�D�z�D���D�\D�:�Dڇ\D�t)D�
�D�P�D�m�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A�JA�VA�bA�bA�oA�bA�{A��A��A��A��A��A��A�bA�
=A���A��TA��HA���A蝲A�l�A�%A晚A��A�`BA�r�A�{A�A�9XA�p�A�|�A�VA�5?A�A�Q�A���A�  AֶFA՛�A�M�A�ffAџ�A�z�A�E�A��A�l�A��Aˏ\A�S�A�A�`BA���A��A�\)A�ZA��A�{A�=qA��#A�G�A�`BA�1A���A��jA�|�A�A��A�1'A�"�A��A��A�bA�hsA�ȴA��mA�bA�C�A�  A��RA�ȴA�5?A�Q�A�n�A��TA��#A���A�r�A��wA�K�A��A�S�A��A���A���A�$�A��A�7LA��uA��A���A�\)A���A���A�JA���A�A�A��A��-A�bA�p�A��hA��;A�%A�  A�oA��A�p�A�|�A���A���A���A��hA��yA���A�dZA��mA��A�^5A�XA��PA�At�A|�Axv�Au�FAt��ArE�ApbAnA�Akt�Ah�Af�HAdZAb~�A`�!A]��A["�AY��AX{AU�FAT��AR�AO�7AJ�RAG�AFJACdZAAS�A?�A?K�A>��A=33A;��A9�FA6E�A4r�A3G�A/��A-��A,Q�A+�
A*��A)�PA(��A((�A'�
A'�hA'O�A&�`A&Q�A$�\A#\)A"�9A"ZA!x�A 1AA�AVAA�jA�A��AA�A�A�9A�A��A�+A$�A��AA?}A�DA��A�^A�RA
=A
VA	�^A�HA1A��A/A��A�\A\)AffA�hA�Av�A�TA
=A �`A ��@���@�@��@�`B@���@�hs@�r�@��;@�dZ@�~�@�X@�@�v�@� �@��H@���@�bN@�@�~�@�E�@���@���@�O�@���@�u@�C�@��@� �@�S�@�33@ޗ�@�Z@�ff@�7L@�j@�l�@֧�@�V@Ӆ@�?}@�V@Ͼw@Χ�@��/@��@�-@���@�x�@ț�@Ǖ�@�\)@���@���@�G�@���@ċD@�j@��@�ƨ@��@�v�@�E�@�{@��T@���@��@�Q�@�|�@�;d@�ȴ@�v�@��@�x�@���@�A�@���@���@�@�v�@��@��@�@�/@�z�@�l�@�
=@�"�@��@���@���@�/@���@�bN@�b@���@�C�@��!@�ff@�=q@�@�@�x�@��@��j@�j@�b@���@�;d@�K�@��@��@�x�@�p�@�`B@�?}@�Ĝ@�j@�I�@��@�"�@���@���@�E�@�O�@��@� �@��F@���@�S�@�o@���@�{@�@�`B@�&�@���@�Ĝ@��9@��@�I�@� �@���@�dZ@��@�@�@�^5@��@�7L@��D@�I�@� �@��;@�l�@��@��@��@�S�@�;d@��y@�~�@�5?@�?}@��/@�j@�I�@�b@��w@�|�@�\)@�K�@�+@���@���@�n�@��@��@��^@��7@�%@��u@�  @���@�t�@�;d@�@��@��@���@�-@��T@�`B@�%@���@�Ĝ@�Ĝ@�r�@�Z@�I�@�Q�@� �@��F@�l�@�K�@�33@�
=@��R@�ff@�5?@�{@���@�?}@��@��@��D@�9X@�I�@��@�S�@��y@�ȴ@��!@���@��\@�v�@�M�@�-@��@��T@���@��h@��7@�?}@��@���@���@�j@�bN@�bN@�Q�@�I�@� �@��m@�ƨ@�ƨ@��F@��@���@�dZ@�o@��@���@��\@��+@�~�@�E�@�J@��@���@��^@��^@���@�x�@��m@�n�@z~�@r�\@ihs@`1'@VV@K�
@EV@=�@7��@2J@+��@&v�@!7L@�@Q�@t�@Q�@`B@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A�JA�VA�bA�bA�oA�bA�{A��A��A��A��A��A��A�bA�
=A���A��TA��HA���A蝲A�l�A�%A晚A��A�`BA�r�A�{A�A�9XA�p�A�|�A�VA�5?A�A�Q�A���A�  AֶFA՛�A�M�A�ffAџ�A�z�A�E�A��A�l�A��Aˏ\A�S�A�A�`BA���A��A�\)A�ZA��A�{A�=qA��#A�G�A�`BA�1A���A��jA�|�A�A��A�1'A�"�A��A��A�bA�hsA�ȴA��mA�bA�C�A�  A��RA�ȴA�5?A�Q�A�n�A��TA��#A���A�r�A��wA�K�A��A�S�A��A���A���A�$�A��A�7LA��uA��A���A�\)A���A���A�JA���A�A�A��A��-A�bA�p�A��hA��;A�%A�  A�oA��A�p�A�|�A���A���A���A��hA��yA���A�dZA��mA��A�^5A�XA��PA�At�A|�Axv�Au�FAt��ArE�ApbAnA�Akt�Ah�Af�HAdZAb~�A`�!A]��A["�AY��AX{AU�FAT��AR�AO�7AJ�RAG�AFJACdZAAS�A?�A?K�A>��A=33A;��A9�FA6E�A4r�A3G�A/��A-��A,Q�A+�
A*��A)�PA(��A((�A'�
A'�hA'O�A&�`A&Q�A$�\A#\)A"�9A"ZA!x�A 1AA�AVAA�jA�A��AA�A�A�9A�A��A�+A$�A��AA?}A�DA��A�^A�RA
=A
VA	�^A�HA1A��A/A��A�\A\)AffA�hA�Av�A�TA
=A �`A ��@���@�@��@�`B@���@�hs@�r�@��;@�dZ@�~�@�X@�@�v�@� �@��H@���@�bN@�@�~�@�E�@���@���@�O�@���@�u@�C�@��@� �@�S�@�33@ޗ�@�Z@�ff@�7L@�j@�l�@֧�@�V@Ӆ@�?}@�V@Ͼw@Χ�@��/@��@�-@���@�x�@ț�@Ǖ�@�\)@���@���@�G�@���@ċD@�j@��@�ƨ@��@�v�@�E�@�{@��T@���@��@�Q�@�|�@�;d@�ȴ@�v�@��@�x�@���@�A�@���@���@�@�v�@��@��@�@�/@�z�@�l�@�
=@�"�@��@���@���@�/@���@�bN@�b@���@�C�@��!@�ff@�=q@�@�@�x�@��@��j@�j@�b@���@�;d@�K�@��@��@�x�@�p�@�`B@�?}@�Ĝ@�j@�I�@��@�"�@���@���@�E�@�O�@��@� �@��F@���@�S�@�o@���@�{@�@�`B@�&�@���@�Ĝ@��9@��@�I�@� �@���@�dZ@��@�@�@�^5@��@�7L@��D@�I�@� �@��;@�l�@��@��@��@�S�@�;d@��y@�~�@�5?@�?}@��/@�j@�I�@�b@��w@�|�@�\)@�K�@�+@���@���@�n�@��@��@��^@��7@�%@��u@�  @���@�t�@�;d@�@��@��@���@�-@��T@�`B@�%@���@�Ĝ@�Ĝ@�r�@�Z@�I�@�Q�@� �@��F@�l�@�K�@�33@�
=@��R@�ff@�5?@�{@���@�?}@��@��@��D@�9X@�I�@��@�S�@��y@�ȴ@��!@���@��\@�v�@�M�@�-@��@��T@���@��h@��7@�?}@��@���@���@�j@�bN@�bN@�Q�@�I�@� �@��m@�ƨ@�ƨ@��F@��@���@�dZ@�o@��@���@��\@��+@�~�@�E�@�J@��@���@��^@��^@���@�x�@��m@�n�@z~�@r�\@ihs@`1'@VV@K�
@EV@=�@7��@2J@+��@&v�@!7L@�@Q�@t�@Q�@`B@	�^11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�HB	�HB	�NB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�NB	�HB	�HB	�HB	�BB	�5B	�BB	�;B	�5B	�ZB	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B
JB
!�B
1'B
J�B
J�B
s�B
��B
�qB
��B\BI�B�+B��B��B�RB�ZB�B��B8RBq�B��B��B�B�B�'B�9B�LB��B��B�FB�LB��B�B
=BJBDB
=B�B�B�B�B�BhBPBJB%B��B��B��B�B�B�B�B�sB�TB�;BǮB�wB�9B��B��B��B�VB�B� B{�Bw�Bq�Bk�BiyBcTBW
BJ�B?}B33B$�B�B%B��B�`B�BB�
B��B�oB}�B`BBN�BD�BA�B=qB5?B�B	7B
�B
�/B
�dB
��B
v�B
W
B
<jB
0!B
�B	��B	�mB	ɺB	�B	��B	�PB	�B	t�B	dZB	XB	N�B	E�B	:^B	2-B	$�B	�B	B�B�B�NB�)B�B�
B��B��B��BȴBƨBB�wB�RB�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�\B�oB��B��B��B��B��B��B��B��B��B�oB�\B�VB�hB��B��B��B��B��B��B��B��B�uB�uB�hB�bB�bB�\B�VB�PB�VB�PB�PB�JB�PB�JB�JB�JB�DB�=B�7B�1B�=B�=B�DB�DB�DB�DB�JB�JB�VB�VB�\B�PB�VB�\B�oB�{B�hB�bB�bB�bB�PB�JB�VB�hB�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�'B�3B�9B�?B�RB�jB�qB��BĜBǮBɺB��B��B��B��B��B��B�B�B�
B�B�B�)B�BB�HB�NB�ZB�fB�mB�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	1B	
=B	VB	bB	oB	uB	{B	�B	�B	�B	�B	�B	 �B	 �B	$�B	&�B	(�B	+B	-B	/B	2-B	5?B	7LB	=qB	A�B	C�B	C�B	D�B	G�B	L�B	M�B	O�B	T�B	ZB	ZB	\)B	`BB	dZB	e`B	hsB	iyB	jB	k�B	k�B	n�B	n�B	q�B	s�B	v�B	z�B	|�B	|�B	~�B	�B	�B	�+B	�1B	�7B	�7B	�=B	�DB	�DB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�FB	�LB	�LB	�RB	�XB	�^B	�^B	�^B	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	�}B	��B	��B	ÖB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�BB	�NB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B

=B
JB
oB
�B
$�B
-B
49B
:^B
@�B
F�B
G�B
L�B
R�B
[#B
bNB
hsB
m�B
p�B
u�B
x�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�QB	�PB	�VB	�OB	�PB	�PB	�OB	�OB	�PB	�SB	�UB	�SB	�SB	�VB	�UB	�SB	�PB	�OB	�QB	�KB	�@B	�LB	�CB	�>B	�aB	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B
OB
!�B
1.B
J�B
J�B
s�B
��B
�rB
��BYBI�B�)B��B��B�PB�VB�B��B8PBq�B��B��B�B�B�&B�<B�KB��B��B�DB�JB��B�B
;BGBCB
:B�B�B�B�B�BgBRBIB%B��B��B��B�B�B�B�B�sB�RB�:BǪB�vB�6B��B��B��B�QB�B�B{�Bw�Bq�Bk�BitBcPBWBJ�B?{B34B$�B�B!B��B�ZB�AB�B�B�mB}�B`>BN�BD�BA�B=pB5=B�B	8B
�B
�.B
�bB
��B
v�B
WB
<lB
0'B
�B	��B	�tB	��B	�!B	��B	�ZB	�B	t�B	ddB	XB	N�B	E�B	:jB	28B	$�B	�B	B�B�B�^B�;B�'B�B�B��B��B��BƵBB��B�`B�@B�7B�-B�B�B�B��B��B��B��B��B��B��B��B��B��B�zB�hB�pB��B��B��B��B��B��B��B��B��B��B��B�mB�eB�zB��B��B��B��B��B��B��B��B��B��B�yB�tB�tB�nB�fB�cB�eB�bB�aB�\B�cB�YB�\B�[B�TB�NB�IB�CB�PB�LB�XB�SB�VB�UB�]B�ZB�gB�hB�oB�aB�eB�oB��B��B�{B�tB�tB�tB�aB�[B�gB�|B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�5B�@B�JB�PB�aB�wB�B��BīBǼB��B��B��B��B��B��B�	B�B�B�B�B�%B�6B�NB�UB�[B�fB�rB�xB�B�B�B�B�B��B��B��B��B��B��B�B	B	/B	=B	
GB	^B	mB	zB	B	�B	�B	�B	�B	�B	�B	 �B	 �B	$�B	&�B	(�B	+B	-B	/'B	26B	5GB	7SB	=xB	A�B	C�B	C�B	D�B	G�B	L�B	M�B	O�B	UB	Z%B	Z%B	\1B	`HB	dcB	egB	h{B	iB	j�B	k�B	k�B	n�B	n�B	q�B	s�B	v�B	z�B	|�B	|�B	B	�B	�B	�/B	�7B	�>B	�<B	�DB	�IB	�JB	�aB	�qB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�-B	�KB	�QB	�RB	�YB	�[B	�eB	�bB	�dB	�nB	�pB	�oB	�oB	�oB	�rB	�nB	�oB	�vB	�~B	�B	��B	��B	ÚB	ƬB	ǴB	ǱB	ȸB	ȸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�!B	� B	� B	�B	�%B	�2B	�3B	�:B	�EB	�OB	�WB	�]B	�hB	�pB	�vB	�wB	�zB	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
 B

B
B
B
B
B
B
B
'B
/B
1B
4B
4B
5B

@B
LB
qB
�B
$�B
-B
4;B
:`B
@�B
F�B
G�B
L�B
R�B
[$B
bOB
hsB
m�B
p�B
u�B
x�B
z�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214452016053112144520160531121445  AO  ARCAADJP                                                                    20150923191617    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150923191617  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150923191617  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121445  IP                  G�O�G�O�G�O�                