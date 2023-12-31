CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-06-12T02:15:18Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20160612021518  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               {A   AO  5286_8897_123                   2C  D   APEX                            6531                            072314                          846 @׳1�Q�1   @׳2m�N@4��7Kƨ�c=�S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    {A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy� D��D�S3D��3D�ɚD��D�I�D�vfD�� D� D�C3D���D���D�	�D�,�Dڃ3D�� D�  D�C3D�|�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>{@�=q@�=qA�A!�AA�Aa�A��\A��\A��\A��\A��\AЏ\A��\A��\B G�BG�BG�BG�B G�B(G�B0G�B8G�B@G�BHG�BPG�BXG�B`G�BhG�Bp�Bx�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�W
B�W
B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�B�#�C �C�C�C+�C�C
�C�C�C�C�C�C�C�C�C�C+�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CY�RC\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D {D ��D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�{DA{DA�{DB{DB�{DC{DC�{DD
�DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI�{DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{Da{Da�{Db{Db�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh�{Di{Di�{Dj{Dj�{Dk{Dk~Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Dt�{Dy�{D�
D�UpD��pD���D��D�K�D�x�D��=D�=D�EpD���D��
D��D�/
DڅpD��=D�=D�EpD�
D��p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�9XA�33A�5?A�5?A�&�A�(�A�&�A��A��A� �A�"�A�"�A�"�A� �A��A��A��A��A��A��A��A��A��A��A˓uA���A�1'Aɕ�A�  A�|�A�~�A�  A�;dA��A�C�A���A�+A�bNA��AþwA�=qA´9A�I�A��\A�?}A�{A�v�A��DA���A�I�A�C�A��mA��#A���A��HA���A�O�A��hA�$�A�Q�A���A���A�oA� �A��yA��uA���A�`BA�VA�$�A���A�`BA��`A�~�A�t�A��A���A��A��A���A�z�A�ȴA�-A�ĜA���A�;dA�A���A�/A���A��A��A�JA�-A�{A��A���A�E�A���A��
A���A�%A��-A��jA�bNA��mA���A}��A{
=Ay33AwAs�^Aq�ApffAn��Al�Aj5?Ag�#Ae��AcVA_��A\�RA\JA[��AZ�AZ1AW��AVAT �AP�uAL�AIS�AE�AC�7AAG�A>1'A<A�A:��A8n�A6�\A3��A2��A2n�A1�A0jA.ȴA-��A,��A+�wA*n�A)�TA(��A'oA%��A$ȴA#�^A"�+A!�wA ��A I�A��A�jA�DAA�A��A�A;dAZAhsAJA$�A7LA%A�A��A�+A�A=qA�!A�RAQ�A�hA&�A�A5?A��A�RA�A33AVA
��A	S�AffA7LA�#AK�A^5A��A+A�!AVA$�A�mAC�A �jA Q�A 1@��w@�v�@��7@��u@�C�@���@�x�@�K�@�J@�r�@�@�n�@�Ĝ@��D@��@�C�@���@��H@�+@�X@�Ĝ@�A�@�33@���@��@�Q�@��H@��@�$�@�/@�z�@�ƨ@߮@�dZ@�;d@�@ް!@�@�ff@��@�v�@��@�l�@�S�@և+@�hs@���@ԓu@�ȴ@љ�@Ѓ@�o@�@ͺ^@��#@��@�V@�ff@�~�@�~�@�V@�1'@�V@ɡ�@�?}@ɩ�@���@ǥ�@�&�@�E�@��h@� �@���@��@�K�@��+@���@�O�@��@���@�r�@�b@��@�t�@�33@�@�M�@��^@���@��@��@�9X@��@�l�@��R@�M�@�@�@�@��@���@�z�@��;@�o@��\@�@��7@���@� �@�K�@��@�33@�(�@�I�@�;d@��@��^@���@���@�7L@���@��`@�Ĝ@�Z@�b@�1@��@��P@�dZ@�"�@���@�ff@�5?@��T@���@���@��h@��@�X@�7L@�&�@��@�Ĝ@�Z@�1'@�  @��@�\)@�C�@�33@�33@�"�@���@�v�@�ff@�E�@�=q@�$�@��@��@��^@��7@�`B@���@�z�@� �@���@��@�dZ@�dZ@�"�@��!@��+@�=q@��@���@���@�`B@��@���@�r�@�K�@��H@��@�ȴ@���@�^5@�{@�@��#@��7@��@���@��D@�Z@� �@��w@�dZ@�@��R@�V@��@��^@�p�@�/@�%@��/@���@��9@���@�r�@�bN@�1'@�b@��w@�t�@�K�@�;d@��@��y@��R@�~�@�M�@�=q@�=q@�{@���@��7@�`B@�X@�/@��j@��u@�z�@�Q�@�A�@�9X@�(�@�1@���@�l�@�"�@��@���@��!@�=q@���@�@���@�`B@��@�%@��/@��9@��@�Z@�(�@� �@��@�  @��
@��F@���@�\)@�33@�S�@�\)@�\)@�33@���@�E�@��T@�O�@�V@��@�r�@���@���@���@���@�"�@��+@��-@���@�z�@vȴ@l�@_�w@W�@Pb@I%@@r�@:=q@3�m@+��@'\)@"~�@ff@�!@{@��@
=@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A�9XA�33A�5?A�5?A�&�A�(�A�&�A��A��A� �A�"�A�"�A�"�A� �A��A��A��A��A��A��A��A��A��A��A˓uA���A�1'Aɕ�A�  A�|�A�~�A�  A�;dA��A�C�A���A�+A�bNA��AþwA�=qA´9A�I�A��\A�?}A�{A�v�A��DA���A�I�A�C�A��mA��#A���A��HA���A�O�A��hA�$�A�Q�A���A���A�oA� �A��yA��uA���A�`BA�VA�$�A���A�`BA��`A�~�A�t�A��A���A��A��A���A�z�A�ȴA�-A�ĜA���A�;dA�A���A�/A���A��A��A�JA�-A�{A��A���A�E�A���A��
A���A�%A��-A��jA�bNA��mA���A}��A{
=Ay33AwAs�^Aq�ApffAn��Al�Aj5?Ag�#Ae��AcVA_��A\�RA\JA[��AZ�AZ1AW��AVAT �AP�uAL�AIS�AE�AC�7AAG�A>1'A<A�A:��A8n�A6�\A3��A2��A2n�A1�A0jA.ȴA-��A,��A+�wA*n�A)�TA(��A'oA%��A$ȴA#�^A"�+A!�wA ��A I�A��A�jA�DAA�A��A�A;dAZAhsAJA$�A7LA%A�A��A�+A�A=qA�!A�RAQ�A�hA&�A�A5?A��A�RA�A33AVA
��A	S�AffA7LA�#AK�A^5A��A+A�!AVA$�A�mAC�A �jA Q�A 1@��w@�v�@��7@��u@�C�@���@�x�@�K�@�J@�r�@�@�n�@�Ĝ@��D@��@�C�@���@��H@�+@�X@�Ĝ@�A�@�33@���@��@�Q�@��H@��@�$�@�/@�z�@�ƨ@߮@�dZ@�;d@�@ް!@�@�ff@��@�v�@��@�l�@�S�@և+@�hs@���@ԓu@�ȴ@љ�@Ѓ@�o@�@ͺ^@��#@��@�V@�ff@�~�@�~�@�V@�1'@�V@ɡ�@�?}@ɩ�@���@ǥ�@�&�@�E�@��h@� �@���@��@�K�@��+@���@�O�@��@���@�r�@�b@��@�t�@�33@�@�M�@��^@���@��@��@�9X@��@�l�@��R@�M�@�@�@�@��@���@�z�@��;@�o@��\@�@��7@���@� �@�K�@��@�33@�(�@�I�@�;d@��@��^@���@���@�7L@���@��`@�Ĝ@�Z@�b@�1@��@��P@�dZ@�"�@���@�ff@�5?@��T@���@���@��h@��@�X@�7L@�&�@��@�Ĝ@�Z@�1'@�  @��@�\)@�C�@�33@�33@�"�@���@�v�@�ff@�E�@�=q@�$�@��@��@��^@��7@�`B@���@�z�@� �@���@��@�dZ@�dZ@�"�@��!@��+@�=q@��@���@���@�`B@��@���@�r�@�K�@��H@��@�ȴ@���@�^5@�{@�@��#@��7@��@���@��D@�Z@� �@��w@�dZ@�@��R@�V@��@��^@�p�@�/@�%@��/@���@��9@���@�r�@�bN@�1'@�b@��w@�t�@�K�@�;d@��@��y@��R@�~�@�M�@�=q@�=q@�{@���@��7@�`B@�X@�/@��j@��u@�z�@�Q�@�A�@�9X@�(�@�1@���@�l�@�"�@��@���@��!@�=q@���@�@���@�`B@��@�%@��/@��9@��@�Z@�(�@� �@��@�  @��
@��F@���@�\)@�33@�S�@�\)@�\)@�33@���@�E�@��T@�O�@�V@��@�r�@���@���@���@���@�"�G�O�@��-@���@�z�@vȴ@l�@_�w@W�@Pb@I%@@r�@:=q@3�m@+��@'\)@"~�@ff@�!@{@��@
=@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�+B	|�B	u�B	r�B	n�B	�+B	ŢB	��B	ÖB	ÖB	ÖB	�B
%B
JB
 �B
;dB
I�B
dZB
�{B
�^B
�B9XBo�B�}B�{B�'B�B�BB�B.BM�BVBYBR�B\)B��B�?B�jB�3B�B�XB��B,B;dB�%B.B�mB�B�B1B9XBE�BE�B>wB&�B��B�B�BB�ZB��B�BǮBƨB��B�DBm�BoB
�B
�#B
��B
�3B
�\B
�PB
�B
��B
��B
��B
>wB
DB	��B	�B	�BB	�B	�jB	�B	��B	��B	�JB	�B	s�B	dZB	T�B	D�B	0!B	)�B	%�B	�B	�B	PB	  B�B�/BÖB�'B��B��B��B��B��B�hB�{B�hB�7B�%B�B�%B{�Bw�Bv�Bs�Bq�Bq�Bn�Bl�Bs�B}�B{�Bu�Bm�BjBiyBgmBffBaHB[#BYBW
BVBVBVBT�BR�BR�B]/B[#BaHBs�B�B�DB�B�=B��B��B��B��B��B��B��B��B��B�B�9B�RB�FB�9B�9B�-B�-B�'B�'B�B�B�B�B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�'B�!B�'B�9B�FB�LB�LB�^B�^B��B��B�}B�}BB��B�}B�dB�XB�XB�RB�XB�XB�XB�^B�^B�jB�wB�XB�XB�qB�}B�}B�}BĜBƨBǮB��B��B��B��B��B�B�B�B�/B�5B�5B�5B�/B�/B�TB�fB�yB�B�B�B�B�B��B��B��B��B��B��B	B	B	B	+B	JB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	!�B	-B	-B	.B	1'B	6FB	:^B	<jB	H�B	L�B	L�B	K�B	K�B	M�B	O�B	O�B	P�B	R�B	P�B	P�B	VB	_;B	aHB	aHB	aHB	e`B	iyB	l�B	o�B	p�B	o�B	o�B	s�B	t�B	t�B	t�B	v�B	y�B	{�B	�B	�B	�%B	�7B	�7B	�DB	�PB	�PB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�-B	�3B	�9B	�9B	�FB	�XB	�XB	�XB	�jB	�jB	�qB	�qB	�wB	�wB	�}B	�}B	��B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B
DB	��B
B
JB
{B
�B
)�B
0!B
8RB
<jB
D�B
I�B
O�B
W
B
[#B
`BB
dZB
gmB
k�B
o�B
s�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�eB	�2B	|�B	u�B	r�B	n�B	�.B	ŪB	��B	ÚB	ØB	ÜB	�B
&B
LB
 �B
;fB
I�B
d\B
�xB
�\B
�B9RBo�B�yB�yB�"B�B�BB�B.BM�BU�BYBR�B\%B��B�;B�gB�0B�B�QB��B+�B;`B�B.B�fB�B�}B*B9SBE�BE�B>oB&�B��B�B�~B	B�UB��B�BǨBƤB��B�=Bm�BhB
�B
� B
��B
�.B
�ZB
�MB
�B
��B
��B
��B
>wB
FB	��B	�B	�EB	�B	�kB	�B	��B	��B	�MB	�B	s�B	d`B	UB	D�B	0'B	*B	%�B	�B	�B	ZB	 
B�B�9BãB�4B��B��B��B��B��B�vB��B�wB�HB�4B�+B�4B{�Bw�Bv�Bs�Bq�Bq�Bn�Bl�Bs�B~B{�Bu�Bm�Bj�Bi�Bg~BfyBaXB[2BY%BWBVBVBVBUBSBSB]?B[1BaUBs�B�B�QB�&B�HB��B��B��B��B��B��B��B��B�B�)B�EB�[B�SB�BB�BB�7B�8B�4B�1B�$B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B�B�&B�1B�-B�1B�CB�OB�WB�WB�kB�hB��B��B��B��BB��B��B�lB�_B�bB�[B�`B�bB�aB�iB�iB�qB��B�`B�_B�wB��B��B��BĥBƮBǸB��B��B��B��B� B�B�B�%B�6B�=B�<B�>B�6B�7B�]B�nB�B�B�B�B�B�B��B��B��B��B��B��B	B	B	$B	2B	QB	hB	tB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	-B	-B	.B	1,B	6KB	:eB	<mB	H�B	L�B	L�B	K�B	K�B	M�B	O�B	O�B	P�B	R�B	P�B	P�B	VB	_@B	aJB	aJB	aIB	e`B	i|B	l�B	o�B	p�B	o�B	o�B	s�B	t�B	t�B	t�B	v�B	y�B	{�B	�B	�B	�&B	�7B	�9B	�HB	�RB	�RB	�[B	�dB	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	� B	�)B	�.B	�-B	�.B	�1B	�8B	�7B	�DB	�UB	�WB	�YB	�hB	�iB	�nB	�nB	�vB	�wB	�zB	�{B	��B	��B	ÕB	ěB	şB	ŢB	ţB	ǬB	ȱB	ɹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�"B	�"B	�#B	�.B	�7B	�:B	�8B	�8B	�9B	�?B	�AB	�FB	�HB	�KB	�KB	�SB	�RB	�YB	�XB	�^B	�cB	�bB	�aB	�kB	�iB	�rB	�wB	�wB	�yB	�|B	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B
B
HB
xB
�B
)�B
0B
8OB
<eB
D�B
I�B
O�B
WB
[B
`>B
dTB
ghB
k�B
o�B
s�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.07 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160612021518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160612021518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160612021518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                