CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-18T10:16:09Z AOML 3.0 creation; 2016-06-01T00:08:29Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20160218101609  20160531170829  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055_7112_142                   2C  D   APEX                            5374                            041511                          846 @ז���a1   @ז�Ǯ;@:V�u�c�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�	�D�I�D���D�ɚD�fD�S3D���D��3D��3D�FfD�y�D�� D�  D�,�Dڜ�D�� D�	�D�)�D�y�D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\#�C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW�\DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�D�ND��HD��D��D�W�D��D�׮D���D�J�D�~D��{D�{D�1HDڡHD��{D�D�.D�~D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A� �A��A� �A� �A� �A�"�A� �A� �A�"�A�"�A�$�A�$�A�"�A��A��A��A��A�oA��A��yA��A�A�A�bA��RA��A�n�A�l�A�?}A��A��FA�33A�bA��A�jA�5?A��A�+A�1'A�E�A�9XA��A��A�(�A��A�r�A�oA�bA���A���A��A���A��`A���A���A�bA�ĜA�~�A���A�p�A���A�|�A��A�\)A��A�^5A���A��FA��A���A�z�A��A���A�Q�A��HA�-A�jA���A��\A�C�A���A�
=A~�jA~E�A{XAyVAx�!AxQ�Aw�hAv�Au��As�Ap�An�yAn1'Am�FAm�AlZAk�Aj-AgdZAf(�Ad��AcK�Ab��Ab�Aax�A`��A`n�A_�mA^�`A]�^A]hsA\��A[p�AZr�AZ^5AY�7AW�hAV��AVr�AUoAT�AT  AS�
ASO�AS\)ASS�AS�7AS��AS��AS�AS?}AR�AQ�#AQ;dAP��AOt�AM�-AK�AJn�AH�DAF��AE"�AC��AB��ABbNAA�;AAVA@=qA?33A>9XA=��A=��A<v�A:�A:9XA8�A6ĜA5�
A4��A4r�A4 �A3A1��A1G�A0bNA/�A.n�A.$�A-�A,�DA+�TA+VA*~�A)�A&�HA&A�A%��A%�A%`BA%oA${A#\)A"��A �jAv�A�AS�A��A��A%A�TA�A%A�A��AbA�Ar�AM�A��AVAp�A�uAI�A�#AG�AA
n�A	�7A��A �A�FAS�A�/AI�A�A��A��A�A�TA|�A�HAv�A5?A�A �@�t�@��\@���@�V@���@��#@���@�C�@�V@�Ĝ@�t�@�\@�ff@�@�%@�A�@@�M�@�7@웦@� �@�w@��@�+@���@��@���@���@�o@���@�/@ۮ@�Q�@ׅ@�o@�@��H@֟�@ՙ�@Ӿw@�hs@�Q�@Ͼw@��H@�5?@�J@���@�;d@�V@�
=@�+@���@�n�@�{@ɺ^@���@���@�O�@öF@�o@��@���@���@��D@�(�@��
@�
=@��@�@���@��P@���@��@���@��;@���@�@�O�@��@�1@���@�G�@��@�j@��w@�
=@�v�@���@���@�Z@�  @��
@��@��+@�@�I�@�+@�ȴ@�~�@�7L@�I�@��@�M�@��7@��/@��@��;@�l�@���@�M�@���@��@���@�Z@� �@�|�@��@�ȴ@�n�@��^@�/@��@���@���@�@���@���@�hs@�9X@�9X@�I�@�Z@���@��/@��9@��D@�z�@�j@�I�@��m@�K�@��@���@��@�^5@�@���@���@��7@�?}@���@�I�@���@�K�@�K�@�C�@�
=@��H@���@���@��\@�{@���@��@��@��F@���@�t�@�S�@���@�ff@�E�@��@�V@�5?@��^@�x�@�x�@���@��h@���@��j@�Ĝ@��9@��9@��j@��
@��@�Q�@���@�bN@�Q�@�A�@�(�@��;@�|�@��@��H@��R@��!@��!@��\@���@�^5@�-@�@��-@��@�V@���@���@��@�z�@�Z@� �@�@K�@+@
=@~�y@~��@~E�@~{@}��@}@}�@}O�@}/@|�j@|I�@{�F@{��@{�@{�@{t�@{dZ@{dZ@{33@{"�@{o@{o@{o@z=q@y��@y7L@xr�@x �@wl�@w�@v�y@v��@v�+@vff@vE�@v{@u�-@up�@uO�@r-@mO�@c��@\z�@U�-@Mp�@E��@?l�@9�^@4�D@.ȴ@(��@#33@�P@�F@K�@��@K�@
�\@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A� �A��A� �A� �A� �A�"�A� �A� �A�"�A�"�A�$�A�$�A�"�A��A��A��A��A�oA��A��yA��A�A�A�bA��RA��A�n�A�l�A�?}A��A��FA�33A�bA��A�jA�5?A��A�+A�1'A�E�A�9XA��A��A�(�A��A�r�A�oA�bA���A���A��A���A��`A���A���A�bA�ĜA�~�A���A�p�A���A�|�A��A�\)A��A�^5A���A��FA��A���A�z�A��A���A�Q�A��HA�-A�jA���A��\A�C�A���A�
=A~�jA~E�A{XAyVAx�!AxQ�Aw�hAv�Au��As�Ap�An�yAn1'Am�FAm�AlZAk�Aj-AgdZAf(�Ad��AcK�Ab��Ab�Aax�A`��A`n�A_�mA^�`A]�^A]hsA\��A[p�AZr�AZ^5AY�7AW�hAV��AVr�AUoAT�AT  AS�
ASO�AS\)ASS�AS�7AS��AS��AS�AS?}AR�AQ�#AQ;dAP��AOt�AM�-AK�AJn�AH�DAF��AE"�AC��AB��ABbNAA�;AAVA@=qA?33A>9XA=��A=��A<v�A:�A:9XA8�A6ĜA5�
A4��A4r�A4 �A3A1��A1G�A0bNA/�A.n�A.$�A-�A,�DA+�TA+VA*~�A)�A&�HA&A�A%��A%�A%`BA%oA${A#\)A"��A �jAv�A�AS�A��A��A%A�TA�A%A�A��AbA�Ar�AM�A��AVAp�A�uAI�A�#AG�AA
n�A	�7A��A �A�FAS�A�/AI�A�A��A��A�A�TA|�A�HAv�A5?A�A �@�t�@��\@���@�V@���@��#@���@�C�@�V@�Ĝ@�t�@�\@�ff@�@�%@�A�@@�M�@�7@웦@� �@�w@��@�+@���@��@���@���@�o@���@�/@ۮ@�Q�@ׅ@�o@�@��H@֟�@ՙ�@Ӿw@�hs@�Q�@Ͼw@��H@�5?@�J@���@�;d@�V@�
=@�+@���@�n�@�{@ɺ^@���@���@�O�@öF@�o@��@���@���@��D@�(�@��
@�
=@��@�@���@��P@���@��@���@��;@���@�@�O�@��@�1@���@�G�@��@�j@��w@�
=@�v�@���@���@�Z@�  @��
@��@��+@�@�I�@�+@�ȴ@�~�@�7L@�I�@��@�M�@��7@��/@��@��;@�l�@���@�M�@���@��@���@�Z@� �@�|�@��@�ȴ@�n�@��^@�/@��@���@���@�@���@���@�hs@�9X@�9X@�I�@�Z@���@��/@��9@��D@�z�@�j@�I�@��m@�K�@��@���@��@�^5@�@���@���@��7@�?}@���@�I�@���@�K�@�K�@�C�@�
=@��H@���@���@��\@�{@���@��@��@��F@���@�t�@�S�@���@�ff@�E�@��@�V@�5?@��^@�x�@�x�@���@��h@���@��j@�Ĝ@��9@��9@��j@��
@��@�Q�@���@�bN@�Q�@�A�@�(�@��;@�|�@��@��H@��R@��!@��!@��\@���@�^5@�-@�@��-@��@�V@���@���@��@�z�@�Z@� �@�@K�@+@
=@~�y@~��@~E�@~{@}��@}@}�@}O�@}/@|�j@|I�@{�F@{��@{�@{�@{t�@{dZ@{dZ@{33@{"�@{o@{o@{o@z=q@y��@y7L@xr�@x �@wl�@w�@v�y@v��@v�+@vff@vE�@v{@u�-@up�@uO�@r-@mO�@c��@\z�@U�-@Mp�@E��@?l�@9�^@4�D@.ȴ@(��@#33@�P@�F@K�@��@K�@
�\@�R@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBZBZBZBZB[#BZBZBS�BO�BL�BG�BD�BC�BB�BA�B?}B=qB6FB49B)�BB�B�B��B��B��BB��B��BBB��B�5B�FB�7B_;BW
BB�B$�BVB�B��BÖB�3B��B��B��B�{B�Bq�Bk�BcTBYBL�B=qB1'B�BPBB
��B
�TB
�B
��B
ÖB
��B
�XB
�'B
��B
�%B
�B
r�B
hsB
iyB
gmB
bNB
[#B
Q�B
C�B
+B
�B
�B
oB
PB

=B
B	��B	�5B	��B	ƨB	�^B	�9B	�!B	��B	��B	��B	��B	��B	�{B	��B	�{B	�7B	�+B	�=B	�DB	� B	|�B	x�B	jB	k�B	hsB	gmB	gmB	jB	m�B	r�B	{�B	�B	�B	�B	~�B	x�B	r�B	k�B	bNB	T�B	E�B	>wB	1'B	'�B	�B	{B	bB	PB	DB	+B	B	  B��B��B��B�B�B�mB�HB�#B�B��B��B��B��BŢBB�wB�dB�RB�FB�9B�!B�B��B��B��B��B��B��B��B��B�uB�bB�PB�7B�B|�Bz�Bx�Bv�Br�Bk�BjBffBdZBbNB_;B\)BYBW
BT�BO�BI�BF�BD�BC�BB�BB�B@�B>wB>wB=qB>wB>wB=qB;dB;dB:^B<jB=qB:^B9XB;dB:^B;dB;dB9XB49B33B33B2-B1'B2-B0!B.B-B,B)�B)�B)�B(�B(�B)�B(�B(�B)�B+B,B,B,B,B-B/B.B.B+B(�B'�B&�B$�B%�B%�B%�B%�B%�B$�B$�B%�B'�B'�B'�B(�B(�B(�B+B,B.B:^B>wB?}B?}B>wB=qB<jB9XB7LB5?B33B5?B7LB9XB:^B:^B:^B;dB<jB=qB?}BA�BC�BD�BG�BI�BL�BM�BN�BN�BP�BVB[#B\)B\)B_;BaHBcTBiyBjBjBl�Bl�Bm�Bq�Bs�Bz�B~�B�B�B�%B�7B�=B�\B�hB�{B�{B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�RB�^B�}BÖB��B��B��B��B�B�#B�/B�;B�HB�ZB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	B	B	B	+B	
=B	JB	PB	PB	hB	�B	�B	{B	�B	�B	�B	�B	!�B	#�B	#�B	%�B	&�B	'�B	,B	/B	0!B	2-B	6FB	7LB	;dB	=qB	<jB	<jB	=qB	=qB	>wB	@�B	A�B	B�B	H�B	M�B	O�B	Q�B	R�B	R�B	T�B	YB	\)B	^5B	_;B	_;B	_;B	`BB	aHB	ffB	iyB	k�B	m�B	o�B	q�B	q�B	r�B	s�B	u�B	v�B	x�B	{�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�DB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�hB	�hB	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�XB	��B	�sB	��B
	7B
�B
"�B
)�B
1'B
7LB
?}B
F�B
M�B
R�B
W
B
]/B
bNB
ffB
k�B
p�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BX	BX	BX	BX	BX	BXBX	BX	BXBX	BX	BX	BXBXBXBX	BZBZBZBZB[BZBZBS�BO�BL�BG�BD�BC�BB�BA�B?qB=iB6>B40B)�BB�B�B��B��B��B �B��B��B �BB��B�*B�<B�'B_.BV�BB�B$�BIB�mB��BÊB�#B��B��B��B�nB��Bq�BkxBcEBYBL�B=dB1B�BCBB
��B
�GB
��B
ʴB
ÈB
�yB
�MB
�B
��B
�B
��B
r�B
hiB
imB
gbB
bDB
[B
Q�B
C�B
*�B
�B
~B
fB
FB

3B
B	��B	�3B	��B	ƢB	�\B	�5B	�B	��B	��B	��B	��B	��B	�xB	�}B	�wB	�3B	�$B	�7B	�AB	�B	|�B	x�B	j~B	k�B	hrB	glB	gkB	j~B	m�B	r�B	{�B	�B	�B	�B	~�B	x�B	r�B	k�B	bLB	T�B	E�B	>uB	1'B	'�B	�B	{B	cB	NB	DB	0B	B	 B��B��B��B�B�B�oB�IB�%B�	B��B��B��B��BŦBB�|B�gB�VB�KB�>B�&B�B�B��B��B��B��B��B��B��B�xB�hB�UB�=B� B|�Bz�Bx�Bv�Br�Bk�Bj�BfnBdaBbRB_BB\/BYBWBUBO�BI�BF�BD�BC�BB�BB�B@�B>B>B={B>�B>�B=zB;hB;mB:hB<sB=zB:eB9`B;lB:gB;kB;jB9`B4?B3:B3;B26B1/B25B0,B.B-B,B*B*B*B) B(�B*B(�B(�B*B+B,B,B,B,B-B/"B.B.B+
B) B'�B&�B$�B%�B%�B%�B%�B%�B$�B$�B%�B'�B'�B'�B(�B(�B(�B+
B,B.B:eB>|B?�B?�B>~B=yB<nB9^B7RB5DB39B5DB7SB9_B:dB:eB:cB;jB<nB=xB?�BA�BC�BD�BG�BI�BL�BM�BN�BN�BP�BVB[)B\-B\.B_@BaMBcYBi~Bj�Bj�Bl�Bl�Bm�Bq�Bs�Bz�B~�B�	B�	B�'B�9B�>B�`B�jB�|B�|B��B��B��B��B��B��B��B��B��B�B�B�-B�:B�RB�]B�|B×BʿB��B��B��B�B� B�,B�7B�DB�YB�pB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	B	
B	 B	B	(B	
9B	CB	MB	NB	eB	|B	|B	vB	�B	�B	�B	�B	!�B	#�B	#�B	%�B	&�B	'�B	,B	/B	0B	2'B	6?B	7DB	;_B	=iB	<bB	<eB	=lB	=mB	>qB	@{B	A�B	B�B	H�B	M�B	O�B	Q�B	R�B	R�B	T�B	YB	\!B	^.B	_7B	_5B	_5B	`:B	a>B	f\B	irB	k~B	m�B	o�B	q�B	q�B	r�B	s�B	u�B	v�B	x�B	{�B	}�B	~�B	~�B	�B	��B	�B	�B	�B	�B	�B	�!B	�*B	�.B	�:B	�NB	�LB	�QB	�QB	�XB	�]B	�]B	�^B	�`B	�`B	�`B	�aB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	� B	� B	�B	�B	�B	�B	�B	�LB	��B	�iB	��B
	*B
�B
"�B
)�B
1B
7;B
?nB
F�B
M�B
R�B
V�B
]!B
bAB
fXB
kxB
p�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.14 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708292016053117082920160531170829  AO  ARCAADJP                                                                    20160218101609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160218101609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160218101609  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170829  IP                  G�O�G�O�G�O�                