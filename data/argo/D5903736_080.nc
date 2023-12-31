CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:48Z AOML 3.0 creation; 2016-05-31T19:14:37Z UW 3.1 conversion     
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
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20140721230548  20160531121438  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               PA   AO  4051_7090_080                   2C  D   APEX                            5368                            041511                          846 @��[���
1   @��\Pg�@3O�;dZ�e�9Xb1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    PA   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B ffB  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�3D��3D�9�D��fD��3D���D�I�D��fD�� D�	�D�S3D�|�Dǹ�D��D�C3D�y�D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z�@tz�@�=q@�=qA�A=�A]�A}�A��\A��\A��\A�\)A�\)Aޏ\A�\A�\(BG�BG�BG�B�HB'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-�C/��C1��C3��C5��C7��C9�C;�C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"��D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df��Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{DtaHDy��D��pD�3�D���D��pD��
D�C�D���D��=D��D�MpD�w
Dǳ�D�
D�=pD�s�D��
D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��A��A��A��A��A��A���A�  A���A���A���A���A�  A�A�A�A���A���AٸRA�VA�t�A�-Aԉ7A҇+A�/Aѧ�AЗ�A��#A��A�7LA�?}A��mA��A�JAʓuA���A�VAȟ�AǏ\A�jA�&�A��A�^5A��TAA���A�-A�p�A���A���A�K�A�VA�|�A��A���A��yA�|�A�bNA��hA��A��A��
A�I�A�ȴA�%A��7A��A�1'A��HA��DA�?}A��PA�A���A�A��`A�+A�A�t�A�oA���A�t�A�
=A�A�A��A��uA�
=A�%A�(�A���A���A���A��mA��A��#A�=qA��A�E�A���A�VA�oA��7A��`A���A��A��RA�oA��mA�  A�ȴA�{A�oA��jA���A�`BA�G�A�M�A��/A���A���A�`BA��A�`BA�1A���A}��AyXAu�#AtJAqAqoApffAoG�An�+Al�+AjE�Ag��AcO�A_x�A]�wA[&�AV$�AP�!AO�AN��AM��AKVAH��AC�AA7LA@1'A>�A;XA:��A8�/A4��A2��A1&�A0��A0(�A0  A/��A.�/A.M�A-�FA-&�A,v�A+XA*Q�A*  A)�A(=qA&�DA$~�A#oA!��A ~�A A��Ap�A^5A��A��A��A/A�9A��A
=A"�AM�A�A��A
=A��A�AĜAr�A5?A�HA��AȴAE�A�yA��A�`AE�A��A33A	G�A��A�Ar�A�A�Av�A&�A�Ap�AA J@�~�@�%@�dZ@�$�@�z�@��;@�t�@��R@�5?@��@���@���@�p�@�&�@��/@�bN@�
=@��@�7@�p�@�X@�7L@��@��m@�@�\)@@��@�t�@��y@@�%@�w@���@��@���@��@�I�@�X@�P@�K�@�\)@�dZ@�dZ@�l�@�t�@�+@�O�@��@�ƨ@�V@�\)@�@���@���@��#@��@���@���@ّh@��@ش9@؃@�j@�9X@���@�t�@�K�@��@��@��y@��@�x�@�z�@�$�@�I�@�$�@�z�@���@��H@�n�@�@�n�@�$�@���@�1@��+@��@�j@���@�{@�/@�b@�  @��w@�"�@���@��@��+@�-@��/@���@�K�@�@���@�=q@�@��
@���@��@��^@��@��@���@�1'@���@�9X@�%@�p�@�x�@�`B@�O�@��`@��+@�hs@�  @��F@���@��@�+@���@��+@��+@�^5@�O�@��@���@���@��D@��
@�;d@���@�v�@�n�@�$�@���@��@�&�@��`@���@���@��D@�z�@�Q�@���@��@�S�@�S�@��@���@���@���@��7@��@�`B@�7L@���@�Ĝ@���@�j@�9X@���@�33@���@�^5@�M�@�J@�@��@�@���@��@�`B@�`B@�O�@���@���@�ƨ@�|�@�l�@�C�@�33@�"�@�@��@���@��R@�E�@��@��#@���@��@���@���@���@��^@���@�A�@� �@���@�l�@��@���@���@�@�@�
=@�
=@�
=@�
=@�o@�@��y@�ȴ@���@�M�@�@��@��T@���@���@��^@���@�9X@� �@� �@��@��@���@�t�@�
=@��@�v�@�?}@��@��@�9X@� �@� �@��@��@�b@��m@��F@���@��@�K�@��+@�ff@�{@��@�7L@���@���@�Ĝ@��9@��@���@�+@�V@s��@o�@e/@ax�@Zn�@M�h@Kƨ@F�y@>ff@2��@,Z@'�;@$9X@ 1'@(�@X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��`A��A��A��A��A��A��A���A�  A���A���A���A���A�  A�A�A�A���A���AٸRA�VA�t�A�-Aԉ7A҇+A�/Aѧ�AЗ�A��#A��A�7LA�?}A��mA��A�JAʓuA���A�VAȟ�AǏ\A�jA�&�A��A�^5A��TAA���A�-A�p�A���A���A�K�A�VA�|�A��A���A��yA�|�A�bNA��hA��A��A��
A�I�A�ȴA�%A��7A��A�1'A��HA��DA�?}A��PA�A���A�A��`A�+A�A�t�A�oA���A�t�A�
=A�A�A��A��uA�
=A�%A�(�A���A���A���A��mA��A��#A�=qA��A�E�A���A�VA�oA��7A��`A���A��A��RA�oA��mA�  A�ȴA�{A�oA��jA���A�`BA�G�A�M�A��/A���A���A�`BA��A�`BA�1A���A}��AyXAu�#AtJAqAqoApffAoG�An�+Al�+AjE�Ag��AcO�A_x�A]�wA[&�AV$�AP�!AO�AN��AM��AKVAH��AC�AA7LA@1'A>�A;XA:��A8�/A4��A2��A1&�A0��A0(�A0  A/��A.�/A.M�A-�FA-&�A,v�A+XA*Q�A*  A)�A(=qA&�DA$~�A#oA!��A ~�A A��Ap�A^5A��A��A��A/A�9A��A
=A"�AM�A�A��A
=A��A�AĜAr�A5?A�HA��AȴAE�A�yA��A�`AE�A��A33A	G�A��A�Ar�A�A�Av�A&�A�Ap�AA J@�~�@�%@�dZ@�$�@�z�@��;@�t�@��R@�5?@��@���@���@�p�@�&�@��/@�bN@�
=@��@�7@�p�@�X@�7L@��@��m@�@�\)@@��@�t�@��y@@�%@�w@���@��@���@��@�I�@�X@�P@�K�@�\)@�dZ@�dZ@�l�@�t�@�+@�O�@��@�ƨ@�V@�\)@�@���@���@��#@��@���@���@ّh@��@ش9@؃@�j@�9X@���@�t�@�K�@��@��@��y@��@�x�@�z�@�$�@�I�@�$�@�z�@���@��H@�n�@�@�n�@�$�@���@�1@��+@��@�j@���@�{@�/@�b@�  @��w@�"�@���@��@��+@�-@��/@���@�K�@�@���@�=q@�@��
@���@��@��^@��@��@���@�1'@���@�9X@�%@�p�@�x�@�`B@�O�@��`@��+@�hs@�  @��F@���@��@�+@���@��+@��+@�^5@�O�@��@���@���@��D@��
@�;d@���@�v�@�n�@�$�@���@��@�&�@��`@���@���@��D@�z�@�Q�@���@��@�S�@�S�@��@���@���@���@��7@��@�`B@�7L@���@�Ĝ@���@�j@�9X@���@�33@���@�^5@�M�@�J@�@��@�@���@��@�`B@�`B@�O�@���@���@�ƨ@�|�@�l�@�C�@�33@�"�@�@��@���@��R@�E�@��@��#@���@��@���@���@���@��^@���@�A�@� �@���@�l�@��@���@���@�@�@�
=@�
=@�
=@�
=@�o@�@��y@�ȴ@���@�M�@�@��@��T@���@���@��^@���@�9X@� �@� �@��@��@���@�t�@�
=@��@�v�@�?}@��@��@�9X@� �@� �@��@��@�b@��m@��F@���@��@�K�@��+@�ff@�{@��@�7L@���@���@�Ĝ@��9@��@���@�+@�V@s��@o�@e/@ax�@Zn�@M�h@Kƨ@F�y@>ff@2��@,Z@'�;@$9X@ 1'@(�@X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBW
BVBVBVBVBVBVBVBVBVBVBVBW
BVBVBVBVBVBVBVBXBXB]/Bp�B�1B�VB��B��B�-B�wB��B�;B�NB�BB\B�B-B49BH�BbNBu�B{�B�B��B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�B�B��B��B��B�DB�B~�B|�Bu�Bm�BD�B\BB��B�B�B�B�sB�fB�TB�fB�mB�BB�BȴB�9B��B��B�hB��B��B�oB�\B�VB�1B�bB�+Bs�BZBT�BN�BB�B7LB#�B�BȴB��Bw�BK�B.B�B
�B
��B
�-B
��B
�hB
�\B
�JB
�+B
~�B
x�B
r�B
W
B
7LB
!�B
�B
	7B
B
  B	��B	�B	�NB	��B	��B	��B	�DB	�B	u�B	dZB	F�B	:^B	5?B	,B	�B	PB��B�B�B�fB�5B�B��B��BǮBƨBŢBŢBĜBÖBBB��B��B��B��B��B��B�}B�qB�qB�}B��B��BBBĜBɺBɺBǮBɺBȴBɺB��BɺBȴB��B��B��B��B��B��B��B��B��B��BȴB��B��BȴBŢBÖB��B��B�}B�qB�RB�FB�?B�9B�'B�-B�-B�'B�!B�9B�?B�LB�qB��BÖBȴB��B��B��B��B�#B�TB�yB�B�B�B�B�B��B	%B	JB	bB	�B	�B	�B	�B	"�B	&�B	(�B	(�B	,B	.B	.B	-B	/B	33B	49B	5?B	49B	33B	2-B	5?B	;dB	<jB	=qB	>wB	?}B	?}B	?}B	B�B	B�B	@�B	?}B	?}B	C�B	E�B	F�B	G�B	H�B	I�B	J�B	M�B	N�B	O�B	P�B	P�B	P�B	P�B	R�B	Q�B	VB	ZB	YB	YB	ZB	\)B	\)B	\)B	ZB	W
B	VB	Q�B	R�B	T�B	VB	XB	[#B	]/B	_;B	_;B	_;B	aHB	jB	iyB	q�B	u�B	x�B	z�B	{�B	{�B	z�B	z�B	� B	�B	�B	�B	�B	�B	~�B	{�B	z�B	y�B	z�B	z�B	}�B	�B	�%B	�7B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�9B	�9B	�9B	�?B	�LB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�qB	�qB	�qB	�}B	B	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�/B	�;B	�HB	�NB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B

=B
�B
�B
&�B
+B
0!B
>wB
?}B
B�B
L�B
W
B
^5B
aHB
dZB
hsB
k�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BWBVBVBVBVBVBVBVBVBVBVBVBWBVBVBVBVBVBVBVBXBX!B]=Bp�B�AB�iB��B�B�?B��B��B�KB�bB��B0BlB�B-B4KBH�BbbBu�B{�B�2B��B�"B�B�"B�B��B�
B��B��B��B��B��B��B��B��B��B�B��B�B�2B�3B�B��B��B�ZB�BB}Bu�Bm�BD�BpBB��B�B�B�B�B�yB�fB�vB�B�SB�0B��B�LB��B��B�wB��B��B��B�mB�fB�CB�tB�ABs�BZ2BUBN�BB�B7[B#�B�B��B��Bw�BK�B.&B�B
�B
�B
�AB
��B
�}B
�uB
�bB
�CB
B
x�B
r�B
W"B
7bB
!�B
�B
	TB
6B
 B	��B	�B	�hB	�B	��B	��B	�bB	�%B	u�B	dzB	F�B	:�B	5bB	,+B	�B	sB�B��B�B�B�YB�DB�B��B��B��B��B��B��BþB·B¸B��B��B��B��B��B��B��B��B��B��B��B��B¸B¶B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BüB��B��B��B��B�yB�pB�iB�aB�QB�UB�VB�MB�KB�aB�hB�vB��B��BþB��B�B�B�B�B�JB�yB�B�B�B��B�B��B��B	LB	pB	�B	�B	�B	�B	�B	"�B	'B	)B	)B	,-B	.8B	.9B	-0B	/?B	3XB	4^B	5cB	4[B	3WB	2RB	5dB	;�B	<�B	=�B	>�B	?�B	?�B	?�B	B�B	B�B	@�B	?�B	?�B	C�B	E�B	F�B	G�B	H�B	I�B	J�B	M�B	N�B	PB	QB	QB	QB	Q	B	SB	RB	V'B	Z@B	Y9B	Y:B	ZBB	\KB	\MB	\KB	ZBB	W,B	V(B	RB	SB	U!B	V'B	X3B	[HB	]SB	_]B	_]B	__B	ajB	j�B	i�B	q�B	u�B	x�B	{B	|	B	|B	{B	{B	�"B	�9B	�8B	�;B	�4B	�1B	B	|B	{B	y�B	{B	{B	~B	�3B	�EB	�WB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�4B	�<B	�:B	�TB	�XB	�XB	�YB	�`B	�lB	�zB	�~B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	®B	ĺB	ĻB	ĽB	��B	��B	��B	��B	��B	��B	��B	��B	ļB	ļB	ĻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�"B	�4B	�;B	�;B	�AB	�JB	�HB	�NB	�MB	�NB	�YB	�eB	�lB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B
 B
)B

[B
�B
�B
'B
+ B
0>B
>�B
?�B
B�B
L�B
W(B
^OB
adB
dvB
h�B
k�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214382016053112143820160531121438  AO  ARCAADJP                                                                    20140721230548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230548  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230548  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121438  IP                  G�O�G�O�G�O�                