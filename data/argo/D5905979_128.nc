CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:24Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170924  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���T�1   @��UL@72� ě��b��n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX�fDY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�3D�D�]�D���D��HD�)HD�a�D��3D��RD�#3D�QHD���D�޸D��D�V�Dڔ)D��qD��D�Q�D�)D�˅111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=q@�=q@�=qA�A=�A]�A}�A�A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B��
B���B���B��
B���B���B���B���B���Bã�Bǣ�Bˣ�B�p�B�p�Bף�Bۣ�B��
B��B��B��B��B��B���B���B���C��C��C��C�C	�C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1��D2t{D2�{D3z�D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7z�D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK��DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW��DXz�DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt��Dy��D�\D�X D��)D�ӅD�#�D�\)D��pD�ʏD�pD�K�D�|)D���D��D�P�DڎfD�ϮD�
D�L)D�~fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�  A�A�
=A�A���A���A�1A�1A�JA�VA�{A�{A��A��A��A� �A� �A�"�A�"�A�"�A� �A�"�A� �A��A��A��A� �A� �A� �A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�(�A�-A�/A�-A�/A�/A�+A�-A�/A�-A�(�A�(�A�&�A�VA®A��jA�v�A�A��A���A�K�A�v�A��FA���A��yA��`A�-A��A��A��PA�ƨA�I�A���A�ȴA���A�t�A���A��#A�dZA�~�A�5?A��
A�l�A�A�XA�?}A�\)A��!A���A��A�\)A�hsA���A�1A��A���A�C�A��A�%A���A�bA��
A�ĜA��TA�A�A��A�jA��HA�S�A�l�A��A�A�t�A�^5A���A�hsA�?}A��7A�`BA���A��9A���A��A|�jAx��AwO�Aup�Ar��Ap��An(�Aj�yAfĜAe;dAc�PA`�jA]dZA[�TAY`BAV�AUx�AT�+AS%AQ%AN�ALȴAJ$�AGƨAG7LAF�AEG�AB�!A@��A>ĜA=�^A<�A;�PA9�^A85?A6�/A4�A3�A2�A2{A0��A0E�A/��A.z�A-t�A,��A+�-A*ȴA)��A(�jA'�A&�DA%�^A$�!A$JA#�hA"��A!A!;dA ��A��A�A��A�PA�yA�AAVA��AA(�A��A��A��AVA  A��A�AK�A^5A�A��AbNA��A��A�FA
��A
�A
��A\)A
��A
�RA	��A�RA�TA;dAp�A�wA�`A%A9XA`BA ��A�uA�A;dA z�A �A A�@�ƨ@��@�5?@�/@��!@�?}@�l�@�/@���@�z�@��@�-@�-@�u@���@�7L@��@��y@�(�@ߥ�@�$�@�I�@�"�@�\)@�o@�K�@�b@�K�@��m@؛�@�$�@ղ-@�p�@���@�1@�dZ@�@��#@�%@Гu@��;@��@͡�@��@�Z@��@ə�@�I�@�\)@���@�V@��@ź^@���@+@��@�&�@���@���@��R@�J@��`@�9X@�ƨ@�\)@��@�^5@�p�@��D@��@��P@�t�@�dZ@��w@�t�@���@���@�7L@�~�@���@�&�@��/@��`@���@��@��@�n�@�?}@�z�@���@�t�@�S�@���@���@���@�X@�%@�1@��F@���@�\)@�@�@��9@��@���@���@��^@�@�{@��@���@��7@�/@�Z@��@�o@���@���@��
@�+@���@�M�@�E�@��+@�5?@���@�?}@���@�  @�  @�V@�O�@�`B@�X@�hs@���@�ȴ@���@���@���@��@��@�G�@�j@�C�@��R@���@�-@��-@�7L@�V@��j@��@��@�A�@��m@���@���@���@�~�@�^5@�M�@��@���@��@��#@��^@��@��7@���@���@��@�Ĝ@�j@�b@���@��
@��w@��F@���@���@��m@�  @��@� �@� �@� �@��@��@�+@���@�M�@�5?@�$�@��@�x�@�/@��/@�%@��/@��@�(�@�K�@�o@���@�V@�J@���@�p�@�O�@�/@�V@��u@�bN@�bN@�A�@���@��@��;@��w@�
=@�ȴ@���@�ȴ@��R@�=q@���@�/@���@��/@���@���@��D@� �@��w@�\)@�o@�ȴ@���@���@�ff@�{@��T@���@��-@���@��7@�O�@�?}@�7L@�7L@�&�@�V@��@�9X@��@��;@��F@�\)@��@�F@x�@q:�@i%F@a:�@[�V@RE�@HC-@A@:H�@3j�@/Y@(�4@$�@ b@�,@�#@\�@��@
;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�  A�A�
=A�A���A���A�1A�1A�JA�VA�{A�{A��A��A��A� �A� �A�"�A�"�A�"�A� �A�"�A� �A��A��A��A� �A� �A� �A�"�A�"�A�$�A�&�A�&�A�&�A�(�A�(�A�-A�/A�-A�/A�/A�+A�-A�/A�-A�(�A�(�A�&�A�VA®A��jA�v�A�A��A���A�K�A�v�A��FA���A��yA��`A�-A��A��A��PA�ƨA�I�A���A�ȴA���A�t�A���A��#A�dZA�~�A�5?A��
A�l�A�A�XA�?}A�\)A��!A���A��A�\)A�hsA���A�1A��A���A�C�A��A�%A���A�bA��
A�ĜA��TA�A�A��A�jA��HA�S�A�l�A��A�A�t�A�^5A���A�hsA�?}A��7A�`BA���A��9A���A��A|�jAx��AwO�Aup�Ar��Ap��An(�Aj�yAfĜAe;dAc�PA`�jA]dZA[�TAY`BAV�AUx�AT�+AS%AQ%AN�ALȴAJ$�AGƨAG7LAF�AEG�AB�!A@��A>ĜA=�^A<�A;�PA9�^A85?A6�/A4�A3�A2�A2{A0��A0E�A/��A.z�A-t�A,��A+�-A*ȴA)��A(�jA'�A&�DA%�^A$�!A$JA#�hA"��A!A!;dA ��A��A�A��A�PA�yA�AAVA��AA(�A��A��A��AVA  A��A�AK�A^5A�A��AbNA��A��A�FA
��A
�A
��A\)A
��A
�RA	��A�RA�TA;dAp�A�wA�`A%A9XA`BA ��A�uA�A;dA z�A �A A�@�ƨ@��@�5?@�/@��!@�?}@�l�@�/@���@�z�@��@�-@�-@�u@���@�7L@��@��y@�(�@ߥ�@�$�@�I�@�"�@�\)@�o@�K�@�b@�K�@��m@؛�@�$�@ղ-@�p�@���@�1@�dZ@�@��#@�%@Гu@��;@��@͡�@��@�Z@��@ə�@�I�@�\)@���@�V@��@ź^@���@+@��@�&�@���@���@��R@�J@��`@�9X@�ƨ@�\)@��@�^5@�p�@��D@��@��P@�t�@�dZ@��w@�t�@���@���@�7L@�~�@���@�&�@��/@��`@���@��@��@�n�@�?}@�z�@���@�t�@�S�@���@���@���@�X@�%@�1@��F@���@�\)@�@�@��9@��@���@���@��^@�@�{@��@���@��7@�/@�Z@��@�o@���@���@��
@�+@���@�M�@�E�@��+@�5?@���@�?}@���@�  @�  @�V@�O�@�`B@�X@�hs@���@�ȴ@���@���@���@��@��@�G�@�j@�C�@��R@���@�-@��-@�7L@�V@��j@��@��@�A�@��m@���@���@���@�~�@�^5@�M�@��@���@��@��#@��^@��@��7@���@���@��@�Ĝ@�j@�b@���@��
@��w@��F@���@���@��m@�  @��@� �@� �@� �@��@��@�+@���@�M�@�5?@�$�@��@�x�@�/@��/@�%@��/@��@�(�@�K�@�o@���@�V@�J@���@�p�@�O�@�/@�V@��u@�bN@�bN@�A�@���@��@��;@��w@�
=@�ȴ@���@�ȴ@��R@�=q@���@�/@���@��/@���@���@��D@� �@��w@�\)@�o@�ȴ@���@���@�ff@�{@��T@���@��-@���@��7@�O�@�?}@�7L@�7L@�&�@�V@��@�9X@��@��;@��FG�O�@��@�F@x�@q:�@i%F@a:�@[�V@RE�@HC-@A@:H�@3j�@/Y@(�4@$�@ b@�,@�#@\�@��@
;�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B
�B
�HBB�BC�BR�B`BBv�B� B��B��BĜB��B�BB�B��B	7B�B'�B.B0!B>wB@�B?}B?}B49B�B�BhBbBbBhB�B%�B�B�B�B�B�BhB�B�B�B�B	7BB��B�B�B�ZB��B�wB�B��B��B��B�oB�%Bx�B[#BE�B9XB'�B�B+B
�B
�fB
��B
�XB
��B
�uB
o�B
K�B
$�B
�B
1B	�B	�/B	��B	�'B	�hB	� B	v�B	ffB	N�B	C�B	5?B	&�B	�B	{B	DB	  B��B�yB�BB�B��B��B��BÖB�wB�-B�B��B��B��B��B�hB�JB�%B�B�B�B}�B|�By�Bu�Bs�Bs�Bo�Bk�Bm�BhsBffBffBffBcTBbNB_;B\)B[#BZBZB^5BZB]/B_;B^5BbNBffBffBcTBbNBXBYBXBYBYBZB[#BYBW
BVBT�BT�BYBaHBZBYBXBiyB}�B{�Bz�Bu�Bm�Be`BcTBp�B�B|�Bq�BdZB^5B^5Bq�Bw�Br�Bk�BhsBq�Be`B]/BhsBl�BiyBe`BaHB^5BVBP�BN�BO�BP�BR�BO�BL�BP�B[#BVBVBW
BVBQ�BT�B[#B`BBjBiyBq�BiyB^5B\)B]/B^5B]/B]/B_;B_;B_;BaHBdZBgmBgmBffBs�Bs�Bq�Br�Bt�Bu�Bu�Bw�Bz�B� B�B�B�B�B�+B�DB�JB�VB�\B�bB�bB�hB�hB�{B��B��B��B��B��B��B�B�9B�'B�FBB��B��B�B�B�)B�B�B��B��B��B�B�)B�`B�mB�B�B�B�B�B�B�B�B�B�B�B�B�B	B	\B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	&�B	%�B	#�B	#�B	%�B	0!B	7LB	<jB	=qB	>wB	A�B	I�B	J�B	K�B	J�B	H�B	K�B	M�B	M�B	N�B	O�B	O�B	N�B	L�B	L�B	N�B	P�B	R�B	YB	\)B	_;B	`BB	e`B	ffB	hsB	hsB	hsB	jB	l�B	o�B	q�B	r�B	s�B	u�B	x�B	{�B	~�B	}�B	~�B	� B	�B	�B	�%B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�9B	�FB	�LB	�RB	�^B	�^B	�^B	�dB	�dB	�dB	�jB	�}B	��B	��B	��B	ÖB	ĜB	ǮB	ɺB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�;B	�B
 B
<B
�B
!B
*0B
4�B
="B
@�B
E9B
J�B
P.B
T�B
W�B
ZQB
^�B
d�B
kkB
oOB
r�B
w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�RB
�RB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�LB
�LB
�RB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�LB
�RB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�LB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�RB
�XB
�RB
�RB
�cB
�|B
�|B
ԦB
�gB�B6�BFFBS�BjBsPB��B�=B��B�;BӊB��B�B�|B�B3B!WB#dB1�B3�B2�B2�B'}BB�B�B�B�B�BB+BB�B�B�B�B�B	�B�B�B�B��B�ZB�CB��B��B׫B�9B��B�rB�<B�B��B��By�Bl3BN�B9B,�BWB
�B
��B
�"B
��B
�hB
��B
�AB
��B
cB
?FB
aB

B	��B	�(B	лB	�aB	��B	��B	s�B	j`B	Z B	BuB	74B	(�B	�B	HB	B��B�B�cB�"B��BɯBŘBB�zB�DB�&B��B��B��B�}B�kB�;B�B� By�Bw�Bt�Bt�Bq�Bp�Bm�Bi|BgoBgoBcXB_@BaLB\.BZ"BZ"BZ"BWBVBR�BO�BN�BM�BM�BQ�BM�BP�BR�BQ�BVBZ$BZ$BWBVBK�BL�BK�BL�BL�BM�BN�BL�BJ�BI�BH�BH�BL�BU	BM�BL�BK�B]:Bq�Bo�Bn�Bi�BaRBY!BWBdeBu�Bp�BekBXBQ�BQ�BekBk�BfrB_GB\6BelBY#BP�B\6B`NB]=BY$BUBQ�BI�BD�BB�BC�BD�BF�BC�B@�BD�BN�BI�BI�BJ�BI�BE�BH�BN�BT	B^EB]?BeoB]@BQ�BO�BP�BQ�BP�BP�BSBSBSBUBX"B[5B[5BZ.Bg}Bg}BeqBfwBh�Bi�Bi�Bk�Bn�Bs�Bv�Bw�Bx�Bx�Bz�B
B�B�B�"B�(B�(B�.B�.B�AB�GB�MB�kB��B��B��B��B��B��B�	B�QBBȾB��B��B��B��B��BĦBáBŭB��B��B� B�-B�DB�JB�iB�oB�iB�cB�iB�oB�oB�uB�cB�^B�dB��B	B	+B	
CB	IB	UB	UB	UB	VB	aB	\B	\B		=B		=B	
CB	IB	\B	bB	tB	�B	�B	�B	�B	�B	�B	#�B	+B	0#B	1*B	20B	5AB	=rB	>xB	?~B	>yB	<lB	?B	A�B	A�B	B�B	C�B	C�B	B�B	@�B	@�B	B�B	D�B	F�B	L�B	O�B	R�B	S�B	YB	ZB	\)B	\)B	\)B	^4B	`@B	cSB	e_B	feB	gkB	iwB	l�B	o�B	r�B	q�B	r�B	s�B	u�B	w�B	y�B	}�B	�B	�B	�!B	�3B	�>B	�DB	�KB	�QB	�]B	�iB	�oB	�oB	�iB	�cB	�cB	�]B	�cB	�iB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�1B	�7B	�8B	�DB	�JB	�\B	�hB	�hB	�oB	�hB	�oB	�oB	�oB	�{B	��B	ÌB	ĒB	řB	řB	ƟB	ǥB	ǥB	ȫB	ɱB	ɱB	ʷB	˽B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	��B
�B
\B
�B
�B
(}B
0�B
4)B
8�B
>LB
C�B
HpB
KNB
M�B
RCB
X4B
_B
b�B
f:B
j�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170924    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170924  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                