CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:22Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170922  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               {A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���N ��1   @����s��@6��
=p��b��-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    {A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DW��DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�
D�qD�d{D��HD�� D�D�^fD��HD��3D��D�Z=D���D���D��D�MDډHD��=D� D�^�D��D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B�
=B���B�p�B�p�Bã�Bǣ�B��
B��
B�p�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C�C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�RC3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+��D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7nD7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV��DWt{DW�DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�HDy��D���D�^�D���D��=D�QD�X�D���D��pD�)D�TzD���D��3D��D�G\Dڃ�D��zD�
=D�X�D�D��G111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AɾwA���A�ƨA�ƨA�ƨA�A�ƨA�ĜA���A��
A���A���A���A���A��
A��
A��#A��A��A��A��A��#A��#A��HA��TA��TA��mA��mA��TA��`A��`A��mA��mA��A��A��yA��A��yA��mA��yA��A��A��A��A��A��`A��AA��A�"�A���A���A�?}A�A��A��A��A��
A��HA���A��^A�K�A���A��
A�ZA�1A���A�\)A�VA�t�A�$�A���A�ȴA��A�ĜA�ffA���A��^A� �A�;dA�A���A���A�9XA���A��HA���A���A�ĜA��uA��RA�C�A�M�A���A�S�A�ĜA�x�A�+A��+A���A�VA�O�A���A��jA��;A�C�A��9A��^A��^A��wA�VA���A��-A���A�VAz�/Ax�`Aw�PAu�AsVArr�Ap�/An�Ak�Aj�`Ai��Ah��Ag|�Ac7LAbr�Aa/A_?}A[+AVr�AU&�AT9XAR��AM&�AKAH��AE�ADȴAC�-AB�!ABE�A@�`A>ffA=hsA=\)A=O�A;�A:JA8��A6�yA4��A3�A2��A2ffA2  A0ĜA0A/VA.JA-\)A,�A,JA+��A+&�A*��A)�A)G�A(z�A'O�A&-A%\)A$�A$=qA#�PA!�A!oA�AĜA��A�9AƨA�`AQ�A�PAI�A�A�AE�A�AĜAVA�^A/A��AJA��Ar�A�;A�PAt�A��A��AO�A�^A
��A	�A	dZA�-AA�A�!AZAt�A��A��A v�@��!@���@��`@�S�@�S�@�I�@�^5@�/@��@�M�@�-@���@�X@�@���@�G�@畁@��@柾@�r�@�@�Ĝ@�@�@���@���@�p�@��@��/@�Ĝ@���@�b@�5?@ܼj@ۍP@ؼj@�ȴ@�5?@պ^@պ^@�G�@���@ӝ�@�
=@�^5@щ7@ЋD@�1@�@˕�@��T@ɩ�@�V@�`B@�1'@��y@���@�K�@�l�@��@��j@��F@��@�1@��F@�+@��y@�ȴ@�~�@�$�@��^@�7L@��j@�Q�@�ƨ@���@��@�p�@��@�33@�V@���@��@�@��u@��u@��@��@�X@���@���@�1@��@���@���@�;d@�v�@��@��;@�|�@�dZ@�C�@�33@��+@��@�+@�K�@��m@��;@�ƨ@��@���@�ff@�@���@�A�@��@��m@���@��-@��-@�x�@�&�@��j@�(�@�;d@���@��@��H@��\@��#@��h@�G�@��u@�b@��
@���@��F@�\)@��@���@���@���@���@�ff@�5?@�{@��@���@��^@��-@��h@�p�@�/@�&�@��`@��9@�r�@�I�@�1'@�ƨ@�|�@�\)@�33@�=q@���@���@���@�?}@��@��@�j@�9X@��w@�"�@��
@�ƨ@�l�@���@�n�@��^@�G�@��j@�X@�@�Ĝ@�bN@��-@�x�@�r�@�@�n�@��@�ff@�J@���@�  @�K�@��@���@��+@�E�@���@�x�@�?}@���@�Ĝ@�bN@�I�@� �@�1@�  @�  @���@�ƨ@��F@��P@��@�dZ@��@�@���@���@��@�ȴ@���@�$�@��@���@�{@�$�@�J@��@��@��@��T@���@�@��7@�p�@�O�@��@��/@���@��j@��u@�r�@��m@�;d@��@�@�n�@�v�@�V@�J@��-@�?}@��@��@�j@�Q�@� �@�  @��m@��w@���@��@�S�@�;d@�@���@x�@oo@gn/@a!�@Zxl@R�@M��@Go�@D  @<K^@6�@0�I@-2a@'n/@#�F@�t@�]@�?@P�@	+�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AɾwA���A�ƨA�ƨA�ƨA�A�ƨA�ĜA���A��
A���A���A���A���A��
A��
A��#A��A��A��A��A��#A��#A��HA��TA��TA��mA��mA��TA��`A��`A��mA��mA��A��A��yA��A��yA��mA��yA��A��A��A��A��A��`A��AA��A�"�A���A���A�?}A�A��A��A��A��
A��HA���A��^A�K�A���A��
A�ZA�1A���A�\)A�VA�t�A�$�A���A�ȴA��A�ĜA�ffA���A��^A� �A�;dA�A���A���A�9XA���A��HA���A���A�ĜA��uA��RA�C�A�M�A���A�S�A�ĜA�x�A�+A��+A���A�VA�O�A���A��jA��;A�C�A��9A��^A��^A��wA�VA���A��-A���A�VAz�/Ax�`Aw�PAu�AsVArr�Ap�/An�Ak�Aj�`Ai��Ah��Ag|�Ac7LAbr�Aa/A_?}A[+AVr�AU&�AT9XAR��AM&�AKAH��AE�ADȴAC�-AB�!ABE�A@�`A>ffA=hsA=\)A=O�A;�A:JA8��A6�yA4��A3�A2��A2ffA2  A0ĜA0A/VA.JA-\)A,�A,JA+��A+&�A*��A)�A)G�A(z�A'O�A&-A%\)A$�A$=qA#�PA!�A!oA�AĜA��A�9AƨA�`AQ�A�PAI�A�A�AE�A�AĜAVA�^A/A��AJA��Ar�A�;A�PAt�A��A��AO�A�^A
��A	�A	dZA�-AA�A�!AZAt�A��A��A v�@��!@���@��`@�S�@�S�@�I�@�^5@�/@��@�M�@�-@���@�X@�@���@�G�@畁@��@柾@�r�@�@�Ĝ@�@�@���@���@�p�@��@��/@�Ĝ@���@�b@�5?@ܼj@ۍP@ؼj@�ȴ@�5?@պ^@պ^@�G�@���@ӝ�@�
=@�^5@щ7@ЋD@�1@�@˕�@��T@ɩ�@�V@�`B@�1'@��y@���@�K�@�l�@��@��j@��F@��@�1@��F@�+@��y@�ȴ@�~�@�$�@��^@�7L@��j@�Q�@�ƨ@���@��@�p�@��@�33@�V@���@��@�@��u@��u@��@��@�X@���@���@�1@��@���@���@�;d@�v�@��@��;@�|�@�dZ@�C�@�33@��+@��@�+@�K�@��m@��;@�ƨ@��@���@�ff@�@���@�A�@��@��m@���@��-@��-@�x�@�&�@��j@�(�@�;d@���@��@��H@��\@��#@��h@�G�@��u@�b@��
@���@��F@�\)@��@���@���@���@���@�ff@�5?@�{@��@���@��^@��-@��h@�p�@�/@�&�@��`@��9@�r�@�I�@�1'@�ƨ@�|�@�\)@�33@�=q@���@���@���@�?}@��@��@�j@�9X@��w@�"�@��
@�ƨ@�l�@���@�n�@��^@�G�@��j@�X@�@�Ĝ@�bN@��-@�x�@�r�@�@�n�@��@�ff@�J@���@�  @�K�@��@���@��+@�E�@���@�x�@�?}@���@�Ĝ@�bN@�I�@� �@�1@�  @�  @���@�ƨ@��F@��P@��@�dZ@��@�@���@���@��@�ȴ@���@�$�@��@���@�{@�$�@�J@��@��@��@��T@���@�@��7@�p�@�O�@��@��/@���@��j@��u@�r�@��m@�;d@��@�@�n�@�v�@�V@�J@��-@�?}@��@��@�j@�Q�@� �@�  @��m@��w@���@��@�S�@�;dG�O�@���@x�@oo@gn/@a!�@Zxl@R�@M��@Go�@D  @<K^@6�@0�I@-2a@'n/@#�F@�t@�]@�?@P�@	+�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��B
��B
�`B
�BbB�B-B=qBaHBiyBm�Bq�B{�B�7B�uB�bB�bB��B��B�jB�)B�B��BB1B
=B
=B��B�B�B�B�yB�5B��BĜB�?B��B��B��B��B}�B�VBǮB%BbBPB��B�B�BȴB��B~�Bm�B_;B^5BffBu�Bl�BZB!�B
�fB
�qB
�B
��B
�PB
w�B
R�B
0!B
�B	�B	�B	��B	ƨB	�^B	�RB	�B	��B	�+B	{�B	r�B	jB	ffB	YB	N�B	G�B	>wB	,B	VB	1B	B��B�sB�BB�#B��B��B��B��B��B��B��B��B��B��B�wB�?B�B�!B��B��B��B��B��B��B��B�uB�\B�PB�7B�+B�B�B�B�B� B}�B|�By�Bv�Bs�Bs�Br�Bl�BhsBgmBcTB_;B_;BYBW
BT�BW
BXBW
BXBgmB{�B��B�3B��B�B�
B�NB�ZB�/B��BǮB�LB�FB�-B�B�B��B��B�{B�VB�1B|�B}�Bu�Bp�BhsB_;BVBQ�BQ�BO�BA�B9XB49B1'B1'B/B/B/B/B.B0!B1'B33B6FB49B8RB>wBL�BW
BXBXBR�BM�BN�BYB]/B^5B`BB]/BZBYBT�BN�BN�BO�BYB[#B]/B]/B]/BcTBcTBffBiyBk�Bk�Bk�Bp�Bx�B}�Bx�Bu�Bu�Br�Bt�Bt�Bu�Bw�By�B�B�+B�%B�%B�%B�B�%B�7B�DB�JB�VB�bB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B�3B�9B�9B�^BÖB��B�#B�B�
B��B��B��B��B�B�#B�`B�B��B	B	B	B	+B		7B		7B	
=B	\B	hB	oB	oB	uB	uB	�B	�B	�B	�B	�B	�B	"�B	.B	1'B	2-B	5?B	6FB	7LB	9XB	<jB	<jB	;dB	=qB	>wB	A�B	A�B	A�B	A�B	B�B	D�B	F�B	I�B	L�B	M�B	M�B	M�B	N�B	O�B	S�B	VB	[#B	]/B	aHB	bNB	cTB	e`B	gmB	gmB	hsB	iyB	k�B	l�B	n�B	r�B	r�B	s�B	s�B	s�B	s�B	t�B	y�B	|�B	}�B	|�B	{�B	{�B	z�B	y�B	}�B	�B	�B	�B	�VB	�hB	�oB	�\B	�PB	�VB	�hB	�uB	�hB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�3B	�?B	�?B	�FB	�XB	�^B	�jB	�qB	�wB	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�/B	�)B	�)B	�#B	�)B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�B	�$B
�B
�B
#�B
)�B
.�B
8�B
AB
HKB
K�B
R�B
WsB
[�B
]�B
abB
b�B
h
B
o5B
uB
y>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
��B
��B
�UB
�BSB�B �B1_BU2B]cBa{Be�Bo�B}B�\B�JB�JB��B��B�OB�B�xB��B��B�B�B�B��B�B�B�sB�\B�B��B��B�(B��B��B��B��Bq�B�DB��B�BDB2B�B�pB��B��B��Br�Ba�BS/BR)BZZBi�B`BNB�B
�gB
�wB
�B
��B
�[B
k�B
GB
$6B
	�B	�B	�<B	��B	��B	��B	�uB	�,B	��B	{SB	pB	f�B	^�B	Z�B	MEB	CB	;�B	2�B	 ;B	�B�hB�>B�,BܮB�~B�`B�0B�B�B�B�B�B��B��B��B��B��B��B�XB�eB�5B�B��B��B��B��B��B��B��B��B}�B{uByiBxcBvVBvVBtKBr?Bq9Bn'BkBhBhBf�B`�B\�B[�BW�BS�BS�BMhBK[BIOBK[BLaBK\BLbB[�Bp5B�B�|B�%B�JB�PB֓B؟B�uB�9B��B��B��B�xB�fB�NB�B��B��B��B|�Bq@BrFBjBd�B\�BS�BJZBFCBFCBD6B5�B-�B(�B%�B%�B#vB#vB#vB#vB"oB$|B%�B'�B*�B(�B,�B2�BA&BKbBLhBLhBGKBB-BC3BMpBQ�BR�BT�BQ�BNvBMpBIXBC4BC4BD:BMqBO}BQ�BQ�BQ�BW�BW�BZ�B]�B_�B_�B_�Bd�Bm-BrLBm-BjBjBg	BiBiBjBl(Bn4BxqB{�Bz}Bz}Bz}BywBz}B}�B�B��B��B��B��B��B��B��B�B�B��B��B��B��B��B�	B�B�LB��B��B��B��B��B�&B�uB�oB�]B�EB�?B�EB�EB�WB�uBٱB��B�%B�UB�nB�nB�yB��B��B��B	�B	�B	�B	�B	�B	�B		�B	�B	�B	�B	�B	B	B	"`B	%sB	&xB	)�B	*�B	+�B	-�B	0�B	0�B	/�B	1�B	2�B	5�B	5�B	5�B	5�B	6�B	8�B	:�B	>B	AB	BB	BB	BB	C"B	D(B	HAB	JMB	OkB	QwB	U�B	V�B	W�B	Y�B	[�B	[�B	\�B	]�B	_�B	`�B	b�B	f�B	f�B	g�B	g�B	g�B	g�B	iB	n!B	q4B	r:B	q4B	p-B	p-B	o'B	n!B	r:B	ydB	ydB	ydB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�&B	�?B	�EB	�QB	�WB	�WB	�^B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�,B	�2B	�8B	�=B	�=B	�8B	�>B	�IB	�VB	�\B	�hB	�nB	�nB	�hB	�hB	�bB	�hB	�nB	�tB	�tB	�zB	�zB	�zB	ԁB	ԁB	ՇB	ՇG�O�B	�@B	�aB	�
B
,B
�B
�B
#B
,�B
5?B
<�B
@4B
G*B
K�B
P-B
Q�B
U�B
V�B
\BB
cmB
iBB
mu111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170922    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170922  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170922  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                