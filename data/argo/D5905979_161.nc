CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-07-09T14:01:08Z creation      
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �0   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �D   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �\   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20200709140108  20220204114427  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @�'\���1   @�']ww�0@6ix����b߅�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@���A   A   A@  A`  A�  A�  A���A�  A�  A�  A���A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D,��D-y�D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�$�D�]�D���D�ۅD�  D�R�D��RD��RD��D�W\D��D�ȤD�(�D�UDڊ�D�� D�'�D�b=D�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
>@�
>@�=qA�A=�A]�A}�A��\A�\)A��\A��\AΏ\A�\)A�\A�BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B��
B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�B�p�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C�C�C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ�RCS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C�RC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�D-nD-�{D.t{D.�{D/t{D/�{D0t{D0�D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4��D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS��DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dy�3D�
D�X D���D���D�=D�MD���D���D��D�Q�D��\D���D�"�D�O\Dڄ�D��=D�!�D�\zD��D��
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˲-A˶FA˰!A˸RA˺^A˺^A˼jA˺^A˺^A˺^A˺^A˼jA˺^A˶FA˸RA˝�A�p�A�E�A��Aʰ!A�ffA�A�dZA��;A�~�A�(�A�A���Aơ�A�O�A�A��mA��A�1A�bA�{A���A��#A�G�A���A�
=A�?}A��yA�M�A�Q�A�dZA�p�A���A�oA��hA���A��/A�oA�l�A���A�ȴA�I�A�
=A��A�O�A���A�K�A�ȴA��A�\)A��HA���A���A��A�ffA��mA�S�A���A��A�{A�x�A��A�r�A�C�A���A�^5A��A�S�A��A��-A��HA��+A�C�A��hA��/A��A�dZA��RA��A�~�A���A��A���A�^5A��A���A��A�\)A�hsA�|�A��A�ȴA�K�A�JA�9XA�"�A��FA�+A���A�t�A���A�A�(�A���A�^5A���A���A��/A~bAy�hAw�mAv(�AqO�Ao��An5?Al�uAk
=Ahv�Ag�^Ae�7Ac�Aa\)A\bNAY�wAV�HAU"�AS�AQ��AP�RAO��AMC�AK�wAJ�AH�`AFȴAE�mAE7LAC33AA�A?�A<�!A;�A;`BA:�A:z�A9��A9�A7t�A5��A49XA2ȴA0~�A/�FA.ZA-��A-�A-K�A,$�A*Q�A(��A(bA'x�A&bNA%��A%dZA$r�A"�!A!�A!A ��A �uA 5?A;dA�+A�AhsAoAZA��A�!AZA`BA$�A-A~�A�FAE�AhsA��A�TAO�A��A�A$�A/A
I�A	�
A	��A	O�A�A�A�FA��AbA��AE�A��A��AXA%AXA�A�A�\AAG�A�#A�DA��AXA V@�ff@��@�Ĝ@�$�@���@�ff@��@��@��@�b@�F@�@��@�^5@�7@��@�X@�7@���@�
=@�$�@ّh@�%@�1'@�Q�@�`B@�%@ؓu@�Z@�S�@�p�@Ӯ@�n�@�=q@�5?@�5?@���@���@��@�\)@��@���@ɩ�@�7L@�33@��@î@���@�(�@�@�O�@��@�ƨ@�;d@�M�@��^@��@��\@���@���@���@���@��h@��@�r�@�1'@��F@�
=@��@�?}@�?}@��9@�r�@��@�t�@��@��\@��y@�K�@�S�@�ȴ@�^5@���@��u@��m@��;@���@�K�@�o@���@�^5@��T@�x�@�O�@�/@��`@���@��@�z�@�b@�K�@��R@�ff@�5?@���@���@��h@�/@���@���@���@���@�x�@�&�@���@�V@��`@���@�1@��@�\)@�C�@�
=@��@�=q@��@��@��9@���@�r�@�1@��;@�C�@�@��@��+@�ff@�E�@��@���@�`B@�7L@�%@���@���@�bN@�1'@�1@���@�\)@�
=@�
=@�
=@��@��+@�V@�J@��@�@��@�/@��@��9@��u@��@�(�@�ƨ@��@��@���@�1@��@�|�@��+@�{@���@��@���@�Ĝ@��u@�r�@�z�@���@�X@�%@��@��j@��@�Ĝ@���@��D@��@�@��@��!@���@���@��7@�x�@�`B@�?}@���@���@�j@� �@�1@�1@�1@�  @�ƨ@�|�@�33@�
=@��H@��+@�ff@�$�@�$�@�J@���@�&�@���@��@�z�@�bN@�A�@�9X@�1'@��@���@��
@�ƨ@��@�l�@�;d@��@��H@��!@�n�@�M�@�=q@�5?@�$�@�J@��@�X@�V@�Ĝ@��D@�1'@���@�l�@�*�@|�o@s@O@ic@_��@W��@Q�~@M�@G~�@A��@; i@5��@/��@)hs@#�g@E9@�K@��@�@8@"h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A˲-A˶FA˰!A˸RA˺^A˺^A˼jA˺^A˺^A˺^A˺^A˼jA˺^A˶FA˸RA˝�A�p�A�E�A��Aʰ!A�ffA�A�dZA��;A�~�A�(�A�A���Aơ�A�O�A�A��mA��A�1A�bA�{A���A��#A�G�A���A�
=A�?}A��yA�M�A�Q�A�dZA�p�A���A�oA��hA���A��/A�oA�l�A���A�ȴA�I�A�
=A��A�O�A���A�K�A�ȴA��A�\)A��HA���A���A��A�ffA��mA�S�A���A��A�{A�x�A��A�r�A�C�A���A�^5A��A�S�A��A��-A��HA��+A�C�A��hA��/A��A�dZA��RA��A�~�A���A��A���A�^5A��A���A��A�\)A�hsA�|�A��A�ȴA�K�A�JA�9XA�"�A��FA�+A���A�t�A���A�A�(�A���A�^5A���A���A��/A~bAy�hAw�mAv(�AqO�Ao��An5?Al�uAk
=Ahv�Ag�^Ae�7Ac�Aa\)A\bNAY�wAV�HAU"�AS�AQ��AP�RAO��AMC�AK�wAJ�AH�`AFȴAE�mAE7LAC33AA�A?�A<�!A;�A;`BA:�A:z�A9��A9�A7t�A5��A49XA2ȴA0~�A/�FA.ZA-��A-�A-K�A,$�A*Q�A(��A(bA'x�A&bNA%��A%dZA$r�A"�!A!�A!A ��A �uA 5?A;dA�+A�AhsAoAZA��A�!AZA`BA$�A-A~�A�FAE�AhsA��A�TAO�A��A�A$�A/A
I�A	�
A	��A	O�A�A�A�FA��AbA��AE�A��A��AXA%AXA�A�A�\AAG�A�#A�DA��AXA V@�ff@��@�Ĝ@�$�@���@�ff@��@��@��@�b@�F@�@��@�^5@�7@��@�X@�7@���@�
=@�$�@ّh@�%@�1'@�Q�@�`B@�%@ؓu@�Z@�S�@�p�@Ӯ@�n�@�=q@�5?@�5?@���@���@��@�\)@��@���@ɩ�@�7L@�33@��@î@���@�(�@�@�O�@��@�ƨ@�;d@�M�@��^@��@��\@���@���@���@���@��h@��@�r�@�1'@��F@�
=@��@�?}@�?}@��9@�r�@��@�t�@��@��\@��y@�K�@�S�@�ȴ@�^5@���@��u@��m@��;@���@�K�@�o@���@�^5@��T@�x�@�O�@�/@��`@���@��@�z�@�b@�K�@��R@�ff@�5?@���@���@��h@�/@���@���@���@���@�x�@�&�@���@�V@��`@���@�1@��@�\)@�C�@�
=@��@�=q@��@��@��9@���@�r�@�1@��;@�C�@�@��@��+@�ff@�E�@��@���@�`B@�7L@�%@���@���@�bN@�1'@�1@���@�\)@�
=@�
=@�
=@��@��+@�V@�J@��@�@��@�/@��@��9@��u@��@�(�@�ƨ@��@��@���@�1@��@�|�@��+@�{@���@��@���@�Ĝ@��u@�r�@�z�@���@�X@�%@��@��j@��@�Ĝ@���@��D@��@�@��@��!@���@���@��7@�x�@�`B@�?}@���@���@�j@� �@�1@�1@�1@�  @�ƨ@�|�@�33@�
=@��H@��+@�ff@�$�@�$�@�J@���@�&�@���@��@�z�@�bN@�A�@�9X@�1'@��@���@��
@�ƨ@��@�l�@�;d@��@��H@��!@�n�@�M�@�=q@�5?@�$�@�J@��@�X@�V@�Ĝ@��D@�1'@���G�O�@�*�@|�o@s@O@ic@_��@W��@Q�~@M�@G~�@A��@; i@5��@/��@)hs@#�g@E9@�K@��@�@8@"h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
P�B
P�B
O�B
P�B
P�B
P�B
Q�B
P�B
Q�B
P�B
Q�B
R�B
R�B
T�B
T�B
cTB
m�B
t�B
x�B
�B
�1B
��B
��B
��B
�9B
�LB
�9B
�-B
�9B
�B
�B
�B
�?B
�qB
�}B
B
ĜB
ŢB
�dB
�FB
�3B
�B
��B
�{B
��B
�7B
z�B
�PB
��B
�#B
��B!�B/B)�B,B#�BDB�BP�Bx�B��B��B�qBǮBŢB��B��B�TB�B��B��B  B	7B{B�B#�B/B:^B=qB:^B5?B6FB?}BE�BL�B[#Bm�Bz�B�B�B{�Bl�B]/BA�B6FB49B2-B#�B)�B�B1B�B�B��BɺB��B�9B�B�B��B�+BS�B�B
�B
��B
�}B
�B
�\B
� B
� B
y�B
w�B
\)B
A�B
�B
\B
+B	�HB	��B	ǮB	�jB	�'B	��B	�uB	�B	s�B	ffB	H�B	-B	�B	
=B	B��B�B�yB�HB�#B�
B��BɺBÖB�wB�3B��B��B�oB�PB�JB�DB�PB�VB�\B��B��B�\B�hB�bB�bB�VB�DB�JB�\B�oB�VB�%B�B�B~�Bz�Bw�Bv�Bv�Bu�Bw�Bt�Bu�Bu�Bv�Bt�Bx�Bz�B�B�Bq�BjBgmB\)BYB^5BhsBiyBdZB`BB\)B\)BbNBbNB_;BZBS�BN�BK�BI�BI�BE�BB�B@�B=qB9XB<jBA�BG�BL�BM�BR�B`BB��B��B��B��B��B��B��B��B��B��B�{B�\B�%B{�Bo�BiyBcTB^5B[#B^5B_;B`BB_;B^5B]/BYBXBO�B@�B9XB:^B:^BD�BI�BO�B^5BgmBm�Bp�Bs�Bs�Bs�Bv�Bv�Bw�By�B{�B}�B~�B{�By�Bx�Bx�B{�B� B�B�1B�JB�PB�PB�PB�DB�7B�1B�7B�1B�7B�DB�PB�VB�{B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�'B�'B�3B�FB�XB�dB�^B�^B�qB��BĜBɺB��B��B��B�B�)B�;B�;B�BB�NB�TB�ZB�TB�mB�B�B�B��B��B��B��B	  B	B	B	B	PB	oB	uB	uB	uB	uB	uB	�B	�B	�B	�B	!�B	$�B	&�B	)�B	.B	33B	5?B	8RB	<jB	=qB	D�B	I�B	L�B	O�B	Q�B	R�B	S�B	XB	\)B	^5B	`BB	bNB	dZB	gmB	iyB	k�B	m�B	o�B	q�B	q�B	t�B	u�B	x�B	y�B	}�B	� B	�B	�B	�+B	�7B	�DB	�JB	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�3B	�RB	�^B	�jB	�wB	��B	��B	��B	ĜB	ĜB	ÖB	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�)B	�/B	�/B	�;B	�BB	�HB	�NB	�NB	�ZB	�`B	�fB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
�B
�B
"�B
+kB
4B
<�B
B�B
I7B
K�B
S�B
X�B
]�B
cTB
h�B
l�B
o�B
s�B
y	B
|�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
@vB
@vB
?pB
@vB
@vB
@vB
A}B
@vB
A}B
@vB
A}B
B�B
B�B
D�B
D�B
R�B
]B
dHB
haB
r�B
w�B
�XB
�SB
�MB
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
�B
�B
� B
�&B
��B
��B
��B
��B
�BB
�B
�B
x�B
jqB
|�B
�tB
ʧB
�PBFB�BwB�BSB
��B
B@\BhGB��B�iB��B�B�B�*B�ZBһB��B�!B�?B�dB��B�BB7ByB)�B,�B)�B$�B%�B.�B4�B<)BJ}B\�Bj8BsoBqbBk>B[�BL�B0�B%�B#�B!�B;B`BB��B�BȆB�]B�,B�3B��B�yB��B�Bv�BC|B2B
�<B
�|B
�B
��B
~�B
o�B
o�B
iB
gsB
K�B
15B
jB	�B	��B	�B	õB	�mB	�*B	��B	�}B	�;B	t�B	c�B	V3B	8�B	�B	�B�B��B�B�wB�YB�*B�B��B��B��B�}B�_B�B��B��B�^B}@B|:B{4B}@B~FBLB��B�}BMB�YB�TB�TB~IB{7B|=BOB�bB~JBvBsBrBn�Bj�Bg�Bf�Bf�Be�Bg�Bd�Be�Be�Bf�Bd�Bh�Bj�Bp�Bp�Ba�BZ{BWjBL(BIBN4BXpBYvBTXBPABL)BL)BRMBRMBO;BJBC�B>�B;�B9�B9�B5�B2�B0�B-wB)_B,pB1�B7�B<�B=�BB�BPDB��B��B��B��B��B��B��B��B��B��B�xBZBv$Bk�B_�BY}BSYBN;BK)BN;BOABPHBOABN;BM5BIBHB?�B0�B)eB*kB*kB4�B9�B?�BN=BWtB]�B`�Bc�Bc�Bc�Bf�Bf�Bg�Bi�Bk�Bm�Bo Bk�Bi�Bh�Bh�Bk�BpBtBx7B|PB}VB}VB}VB{KBy>Bx8By>Bx8By>B{KB}WB~]B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�+B�+B�7B�JB�[B�gB�aB�aB�tB��B��B��B��B��B��B�B�)B�:B�:B�AB�MB�SB�YB�SB�lB܉B�B�B��B��B��B��B��B�B�B�B�JB	iB	oB	oB	oB	oB	oB	zB		�B	�B	�B	�B	�B	�B	�B	B	#)B	%5B	(HB	,_B	-fB	4�B	9�B	<�B	?�B	A�B	B�B	C�B	HB	LB	N&B	P3B	R?B	TKB	W]B	YiB	[uB	]�B	_�B	a�B	a�B	d�B	e�B	h�B	i�B	m�B	o�B	q�B	uB	wB	y$B	{1B	|6B	~BB	�NB	�aB	�gB	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�EB	�QB	�^B	�jB	�oB	�oB	��B	��B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�1B	�=B	�CB	�IB	�VB	�VB	�VB	�bB	�bB	�bB	�bB	�gB	�mB	�sB	�sB	�zB	�zB	��B	߀B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�G�O�B	��B	��B
�B
`B
IB
#�B
,bB
2kB
9B
;�B
C�B
H�B
MsB
S.B
X�B
\eB
_]B
c�B
h�B
lxB
nj11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.016(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144272022020411442720220204114427  AO  ARCAADJP                                                                    20200709140108    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200709140108  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200709140108  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114427  IP                  G�O�G�O�G�O�                