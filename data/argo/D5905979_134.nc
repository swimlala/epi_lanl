CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:25Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170925  20220204114424  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��C�%1   @���}7@7
~��"��b�n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D��D�Y�D���D�ӅD�'\D�VfD��fD�� D�#3D�W
D���DǬ)D�
D�S�Dڧ\D��D�'\D�i�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�=q@�
>A�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B�=qB�=qB���B���B���B���B�p�B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��
B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%�C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy�RC{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;nD;�D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{DtǮDy�
D��D�S�D��3D���D�!�D�P�D���D��=D�pD�QGD��
DǦfD�GD�M�Dڡ�D���D�!�D�d)D�
D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��RA���A���A���A��#A��A��A��;A��/A��A��HA��HA��TA��TA��TA��`A��`A��`A��mA��yA��yA��yA��A��A��TA���A���A�A��wA���A���A�A��jA�bNA���A��HA�A�A�G�A��mA�33A��^A�VA���A�hsA�O�A�7LA�&�A��A�bA�%A���A��A��`A�ȴA��-A�~�A�?}A��
A��PA���A�~�A�JA��hA�;dA�K�A���A�XA��FA��A��A�|�A�r�A���A���A��A��HA���A��A�&�A��A���A��\A�=qA�33A���A��hA�p�A�|�A��A�n�A�ZA�?}A�C�A�jA��TA��TA���A�E�A�dZA��`A��HA�1'A�v�A���A� �A��A���A�A�l�A�A}��A{+Ax��AvI�AtQ�Aq�Ao&�AmAk�Ai��Ah{Ae�PAdn�AcG�Aa�A`A_�FA^ĜA[�7AZ~�AYK�AU�AT�DAS��AS33ARA�AO�-AM��AL��AKl�AJn�AHA�AF5?AE�AEO�AC/A?`BA=t�A<ȴA<r�A<A;��A:�9A8VA5��A4�\A2n�A/��A-p�A+��A+S�A*�jA)A(I�A'��A&z�A%?}A$E�A#�wA#p�A!�TA!�A ~�A Q�A|�AAK�A��A�7A�A�PA`BAA��A�`A��AC�A�A�uA{AA$�A�hA�A�wA�HAS�A
1'A�`A��A+A�uA{A�jA�A  A�#A�-A�PAo@��@��m@�\)@���@��@��\@���@�@��@�@�$�@�u@�+@�=q@�7@�z�@�+@�I�@�\@���@�O�@���@���@�~�@���@�ȴ@�=q@��@�hs@�C�@�~�@�M�@�{@Ձ@�|�@�@�x�@ЋD@��@͉7@�ƨ@�V@�p�@�o@�M�@��#@�G�@�Q�@å�@�@�?}@��/@��D@���@���@�E�@�`B@�j@���@��P@�|�@�S�@��@�-@��-@��-@��@��/@�1@�33@�o@���@��@��y@��H@�ȴ@���@���@�Z@��w@�dZ@���@��H@�M�@��@���@��9@� �@���@�S�@���@���@�$�@�G�@���@���@�A�@��D@��@�A�@��@�1'@�  @��@���@�\)@�^5@���@�`B@��`@��@��^@��@�Ĝ@��;@��F@��H@�~�@��@��`@�r�@�|�@���@�n�@���@��^@��T@�{@�J@�O�@���@�l�@�M�@�~�@�n�@�n�@��#@�/@��@�(�@�  @�  @�I�@�ƨ@���@�^5@�V@�n�@���@��+@���@�5?@�M�@��^@���@��!@�M�@�J@�@���@��-@���@�x�@�hs@�/@��@�v�@���@�E�@��@�\)@��w@�l�@���@��@���@�~�@�M�@�=q@�-@�{@�@�@��h@�O�@��@���@�z�@�9X@�  @��F@���@��P@��@�t�@�l�@�dZ@�K�@�C�@�+@���@�ȴ@��!@�ff@�-@�J@���@�O�@���@�(�@�b@��m@�|�@�"�@�"�@�33@��@�
=@�
=@���@���@��+@�n�@�V@�5?@��@��h@�G�@�&�@��@���@���@��`@���@���@�Z@�(�@�  @�1@��m@���@�dZ@�K�@�o@���@�V@��@��#@��-@�x�@�/@�V@��@���@��@��D@�r�@�1'@�  @�ƨ@���@�dZ@�+@��@���@��+@�v�@�=q@��@��^@���@��@�7L@��@��@��@�j@�A�@�9X@��D@{�0@t��@mS&@dq@[��@Sb�@KP�@D�@?�
@:J@3v`@.�'@(r�@"i�@@��@s�@4n@
.�@�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��RA���A���A���A��#A��A��A��;A��/A��A��HA��HA��TA��TA��TA��`A��`A��`A��mA��yA��yA��yA��A��A��TA���A���A�A��wA���A���A�A��jA�bNA���A��HA�A�A�G�A��mA�33A��^A�VA���A�hsA�O�A�7LA�&�A��A�bA�%A���A��A��`A�ȴA��-A�~�A�?}A��
A��PA���A�~�A�JA��hA�;dA�K�A���A�XA��FA��A��A�|�A�r�A���A���A��A��HA���A��A�&�A��A���A��\A�=qA�33A���A��hA�p�A�|�A��A�n�A�ZA�?}A�C�A�jA��TA��TA���A�E�A�dZA��`A��HA�1'A�v�A���A� �A��A���A�A�l�A�A}��A{+Ax��AvI�AtQ�Aq�Ao&�AmAk�Ai��Ah{Ae�PAdn�AcG�Aa�A`A_�FA^ĜA[�7AZ~�AYK�AU�AT�DAS��AS33ARA�AO�-AM��AL��AKl�AJn�AHA�AF5?AE�AEO�AC/A?`BA=t�A<ȴA<r�A<A;��A:�9A8VA5��A4�\A2n�A/��A-p�A+��A+S�A*�jA)A(I�A'��A&z�A%?}A$E�A#�wA#p�A!�TA!�A ~�A Q�A|�AAK�A��A�7A�A�PA`BAA��A�`A��AC�A�A�uA{AA$�A�hA�A�wA�HAS�A
1'A�`A��A+A�uA{A�jA�A  A�#A�-A�PAo@��@��m@�\)@���@��@��\@���@�@��@�@�$�@�u@�+@�=q@�7@�z�@�+@�I�@�\@���@�O�@���@���@�~�@���@�ȴ@�=q@��@�hs@�C�@�~�@�M�@�{@Ձ@�|�@�@�x�@ЋD@��@͉7@�ƨ@�V@�p�@�o@�M�@��#@�G�@�Q�@å�@�@�?}@��/@��D@���@���@�E�@�`B@�j@���@��P@�|�@�S�@��@�-@��-@��-@��@��/@�1@�33@�o@���@��@��y@��H@�ȴ@���@���@�Z@��w@�dZ@���@��H@�M�@��@���@��9@� �@���@�S�@���@���@�$�@�G�@���@���@�A�@��D@��@�A�@��@�1'@�  @��@���@�\)@�^5@���@�`B@��`@��@��^@��@�Ĝ@��;@��F@��H@�~�@��@��`@�r�@�|�@���@�n�@���@��^@��T@�{@�J@�O�@���@�l�@�M�@�~�@�n�@�n�@��#@�/@��@�(�@�  @�  @�I�@�ƨ@���@�^5@�V@�n�@���@��+@���@�5?@�M�@��^@���@��!@�M�@�J@�@���@��-@���@�x�@�hs@�/@��@�v�@���@�E�@��@�\)@��w@�l�@���@��@���@�~�@�M�@�=q@�-@�{@�@�@��h@�O�@��@���@�z�@�9X@�  @��F@���@��P@��@�t�@�l�@�dZ@�K�@�C�@�+@���@�ȴ@��!@�ff@�-@�J@���@�O�@���@�(�@�b@��m@�|�@�"�@�"�@�33@��@�
=@�
=@���@���@��+@�n�@�V@�5?@��@��h@�G�@�&�@��@���@���@��`@���@���@�Z@�(�@�  @�1@��m@���@�dZ@�K�@�o@���@�V@��@��#@��-@�x�@�/@�V@��@���@��@��D@�r�@�1'@�  @�ƨ@���@�dZ@�+@��@���@��+@�v�@�=q@��@��^@���@��@�7L@��@��@��@�j@�A�G�O�@��D@{�0@t��@mS&@dq@[��@Sb�@KP�@D�@?�
@:J@3v`@.�'@(r�@"i�@@��@s�@4n@
.�@�b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ÖB
B
ÖB
ÖB
B
B
B
B
B
B
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
B
��B
��B
B
B
ÖB
ÖB
ÖB
ƨB
�BB
��BN�B��B�FB�XB��B�B\B�B)�B-B0!B0!B0!B0!B0!B1'B1'B0!B.B,B.B1'B-B5?BH�BL�BXB[#BB�B@�Be`Bv�B� By�Bk�Bo�B`BBO�BJ�BB�B7LB,B!�B�BB��B�B�TB��BĜB�?B��B��B�B� B}�By�BhsBQ�BI�B33B�BB
��B
�sB
�#B
�dB
��B
�hB
�%B
z�B
p�B
e`B
\)B
M�B
=qB
%�B
{B
%B	�B	�B	ȴB	�wB	�!B	��B	��B	�B	}�B	u�B	o�B	ffB	aHB	^5B	K�B	?}B	:^B	'�B	�B	{B	oB	bB	B��B�B�B�`B�BȴBĜB��B�wB�-B�B��B��B��B��B��B��B�7B�B� Bw�Bw�Bp�Bm�BiyBaHBW
BS�BT�BVBZB[#B\)B`BB]/B\)BYBVBN�BJ�BL�BL�BI�BF�BF�BE�BD�B@�BA�B>wB>wB>wBH�BE�BD�BC�B>wB=qB:^B49B2-B1'B.B-B,B+B+B&�B%�B%�B$�B#�B#�B$�B$�B#�B"�B$�B$�B#�B#�B#�B �B�B!�B!�B"�B"�B!�B"�B%�B%�B&�B'�B&�B&�B'�B,B-B-B.B.B0!B33B5?B6FB6FB<jB<jB;dB=qB>wB>wB@�BB�BF�BL�BM�BM�BM�BN�BN�BP�BR�BR�BS�BXB\)B`BBcTBffBgmBhsBhsBhsBk�Bo�Bp�Bq�Bs�Bv�Bx�Bz�Bz�B{�B}�B~�B� B�B�B�B�1B�7B�=B�DB�DB�bB�{B��B��B��B��B��B��B�B�!B�-B�9B�FB�qBƨBǮB��B��B��B��B�B�5B�BB�TB�ZB�mB�B�B��B	B	B	%B	DB	PB	VB	\B	\B	hB	oB	oB	uB	�B	�B	 �B	%�B	'�B	'�B	,B	+B	(�B	+B	.B	0!B	1'B	2-B	33B	6FB	9XB	;dB	>wB	C�B	A�B	A�B	D�B	G�B	L�B	M�B	O�B	O�B	R�B	T�B	aHB	cTB	gmB	m�B	p�B	q�B	r�B	r�B	r�B	r�B	r�B	r�B	r�B	s�B	v�B	|�B	�B	�%B	�1B	�1B	�JB	�PB	�VB	�VB	�\B	�\B	�\B	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�'B	�-B	�3B	�9B	�FB	�FB	�RB	�RB	�RB	�XB	�dB	�dB	�dB	�jB	�wB	�}B	��B	B	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�NB	�TB	�`B	�`B	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
�B
(B
1B
#TB
($B
2-B
<PB
@iB
F�B
K�B
SuB
UMB
[#B
_�B
c�B
h$B
j�B
oOB
uZB
yX111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�DB
�>B
�DB
�DB
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�8B
�8B
�=B
�=B
�DB
�DB
�DB
�VB
��B
�BAzB�!B��B��B�oB�+B�BLB�B�B"�B"�B"�B"�B"�B#�B#�B"�B �B�B �B#�B�B'�B;?B?WBJ�BM�B5B3BW�BiQBr�BldB^Bb(BR�BBmB=QB5 B)�B�B`B)B��B�B�KB��B�xB�<B��B�vB�-Bu�Br�Bp�Bl�B[BD�B<iB%�BZB
��B
�uB
�-B
��B
�$B
��B
�,B
x�B
m�B
clB
X*B
N�B
@�B
0?B
�B
NB	��B	�|B	��B	��B	�UB	�B	��B	��B	v�B	p�B	h�B	b�B	YOB	T1B	QB	>�B	2kB	-LB	�B	�B	nB	bB	VB�B��B�B�xB�YB��B��B��B��B�vB�.B�
B��B��B��B��B��B��B|=Bx&BsBj�Bj�Bc�B`�B\�BTUBJBGBHBIBM+BN1BO7BSPBP>BO8BL&BIBA�B=�B?�B?�B<�B9�B9�B8�B7�B3�B4�B1�B1�B1�B;�B8�B7�B6�B1�B0�B-tB'PB%DB$?B!,B 'B!BBBB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBBBBBBB&B ,B ,B!2B!2B#?B&QB(]B)dB)dB/�B/�B.�B0�B1�B1�B3�B5�B9�B?�B@�B@�B@�BA�BA�BDBFBFBGBK-BOEBS^BVpBY�BZ�B[�B[�B[�B^�Bb�Bc�Bd�Bf�Bi�Bk�Bm�Bm�BoBqBrBsBt Bt Bw2B{JB|PB}VB~]B~]B�zB��B��B��B��B��B��B� B�B�7B�CB�OB�[B��B��B��B��B��B��B�B�)B�GB�SB�eB�kB�~BޕB��B��B�B�'B�3B�QB	 ]B	cB	iB	iB	uB	|B	|B	�B	�B	�B	�B	�B	�B	�B	B	B	B	B	!B	#,B	$2B	%8B	&>B	)QB	,bB	.nB	1�B	6�B	4�B	4�B	7�B	:�B	?�B	@�B	B�B	B�B	E�B	HB	TNB	VZB	ZsB	`�B	c�B	d�B	e�B	e�B	e�B	e�B	e�B	e�B	e�B	f�B	i�B	o�B	uB	y(B	{4B	{4B	MB	�RB	�XB	�XB	�^B	�^B	�^B	�jB	�jB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�B	�'B	�-B	�2B	�8B	�EB	�EB	�QB	�QB	�QB	�WB	�cB	�cB	�cB	�iB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�1B	�7B	�>B	�>B	�DB	�JB	�JB	�PB	�[B	�[B	�hB	�nB	�nB	�nB	�tB	ހB	ހB	ހB	߆B	��B	�B	�B	�B	�B	�G�O�B	��B	��B
!B
)B
KB
B
%$B
/FB
3_B
9�B
>�B
FjB
HBB
NB
R�B
V�B
[B
]�B
bCB
hNB
lL111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.013(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144242022020411442420220204114424  AO  ARCAADJP                                                                    20200619170925    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170925  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170925  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114424  IP                  G�O�G�O�G�O�                