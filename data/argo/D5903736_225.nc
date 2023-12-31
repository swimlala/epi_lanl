CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-15T07:02:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180715070212  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�q��=�1   @�q���h@5}�E���d}7KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���AᙚA�33B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�)D���D�\�D�|{D���D��D�4�D�~D��3D��D�8�D�c3D��=D� D�8 D�~D�θD�	�D�K�D�vD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G@tz�@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\A�\)A�(�A�A��\BG�BG�B�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{DinDi�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{DtnDy��D��3D�W
D�v�D��D��)D�/
D�xQD��pD�3D�33D�]pD��zD�=D�2=D�xQD���D��D�FD�pQD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aպ^Aգ�AՓuAՉ7AՅA�n�A�hsA�VA�A�A�/A��TA��
A���Aԏ\A�  A���A���A˝�A�A�A��/Aé�A°!A��!A��!A��A��A��RA��
A���A��mA�"�A��^A�ZA��A��+A��A�p�A�=qA���A�ffA�;dA���A�|�A�VA�A���A��A���A�JA��-A��PA�{A�  A�Q�A��#A��7A�oA���A�r�A���A�C�A��\A��A��A���A���A�(�A�|�A��wA�C�A�r�A��A�VA�dZA���A��PA�ĜA�&�A���A�O�A��`A���A�VA��A��A��A�n�A��A��RA�JA��A���A��wA���A�r�A�bA���A�Q�A��A��!A�E�A��HA��^A��A+A~-A{�mAyK�Av�9At��Ar�Aq��Ap5?Ao�Akl�Ah�yAg��Af1'Ad�RA`E�A^�A["�AX �AWx�AWK�AW;dAW�AV�+AS��APv�AN��AM�AL�!AK��AI�PAI\)AIK�AI�AG�mAF�HAFVAE�wADffAB1'A?��A>�A>ZA=oA:�`A9"�A8�A7
=A5x�A4ȴA4ȴA4~�A3�A3
=A1
=A/dZA/%A.5?A-ƨA-��A,��A*�HA)��A(r�A'/A%��A#p�A"JA �DA   AC�A�-A��A?}A�PA�`A�;A��A��A��AQ�A��A�A��A=qA�TA�A;dA��AI�AC�A��A9XA�PAA
~�A	XA�A7LA{A�A=qA+AȴA��At�A33A �A �@�v�@��#@���@�{@��h@�S�@��#@�I�@�S�@�7@���@��@�+@�ff@�M�@�`B@�Z@��@�33@�^5@�p�@�Ĝ@�@�@�@�1'@�\)@�-@���@�@���@��/@���@�ff@�7L@���@֟�@�Ĝ@�r�@��@���@Ϯ@�`B@˝�@���@�$�@�?}@ȴ9@�I�@� �@��
@ȃ@�A�@�A�@���@��@���@�1'@�@�x�@�9X@��@�V@���@�  @�$�@���@�?}@��
@��H@�G�@��`@�hs@��@��@���@��7@�=q@��@�K�@���@���@���@��!@��@�
=@�
=@�5?@���@��w@���@���@�v�@�=q@�J@��#@�@���@��@�G�@���@�1'@��y@�V@��9@���@�/@���@��@���@��!@���@���@�?}@��;@���@�J@��^@�/@��`@��j@�I�@�ƨ@���@�S�@�"�@�
=@�o@�@���@�v�@���@���@�hs@�/@�%@��/@�Ĝ@���@��@�r�@�1@���@���@��F@�l�@�S�@�dZ@�\)@�+@�C�@�|�@�"�@���@�ȴ@���@��R@�M�@�J@���@�hs@�/@��@���@�Ĝ@��@�A�@�(�@�  @�\)@��#@�Ĝ@��@��@�Q�@�I�@��@���@��P@�\)@��F@�@���@�-@�@��@�@��@�7L@���@��@��@�S�@���@��@��P@�dZ@�K�@���@�$�@��\@�ȴ@���@���@���@�X@���@��9@�Ĝ@���@��`@��@���@���@���@���@�Q�@���@���@�"�@���@�-@��h@�`B@�&�@���@�Ĝ@��j@���@�r�@�A�@�9X@�1'@�(�@�1@��m@�|�@�S�@�;d@�+@�"�@��@��!@�v�@�{@��^@�x�@��@���@��@�z�@�Z@�A�@�(�@�1@��;@���@��w@��F@���@�t�@�;d@�
=@��H@���@���@��\@�v�@���@���@��h@�x�@�O�@�V@��/@�Ĝ@�($@��@tg8@l<�@gY@_S�@V�@QDg@JB[@A�T@:?@5f�@/iD@)�h@$��@ e�@S&@��@��@
!�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aպ^Aգ�AՓuAՉ7AՅA�n�A�hsA�VA�A�A�/A��TA��
A���Aԏ\A�  A���A���A˝�A�A�A��/Aé�A°!A��!A��!A��A��A��RA��
A���A��mA�"�A��^A�ZA��A��+A��A�p�A�=qA���A�ffA�;dA���A�|�A�VA�A���A��A���A�JA��-A��PA�{A�  A�Q�A��#A��7A�oA���A�r�A���A�C�A��\A��A��A���A���A�(�A�|�A��wA�C�A�r�A��A�VA�dZA���A��PA�ĜA�&�A���A�O�A��`A���A�VA��A��A��A�n�A��A��RA�JA��A���A��wA���A�r�A�bA���A�Q�A��A��!A�E�A��HA��^A��A+A~-A{�mAyK�Av�9At��Ar�Aq��Ap5?Ao�Akl�Ah�yAg��Af1'Ad�RA`E�A^�A["�AX �AWx�AWK�AW;dAW�AV�+AS��APv�AN��AM�AL�!AK��AI�PAI\)AIK�AI�AG�mAF�HAFVAE�wADffAB1'A?��A>�A>ZA=oA:�`A9"�A8�A7
=A5x�A4ȴA4ȴA4~�A3�A3
=A1
=A/dZA/%A.5?A-ƨA-��A,��A*�HA)��A(r�A'/A%��A#p�A"JA �DA   AC�A�-A��A?}A�PA�`A�;A��A��A��AQ�A��A�A��A=qA�TA�A;dA��AI�AC�A��A9XA�PAA
~�A	XA�A7LA{A�A=qA+AȴA��At�A33A �A �@�v�@��#@���@�{@��h@�S�@��#@�I�@�S�@�7@���@��@�+@�ff@�M�@�`B@�Z@��@�33@�^5@�p�@�Ĝ@�@�@�@�1'@�\)@�-@���@�@���@��/@���@�ff@�7L@���@֟�@�Ĝ@�r�@��@���@Ϯ@�`B@˝�@���@�$�@�?}@ȴ9@�I�@� �@��
@ȃ@�A�@�A�@���@��@���@�1'@�@�x�@�9X@��@�V@���@�  @�$�@���@�?}@��
@��H@�G�@��`@�hs@��@��@���@��7@�=q@��@�K�@���@���@���@��!@��@�
=@�
=@�5?@���@��w@���@���@�v�@�=q@�J@��#@�@���@��@�G�@���@�1'@��y@�V@��9@���@�/@���@��@���@��!@���@���@�?}@��;@���@�J@��^@�/@��`@��j@�I�@�ƨ@���@�S�@�"�@�
=@�o@�@���@�v�@���@���@�hs@�/@�%@��/@�Ĝ@���@��@�r�@�1@���@���@��F@�l�@�S�@�dZ@�\)@�+@�C�@�|�@�"�@���@�ȴ@���@��R@�M�@�J@���@�hs@�/@��@���@�Ĝ@��@�A�@�(�@�  @�\)@��#@�Ĝ@��@��@�Q�@�I�@��@���@��P@�\)@��F@�@���@�-@�@��@�@��@�7L@���@��@��@�S�@���@��@��P@�dZ@�K�@���@�$�@��\@�ȴ@���@���@���@�X@���@��9@�Ĝ@���@��`@��@���@���@���@���@�Q�@���@���@�"�@���@�-@��h@�`B@�&�@���@�Ĝ@��j@���@�r�@�A�@�9X@�1'@�(�@�1@��m@�|�@�S�@�;d@�+@�"�@��@��!@�v�@�{@��^@�x�@��@���@��@�z�@�Z@�A�@�(�@�1@��;@���@��w@��F@���@�t�@�;d@�
=@��H@���@���@��\@�v�@���@���@��h@�x�@�O�@�V@��/G�O�@�($@��@tg8@l<�@gY@_S�@V�@QDg@JB[@A�T@:?@5f�@/iD@)�h@$��@ e�@S&@��@��@
!�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBN�BN�BM�BN�BM�BN�BM�BM�BM�BM�BN�BO�BN�BL�B<jB\)BjBz�B��B�B�BB�B��B�B�fB�BB+B49BA�BT�Bl�BcTB^5BbNBm�Bv�Bw�By�By�Br�Bw�B}�B�=B�\B�PB�PB�{B��B��B��B��B��B�uB�=B�+Bw�Bq�Br�BjBe`B]/BQ�BJ�BC�B1'B�BVB	7BB��B�;BɺB�wB�}BŢB��B�LB��B��B��B�\B�JB�Bw�Bn�By�B�=By�Bl�BO�B5?B2-B0!B.B)�B�BB
�yB
�B
��B
�FB
��B
�B
v�B
n�B
^5B
I�B
9XB
)�B
�B
{B
+B	��B	�;B	��B	��B	�-B	��B	|�B	gmB	N�B	9XB	49B	33B	2-B	0!B	)�B	�B	%B��B��B�B�sB�ZB�TB�TB�HB�ZB�B�yB�fB�HB�#B��B��BǮB��B�}B�qBBB�wBBĜBɺBǮB��BȴBŢBŢBĜBĜBƨBĜB��B�XB�3B�B��B��B��B�DB�+B�Bz�Bu�Bo�Bl�BjBiyBhsBgmBffBe`Be`BcTBbNBaHB`BB_;B_;B^5B\)B\)B[#BYBW
BVBT�BS�BR�BP�BO�BO�BN�BO�BN�BO�BN�BN�BM�BK�BL�BL�BK�BJ�BI�BI�BI�BI�BI�BI�BH�BH�BJ�BK�BJ�BK�BK�BJ�BJ�BK�BL�BL�BN�BM�BM�BN�BN�BO�BO�BQ�BR�BR�BR�BQ�BP�BP�BQ�BS�BS�BS�BR�BVBZBaHBe`BgmBiyBm�Bp�Bt�Bw�B�B�7B�=B�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B��B��B�B�^BĜB�;B�B�sB�sB�B�B�B�B�B�B�B�B�ZB�fB�B�B�B�B�B�B��B��B��B��B��B	  B	  B	B	%B	
=B	PB	hB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	(�B	.B	1'B	33B	5?B	7LB	9XB	<jB	?}B	D�B	E�B	J�B	K�B	M�B	N�B	O�B	P�B	Q�B	R�B	S�B	S�B	W
B	[#B	^5B	`BB	cTB	dZB	e`B	e`B	e`B	gmB	hsB	m�B	q�B	q�B	q�B	q�B	v�B	w�B	y�B	~�B	�B	�B	�B	�+B	�1B	�7B	�=B	�DB	�=B	�+B	�%B	�+B	�1B	�=B	�JB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�^B	�}B	�}B	�}B	�wB	�qB	��B	ƨB	ȴB	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�;B	�HB	�TB	�`B	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
pB
�B
/OB
6`B
;�B
:*B
>(B
DB
J	B
O�B
W�B
]IB
bhB
f�B
j�B
m�B
tTB
x�B
~B
� B
��111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B=B=B<B=
B<B=B<	B<B<B<B=B>B=B;G�O�BJ\BX�BiB��B�4B�fB��B��BگBԌBܿB�9B$B"XB/�BCBZ�BQqBLMBPeB[�Bd�Be�Bg�Bg�B`�Be�BlBxUB}uB{hB{fB��B��B��B��B��B��B��BxUBuBBe�B_�B`�BX�BSBKOB@	B8�B1�BOB�B��B�eB�<B��B�qB��B��B��B��B��B��B�9B�	B��B}�Bz�BoIBfB\�BhBx�Bh$BZ�B>(B#�B ~BqBfBQB
B
�wB
��B
�vB
�#B
��B
�@B
r�B
e7B
]B
L�B
82B
'�B
vB
7B
�B	��B	�dB	��B	�RB	�B	��B	�OB	k�B	V	B	=uB	'�B	"�B	!�B	 �B	�B	�B	CB��B�B�kB�OB�!B�B�B�B��B�	B�.B�+B�B��B��B��B�}B�gB�<B�7B�+B�HB�HB�0B�JB�YB�qB�fB��B�qB�_B�[B�WB�YB�gB�WB�@B�B��B��B��B�vB�EBzBu�Bo�Bi�Bd�B^lB[YBYNBXEBW>BV>BU5BT-BT0BRBQBPBOBNBN	BMBJ�BJ�BI�BG�BE�BD�BC�BB�BA�B?�B>�B>�B=�B>�B=�B>�B=�B=�B<�B:�B;�B;�B:�B9�B8�B8�B8�B8�B8�B8�B7�B7�B9�B:�B9�B:�B:�B9�B9�B:�B;�B;�B=�B<�B<�B=�B=�B>�B>�B@�BA�BA�BA�B@�B?�B?�B@�BB�BB�BB�BA�BD�BH�BP$BT:BVFBXUB\oB_Bc�Bf�Br�BxByB�EB�zB��B��B��B��B��B�B�}B�uB�sB�kB�cB�`B�\B�TB�SB�iB��B��B�/B�jB�B�JB�AB�?B�QB�WB�WB�]B�bB�oB�qB�lB�&B�4B�MB�TB�^B�iB�yB�B�B�B�B�B�B��B��B��B��B�B�B	 2B	HB	IB	JB	
mB	wB	
oB	bB		jB	vB	}B	�B	�B	�B	�B	�B	!�B	$B	&B	(B	+/B	.@B	3]B	4eB	9�B	:�B	<�B	=�B	>�B	?�B	@�B	A�B	B�B	B�B	E�B	I�B	L�B	OB	RB	SB	T B	T B	TB	V,B	W2B	\OB	`hB	`iB	`kB	`iB	e�B	f�B	h�B	m�B	q�B	r�B	s�B	u�B	v�B	w�B	x�B	zB	x�B	u�B	t�B	u�B	v�B	x�B	{B	B	�-B	�0B	�HB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�2B	�1B	�,B	�)B	�9B	�[B	�iB	�sB	�}B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�&B	�.B	�;B	�BB	�@B	�@B	�MB	�TB	�YB	�YB	�YB	�YB	�ZB	�XB	�lB	�rB	�uB	�pB	�pB	�pB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��G�O�B	�B
hB
�B
%B
*YB
(�B
,�B
2�B
8�B
>nB
FQB
K�B
QB
UCB
Y�B
\mB
b�B
gzB
l�B
o�B
tK111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.017(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180715070212    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180715070212  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180715070212  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                