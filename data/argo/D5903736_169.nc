CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:54Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041154  20190604094024  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��O,_�.1   @��P_bB�@3�I�^5�d����m1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDtffDyP D��D�>D�iHD��)D�qD�>fD�p�D��{D�	�D�1HD�y�D�ȤD��D�1�D�
D��=D�RD�0�D�t�D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @tz�@�=q@�=qA�A=�A]�A}�A��\A��\A�A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B��
B�=qB�p�B���B���B�
=B�p�B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C�C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dz�D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df��Dgz�Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds��DtZ�DyD{D�)D�8QD�c�D��fD��D�8�D�k3D�ƸD�)D�+�D�t)D���D� D�,)D�GD�zD�
�D�*�D�o
D��z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA��HA��HA��
A��#A��HA��`A��mA��mA��mA��mA��mA��yA��A��A��A���A���A���A���A���A�  A�A���A�  A���A���A���A���A���A���A���A�  A�A�%A�1A�JA�A�A�  A��A�A�bA�ĜA�%A�XA�jA� �Aʕ�Aɥ�A�33A�1'A�v�A�5?A���A�v�A�~�A�M�A��9A���A�VA��`A�\)A���A��uA�bA��mA��mA��HA���A���A�&�A���A���A���A��A��TA�z�A�ƨA�jA��/A�G�A��;A��A���A���A�z�A� �A�Q�A���A��9A�~�A��TA�$�A���A�;dA��A��FA�ȴA��9A�t�A�I�A�dZA��A�C�A�G�A�I�A�VA���A��A�M�A�r�A��A�9XA��TA�
=A�7LA���A�x�A���A�v�A��A|��AydZAwp�Au�AsXAq7LAp9XAohsAm?}Ajv�Ae�TAdz�AbbAal�A_�A\��AZffAYhsAW�mAT^5AS?}AR{AQ?}AO�ANI�AM�TAM|�AM�AL��AK��AI��AI
=AG�mAF��AD�/AC�A>�uA:�!A9C�A8��A81'A5�^A3"�A1��A0�`A.��A.I�A.�A.  A+�mA)��A(�`A'\)A%K�A#
=A z�A�^A�A�
A {AS�A��A�PAG�AI�A�A�^A
=A9XA�A�A�uAn�A9XA1'Al�AbA�-A+A�A�-A~�A�;A\)A�A�7AS�A
��A	�;AA�A%A��A1A�FAXA�AA �!@��@�M�@�hs@��\@���@��h@�
=@�@�z�@�j@�l�@�ȴ@�+@�$�@��@�r�@��;@�ƨ@��@�  @� �@�^5@���@��@�-@��D@��
@�  @�+@�1'@ޏ\@�G�@��@�%@��`@ܣ�@�O�@��#@�Z@Л�@�{@�p�@��@�ȴ@�hs@�bN@�1'@��;@�@�V@�{@��@ģ�@�  @Å@\@��h@���@�5?@�v�@���@�ƨ@�C�@��y@��@���@�E�@�/@�`B@��h@���@��j@���@�z�@�I�@��@��@�S�@���@���@���@�-@�@��@��@��@���@��@�X@�I�@�(�@�(�@�9X@��@�33@��\@�5?@���@�A�@��F@���@�@�x�@��@�bN@��j@�1@���@�;d@��R@�M�@���@��h@�?}@��@�S�@���@�J@�V@��@�ƨ@��F@��@���@�K�@��!@�~�@�~�@�ff@�{@��^@�x�@�X@�V@���@��`@���@��`@���@�/@�G�@�`B@�V@�Z@�b@�b@�b@�b@�b@���@���@�|�@�t�@�t�@�\)@�+@��y@��\@�V@�n�@�E�@�/@�/@�&�@�Ĝ@�z�@�bN@�I�@�b@�l�@��y@���@�~�@�V@�V@�V@�=q@��@��T@��@�&�@�A�@�Q�@�1'@�1'@�1'@��w@�C�@�+@��R@�M�@���@��^@��7@�G�@�&�@��@��@��u@�I�@�1@��
@�ƨ@��@���@�l�@�K�@�C�@�;d@�+@�"�@��@��@�ff@�E�@�=q@�=q@�5?@���@��@�G�@�/@��@�V@�%@���@���@�j@�1@�ƨ@���@�\)@�33@�o@��@��H@��!@�n�@�5?@���@��-@��@�p�@�G�@���@�Ĝ@��u@��@�Q�@� �@���@�|�@�t�@�K�@�o@��y@��!@�ff@�5?@���@��T@��#@���@���@���@���@���@��^@�&�@��)@x�P@p�$@i�Z@cK�@]Vm@SA�@K9�@E��@=a�@8�@3Y@+]�@'�@!(�@��@�)@t�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A��HA��HA��HA��
A��#A��HA��`A��mA��mA��mA��mA��mA��yA��A��A��A���A���A���A���A���A�  A�A���A�  A���A���A���A���A���A���A���A�  A�A�%A�1A�JA�A�A�  A��A�A�bA�ĜA�%A�XA�jA� �Aʕ�Aɥ�A�33A�1'A�v�A�5?A���A�v�A�~�A�M�A��9A���A�VA��`A�\)A���A��uA�bA��mA��mA��HA���A���A�&�A���A���A���A��A��TA�z�A�ƨA�jA��/A�G�A��;A��A���A���A�z�A� �A�Q�A���A��9A�~�A��TA�$�A���A�;dA��A��FA�ȴA��9A�t�A�I�A�dZA��A�C�A�G�A�I�A�VA���A��A�M�A�r�A��A�9XA��TA�
=A�7LA���A�x�A���A�v�A��A|��AydZAwp�Au�AsXAq7LAp9XAohsAm?}Ajv�Ae�TAdz�AbbAal�A_�A\��AZffAYhsAW�mAT^5AS?}AR{AQ?}AO�ANI�AM�TAM|�AM�AL��AK��AI��AI
=AG�mAF��AD�/AC�A>�uA:�!A9C�A8��A81'A5�^A3"�A1��A0�`A.��A.I�A.�A.  A+�mA)��A(�`A'\)A%K�A#
=A z�A�^A�A�
A {AS�A��A�PAG�AI�A�A�^A
=A9XA�A�A�uAn�A9XA1'Al�AbA�-A+A�A�-A~�A�;A\)A�A�7AS�A
��A	�;AA�A%A��A1A�FAXA�AA �!@��@�M�@�hs@��\@���@��h@�
=@�@�z�@�j@�l�@�ȴ@�+@�$�@��@�r�@��;@�ƨ@��@�  @� �@�^5@���@��@�-@��D@��
@�  @�+@�1'@ޏ\@�G�@��@�%@��`@ܣ�@�O�@��#@�Z@Л�@�{@�p�@��@�ȴ@�hs@�bN@�1'@��;@�@�V@�{@��@ģ�@�  @Å@\@��h@���@�5?@�v�@���@�ƨ@�C�@��y@��@���@�E�@�/@�`B@��h@���@��j@���@�z�@�I�@��@��@�S�@���@���@���@�-@�@��@��@��@���@��@�X@�I�@�(�@�(�@�9X@��@�33@��\@�5?@���@�A�@��F@���@�@�x�@��@�bN@��j@�1@���@�;d@��R@�M�@���@��h@�?}@��@�S�@���@�J@�V@��@�ƨ@��F@��@���@�K�@��!@�~�@�~�@�ff@�{@��^@�x�@�X@�V@���@��`@���@��`@���@�/@�G�@�`B@�V@�Z@�b@�b@�b@�b@�b@���@���@�|�@�t�@�t�@�\)@�+@��y@��\@�V@�n�@�E�@�/@�/@�&�@�Ĝ@�z�@�bN@�I�@�b@�l�@��y@���@�~�@�V@�V@�V@�=q@��@��T@��@�&�@�A�@�Q�@�1'@�1'@�1'@��w@�C�@�+@��R@�M�@���@��^@��7@�G�@�&�@��@��@��u@�I�@�1@��
@�ƨ@��@���@�l�@�K�@�C�@�;d@�+@�"�@��@��@�ff@�E�@�=q@�=q@�5?@���@��@�G�@�/@��@�V@�%@���@���@�j@�1@�ƨ@���@�\)@�33@�o@��@��H@��!@�n�@�5?@���@��-@��@�p�@�G�@���@�Ĝ@��u@��@�Q�@� �@���@�|�@�t�@�K�@�o@��y@��!@�ff@�5?@���@��T@��#@���@���@���@���@���G�O�@�&�@��)@x�P@p�$@i�Z@cK�@]Vm@SA�@K9�@E��@=a�@8�@3Y@+]�@'�@!(�@��@�)@t�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
B
B
B
B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
+B
PB
p�B
�B?}B��B�BB��B{B+B49B8RB;dB6FB49B2-BB�BQ�B;dB5?B�B��B�B�^B�dB�XB�?B�^B��B��B��B�XB��B�VB`BBD�B�?B�^B��B��B�bB�B}�Bw�Bl�B_;B_;BbNB`BBVBD�B33B�B�B�B�B��B�B�/B��B�RB�B��B��B�{Bq�B^5BQ�BF�B49B �B{B
=BB
��B
��B
�yB
��B
�-B
��B
�oB
�JB
{�B
_;B
H�B
<jB
33B
#�B
�B
PB
B	�B	�
B	�-B	��B	��B	�bB	�B	o�B	bNB	\)B	R�B	D�B	>wB	9XB	49B	/B	)�B	'�B	%�B	$�B	!�B	�B	{B	hB	JB	%B��B�B�ZB�)B�/B�BB�BB�/B�B�
B��B��B��B��B��B��B�dB�LB�'B��B��B��B��B��B��B�!B�^B�FB�B��B��B��B�B�B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B�B�!B�B��B��B��B��B�PB�7B�1B�+B�B�B�B}�Bz�By�Bv�Bs�Bp�Bn�Bs�B}�B�%B�B�B�%B�%B�B}�B�B�B�+B�bB��B�hB�PB�=B�=B�=B�VB�oB�uB��B��B��B��B��B��B��B��B�{B��B��B�{B��B��B��B��B��B��B��B��B��B�B�-B�9B�3B�-B�B�B�-B�RB�wB�qB�qB�}BƨBɺB��B��B��B��B�#B�;B�BB�TB�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	B	B	B	+B	
=B	JB	VB	\B	\B	bB	bB	bB	bB	hB	�B	�B	!�B	 �B	 �B	 �B	!�B	"�B	$�B	$�B	%�B	&�B	+B	-B	.B	1'B	8RB	=qB	@�B	B�B	C�B	I�B	R�B	YB	[#B	\)B	_;B	dZB	ffB	hsB	l�B	n�B	o�B	r�B	v�B	x�B	z�B	|�B	~�B	�B	�B	�1B	�7B	�=B	�DB	�JB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�-B	�-B	�3B	�-B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�dB	�qB	�}B	B	ĜB	ŢB	ǮB	ǮB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�NB	�TB	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
fB
�B
�B
$B
2|B
72B
<B
B�B
I7B
M6B
S&B
X�B
_B
f2B
h>B
l=B
o�B
s�B
yrB
}"B
.11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	�B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�B	�B	�B	�
B	�B	�
B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B
2B
WB
k�B
�B:wB��B�5B��BjB%�B/+B3DB6XB19B//B-B=�BL�B6WB06B~B��B��B�NB�PB�DB�*B�JB�tB�zB�xB�FB��B�GB[1B?�B�/B�OB��B��B�WB�Bx�Br�Bg�BZ2BZ3B]FB[7BP�B?�B.+B�B�B�B��B�B�B�,B��B�MB�B��B��B�zBl�BY4BL�BA�B/;B�B�BFB
�B
��B
��B
�B
��B
�<B
��B
�{B
�ZB
v�B
ZIB
C�B
7zB
.AB
�B
�B
dB	�$B	�B	� B	�CB	��B	��B	�zB	}%B	j�B	]eB	W@B	NB	?�B	9�B	4uB	/WB	*9B	%B	#B	 �B	�B	�B	�B	�B	�B	kB	EB�B��B�xB�MB�SB�gB�eB�QB�?B�/B�B�B�B��B��B��B��B�tB�NB�B��B��B��B��B�B�FB��B�lB�6B��B��B�B�1B�4B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�*B�IB�=B�$B�B��B��B�~B�cB�]B�TB~8B|/B|0ByBvBuBq�Bn�Bk�Bi�Bn�By B�NB}6BDB�QB�NBCByB|0BCB�VB��B��B��B�{B�lB�kB�gB�B��B��B��B� B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�EB�ZB�dB�^B�\B�GB�AB�]B�B��B��B��B��B��B��B��B��B��B�-B�PB�dB�kBށB�B�B�B�B�B��B��B��B�B�B�B�B�(B�4B�1B�7B�9B�7B	 JB	UB	eB	sB		|B	
�B	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 B	 B	!
B	"B	&+B	(5B	)>B	,RB	3yB	8�B	;�B	=�B	>�B	D�B	NB	T@B	VGB	WRB	Z_B	_�B	a�B	c�B	g�B	i�B	j�B	m�B	q�B	s�B	vB	xB	zB	|-B	�CB	�VB	�ZB	�aB	�hB	�mB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�PB	�UB	�UB	�VB	�SB	�RB	�YB	�\B	�eB	�lB	�sB	�uB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�#B	� B	�'B	�=B	�CB	�BB	�@B	�FB	�OB	�UB	�^B	�_B	�fB	�eB	�iB	�pB	�tB	�qB	�sB	�sB	�sB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�
B	�B	�B	�B	�B	�B	�#B	�"B	�(B	�(B	�&B	�-B	�2B	�-B	�4B	�4B	�8B	�4G�O�B
�B
�B
B
-B
-�B
2UB
7!B
=�B
DYB
HXB
NHB
S�B
Z)B
aQB
c]B
g[B
kB
n�B
t�B
xCB
zO11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9999(+/-0), vertically averaged dS =-0.005(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940242019060409402420190604094024  AO  ARCAADJP                                                                    20181121041154    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041154  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041154  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094024  IP                  G�O�G�O�G�O�                