CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:57Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041157  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��|��Ta1   @��}@ڒo@4�33333�d�I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1fD1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyc�D��D�FfD�mD��D�{D�6fD�\)D��)D�� D�?
D�i�D���D�{D�B�Dڔ)D�D���D�@RD�{�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C
>C

>C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*��D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D1�D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@��D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DP�DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)DtDy` D���D�DzD�k3D��(D��D�4zD�Z=D��=D��D�=D�g�D���D��D�@�Dڒ=D���D��
D�>fD�y�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A���A�ƨAѲ-Aѥ�Aѝ�Aћ�Aї�Aї�Aї�Aџ�Aѕ�A�C�Aϛ�A�bA��AΕ�A���A�t�A�1'A��A���A�ffA�ZA�Q�AʃA��AɍPA�ffA�O�A�9XA�oA�XAǺ^A�r�A��A��#Aơ�A�9XA���Aş�A�ZAĸRA�t�A�G�A�bA��A�ƨA�S�A���A�/A��A��;A���A���A�ĜA��wA��jA��RA��-A���A��uA�bA�x�A�9XA��jA�\)A�5?A��RA�5?A�hsA��mA�VA���A�5?A�l�A�I�A��DA���A�dZA�VA��A�A�;dA���A���A���A�=qA��/A�K�A�+A�O�A���A�I�A���A��#A���A�-A���A���A��A��9A�%A�{A�7LA���A�E�A�v�A���A�ȴA�S�A�A~jA}XA|1'Az1'Ax��AxA�Aw�Aw�
Aw/AuS�At��Ar^5Aox�Al�DAkK�Aj�Ag��Ac�hA`=qA^5?A\��AYO�AXbAW`BAW�AV(�AT�`AT�+AS��AQ�-AOVAL�uAJ  AH(�AG�mAG;dAE�;AE+AD��AC�ACK�AA�;AA�wAA��A@��A?��A?�A>�A>A=�mA=x�A<bNA<9XA<5?A<1A;��A;�7A:^5A7�;A6�uA5�mA5S�A4^5A3�TA3��A2�9A1��A0^5A/��A-�A,�A+A+��A+"�A*bNA)�7A'l�A&�A%�A#S�A"{A!��A!?}A Q�A��A��A �A�#A�FA`BAC�A�Az�A1'AJA�^Ap�AI�AAp�A{AVAbNA�mA;dA�AM�AA  A��AhsA�;Ax�A
�/A
��A
I�A	��A	hsA	oA��A%A�^A�AQ�A��Al�AXA��Av�AƨAXAG�A;dA/A Ĝ@�=q@��@�o@���@�33@�/@���@��/@�bN@���@��@���@�@���@�Ĝ@�-@�z�@畁@�O�@�S�@�G�@��@܋D@�S�@�^5@��@�@ٙ�@�"�@Դ9@Ԭ@�b@ӍP@�S�@��@��@���@�ȴ@�ȴ@��#@��@�j@��@���@��#@���@��
@�l�@ʗ�@��@�O�@���@ȋD@�I�@��@���@�O�@ċD@��
@î@�?}@��F@�S�@�"�@�ff@��@�O�@���@�(�@�C�@��y@��+@�^5@�{@���@��@��@�A�@���@�S�@��R@�^5@���@�&�@�Ĝ@�r�@�j@��
@�"�@��R@�n�@�=q@�$�@�J@��#@��@�V@�Ĝ@�r�@�1@��F@�@���@�{@���@�hs@�%@��@��w@���@�M�@��7@��@�I�@�A�@�9X@�(�@��F@�l�@��P@��@�^5@���@�"�@�
=@�"�@�+@��@���@��^@��@�9X@��@��@�ȴ@�$�@��@���@�(�@��@�dZ@�"�@�@��R@��!@��!@���@��R@��!@���@�@�O�@�%@��@��/@��/@��/@���@��9@�z�@�Q�@�(�@�b@�b@�1@�b@�1@���@��w@�\)@�C�@�"�@���@��y@��@��@���@���@�~�@�n�@�E�@�J@��#@�X@���@���@�I�@��F@�;d@�33@�+@�@��@�v�@�M�@�-@�{@�@���@���@��@���@�x�@��@��/@�(�@�|�@���@��\@�^5@�$�@���@���@��@�z�@�(�@�ƨ@���@���@�l�@�33@�"�@�
=@��y@���@�^5@�{@�X@�/@��@���@�I�@�  @�ƨ@��P@�K�@�33@�+@�"�@��H@�E�@�@��4@�%�@yϫ@r��@n($@g��@_�@@V�F@Q��@J�2@E��@<�@4�_@/�4@'_p@"�B@ �_@֡@L0@L0@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�x�A���A�ƨAѲ-Aѥ�Aѝ�Aћ�Aї�Aї�Aї�Aџ�Aѕ�A�C�Aϛ�A�bA��AΕ�A���A�t�A�1'A��A���A�ffA�ZA�Q�AʃA��AɍPA�ffA�O�A�9XA�oA�XAǺ^A�r�A��A��#Aơ�A�9XA���Aş�A�ZAĸRA�t�A�G�A�bA��A�ƨA�S�A���A�/A��A��;A���A���A�ĜA��wA��jA��RA��-A���A��uA�bA�x�A�9XA��jA�\)A�5?A��RA�5?A�hsA��mA�VA���A�5?A�l�A�I�A��DA���A�dZA�VA��A�A�;dA���A���A���A�=qA��/A�K�A�+A�O�A���A�I�A���A��#A���A�-A���A���A��A��9A�%A�{A�7LA���A�E�A�v�A���A�ȴA�S�A�A~jA}XA|1'Az1'Ax��AxA�Aw�Aw�
Aw/AuS�At��Ar^5Aox�Al�DAkK�Aj�Ag��Ac�hA`=qA^5?A\��AYO�AXbAW`BAW�AV(�AT�`AT�+AS��AQ�-AOVAL�uAJ  AH(�AG�mAG;dAE�;AE+AD��AC�ACK�AA�;AA�wAA��A@��A?��A?�A>�A>A=�mA=x�A<bNA<9XA<5?A<1A;��A;�7A:^5A7�;A6�uA5�mA5S�A4^5A3�TA3��A2�9A1��A0^5A/��A-�A,�A+A+��A+"�A*bNA)�7A'l�A&�A%�A#S�A"{A!��A!?}A Q�A��A��A �A�#A�FA`BAC�A�Az�A1'AJA�^Ap�AI�AAp�A{AVAbNA�mA;dA�AM�AA  A��AhsA�;Ax�A
�/A
��A
I�A	��A	hsA	oA��A%A�^A�AQ�A��Al�AXA��Av�AƨAXAG�A;dA/A Ĝ@�=q@��@�o@���@�33@�/@���@��/@�bN@���@��@���@�@���@�Ĝ@�-@�z�@畁@�O�@�S�@�G�@��@܋D@�S�@�^5@��@�@ٙ�@�"�@Դ9@Ԭ@�b@ӍP@�S�@��@��@���@�ȴ@�ȴ@��#@��@�j@��@���@��#@���@��
@�l�@ʗ�@��@�O�@���@ȋD@�I�@��@���@�O�@ċD@��
@î@�?}@��F@�S�@�"�@�ff@��@�O�@���@�(�@�C�@��y@��+@�^5@�{@���@��@��@�A�@���@�S�@��R@�^5@���@�&�@�Ĝ@�r�@�j@��
@�"�@��R@�n�@�=q@�$�@�J@��#@��@�V@�Ĝ@�r�@�1@��F@�@���@�{@���@�hs@�%@��@��w@���@�M�@��7@��@�I�@�A�@�9X@�(�@��F@�l�@��P@��@�^5@���@�"�@�
=@�"�@�+@��@���@��^@��@�9X@��@��@�ȴ@�$�@��@���@�(�@��@�dZ@�"�@�@��R@��!@��!@���@��R@��!@���@�@�O�@�%@��@��/@��/@��/@���@��9@�z�@�Q�@�(�@�b@�b@�1@�b@�1@���@��w@�\)@�C�@�"�@���@��y@��@��@���@���@�~�@�n�@�E�@�J@��#@�X@���@���@�I�@��F@�;d@�33@�+@�@��@�v�@�M�@�-@�{@�@���@���@��@���@�x�@��@��/@�(�@�|�@���@��\@�^5@�$�@���@���@��@�z�@�(�@�ƨ@���@���@�l�@�33@�"�@�
=@��y@���@�^5@�{@�X@�/@��@���@�I�@�  @�ƨ@��P@�K�@�33@�+@�"�@��H@�E�G�O�@��4@�%�@yϫ@r��@n($@g��@_�@@V�F@Q��@J�2@E��@<�@4�_@/�4@'_p@"�B@ �_@֡@L0@L0@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
bB
\B
bB
bB
bB
hB
hB
hB
oB
�B
�B
I�B
�BB,B33BS�B��B��B�jBƨB��B��B�mB%B{B �B%�B)�B,B-B1'BA�BG�BJ�BM�BP�BVBVBYBk�Bz�Bo�Br�B{�B}�B�%B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�?B�?B�LB�?B�XB�^B�XB�!B��B�\B�%Bz�Bn�B\)BJ�B>wB9XB-B%�B�B�B��B�B�ZB�
B��B�^B��B�{B�Bx�Bp�BffBYBD�B49B+B�BoB	7B
��B
�HB
��B
�3B
��B
��B
�oB
�DB
�B
u�B
l�B
jB
hsB
ffB
`BB
S�B
O�B
B�B
1'B
�B
�B
JB	��B	�)B	ÖB	�FB	��B	��B	�VB	�DB	�7B	�B	|�B	z�B	w�B	m�B	ZB	F�B	8RB	/B	-B	(�B	"�B	�B	�B	�B	�B	hB	\B	VB	DB	+B	%B	B	  B��B��B��B��B��B��B��B�B�B�fB�HB�;B�)B�B�B�B��B��B��BŢB�}B�qB�jB�dB�RB�FB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�FB�}BÖB��B�
B�)B�HB�ZB�ZB�TB�fB�B�B�B�B�B�B�B�B�B�sB�sB�yB�mB�ZB�BB�;B�B�
B�
B�B�B�B�#B�#B�
B��BĜBƨBǮBŢBŢBƨBǮBȴBȴBȴBȴBǮBŢB�jB�FB�FB�-B�B��B��B��B��B��B��B��B��B��B��B�B�B�-B�-B�9B�FB�RB�XB�^BƨB��B�B�
B�B�B�B�)B�)B�5B�HB�TB�ZB�ZB�ZB�mB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	B	%B	%B	%B		7B	DB	DB	PB	\B	uB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	%�B	&�B	&�B	&�B	&�B	'�B	)�B	+B	.B	0!B	2-B	6FB	7LB	:^B	<jB	<jB	?}B	A�B	E�B	E�B	M�B	Q�B	W
B	ZB	ZB	[#B	[#B	]/B	dZB	hsB	iyB	iyB	p�B	w�B	z�B	�B	�B	�%B	�1B	�PB	�JB	�DB	�7B	�1B	�1B	�+B	�+B	�1B	�=B	�DB	�JB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�RB	�RB	�RB	�RB	�RB	�XB	�dB	�}B	��B	��B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�BB	�HB	�HB	�HB	�HB	�NB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
	7B
�B

B
 'B
-]B
0oB
6B
<�B
DMB
G�B
M�B
QhB
YKB
^B
a|B
gRB
k�B
l�B
p�B
w�B
|�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B
B
<B
B6B
{�B
�B$vB+�BLhB��B�[B��B�B�gB�?B��B��B�B,BFB"_B$lB%vB)�B9�B@BC#BF5BIFBNiBNdBQwBc�Bs>Bg�BkBtFBvOB~�B��B�$B� B�B�B�B�	B�B�	B�	B�B�	B�B�B�	B�*B�SB�cB��B��B��B��B��B��B��B��B��B�B��B~�BsDBf�BT�BC%B6�B1�B%xBJB B�B�UB��B��B�wB�4B��B�VB��B}�BqPBiB^�BQ�B=B,�B#}B0B
�B�B
�JB
��B
�GB
��B
�KB
�B
��B
��B
{�B
nLB
eB
cB
`�B
^�B
X�B
L�B
HjB
;B
)�B
=B
B
�B	�qB	ԻB	�)B	��B	��B	�!B	��B	��B	��B	{�B	u�B	sxB	phB	f(B	R�B	?BB	0�B	'�B	%�B	!�B	oB	aB	LB	8B	"B	
B	�B	�B	�B��B��B��B��B��B�B�|B�zB�vB�pB�iB�ZB�2B�B��B��B��BѺBдBΩBʑB�xB�jB�IB�$B�B�B�B��B��B��B��B��B��B��B�yB�vB�qB�\B�sB�wB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�&B�;B�kBϰB��B��B�B�B� B�B�1B�EB�_B�aB�XB�SB�UB�=B�&B�B�B�"B�B�B��B��B��BϲB϶B��B��BѿB��B��B϶B�rB�EB�RB�YB�QB�PB�UB�WB�aB�bB�bB�bB�ZB�NB�B��B��B��B��B��B�`B�NB�[B�[B�aB�\B�VB�lB��B��B��B��B��B��B��B� B�B�B�WBʜBήBϸBзB��B��B��B��B��B��B��B�B�B�B�B�+B�.B�FB�LB�GB�gB�B�B�B�B��B��B��B��B��B��B��B��B��B��B	�B	�B	�B	�B	B	B	+B	9B	KB	LB	SB	VB	`B	zB	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	&�B	(�B	*�B	.�B	/�B	3	B	5B	5B	8&B	:3B	>FB	>LB	F|B	J�B	O�B	R�B	R�B	S�B	S�B	U�B	] B	aB	b"B	bB	iMB	pwB	s�B	y�B	{�B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�*B	�LB	�PB	�SB	�UB	�[B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�!B	�$B	�-B	�>B	�DB	�JB	�IB	�OB	�WB	�[B	�]B	�cB	�kB	�qB	ȀB	ʊB	˓B	̝B	ΧB	ѸB	ѷB	ѸB	ѸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	� B	�.B	�8B	�>B	�AB	�BB	�>B	�AB	�DB	�CB	�DB	�HB	�SB	�SB	�^B	�rB	�rB	�wB	�{B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
�B
�B
�B
%�B
)B
.�B
5�B
<�B
@eB
F<B
JB
Q�B
V�B
ZB
_�B
d=B
e�B
iBB
p�B
uoB
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.007(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20181121041157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041157  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041157  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                