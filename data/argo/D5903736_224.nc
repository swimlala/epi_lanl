CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-04T22:00:50Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180704220050  20190604094146  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�oo,_�.1   @�oo�-��@5��1'�d�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@���@���A   AA��Aa��A�ffA�ffA�33A�  A���A���A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyT{D�{D�0�D�h�D��qD��D�L�D���D�ָD��D�H�D���D�� D��D�4{Dڒ�D���D��\D�?�D�s�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��H@��HA
=A@��A`��A��A��A��RA��A�Q�A�Q�A߅A�A��B\)BBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��D |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^��D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)DtDyP�D��D�.�D�f�D�ۅD��D�J�D��
D���D�
D�G
D���D��D��D�2�Dڐ�D�� D��pD�=�D�q�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A���A���A�%A�1A�%A���A���A�hsA�
=A���A�p�A��
A��;A�n�A�hsA��TA��;A�1A��wA��`A���A�dZA�bA�  A��A��
A�p�A�G�A���A�G�A�1A��FA�A�A��
A�E�A��A��A��A���A� �A��wA���A�^5A�&�A���A�ffA���A�x�A�hsA�\)A�1'A�1A���A���A�ĜA�C�A���A��wA�7LA�bA���A�C�A��A�\)A�ĜA�VA�&�A�+A�v�A��;A�l�A���A���A�l�A� �A�A��+A�K�A�JA���A�p�A���A���A�oA��`A��A��A��7A��A��TA�$�A���A�v�A�E�A�~�A�{A�A���A��
A�/A�%A�p�A��FA�9XA���A�oA���A���A��`A�(�A�C�A�
=A��A�~�A�%A�1'A���A��
A���A�S�A�$�A�A�XA���A�/A�VA�A~r�A{C�Ayt�Ax��AxZAw�;Av=qAs�ArJAo��An �AlZAkVAh�`AcXA`(�A^��A\�!A\$�AZ�\AX��AWp�AV��AV �AT��AT��AT��ASARbNAQ�AO�hAN�AM��AL$�AJ~�AI��AI&�AHbAF�!AF(�AFAE��AE/AD�uAC�TAC33AA��A>�9A<JA:��A:�yA:ffA9�A8��A7��A6�jA5�A2��A0�DA/��A.bNA,��A*��A)A'��A%�TA$�!A$jA$(�A#�A"Q�A!�A ��A ffA�
A�mA�!Ar�A��A��AE�A7LA��A��A��A�A��A�uA|�A�AVA�PA
��A	;dAO�A�^A\)A&�A��AZA��A?}A b@���@�|�@�|�@�l�@���@���@���@�K�@���@���@�w@�V@�7@��@���@� �@�|�@�R@�7L@�j@�1'@���@��@���@�@�V@�D@�bN@��@�t�@�\@�7L@�@�@�?}@��@߾w@݁@ܴ9@��;@�"�@�=q@�7L@أ�@�|�@��@��@Ԭ@�Z@�b@�ƨ@�;d@�^5@с@�V@�(�@���@�hs@���@���@��@ŉ7@�x�@���@�  @��@�v�@�hs@�
=@��#@�G�@�%@�Ĝ@�bN@� �@��@�|�@���@�v�@���@�/@�1'@��@��@��#@��7@�?}@�Ĝ@��9@��@�bN@��
@��F@��F@���@�
=@�M�@��^@��@��@�Ĝ@�ƨ@��\@�n�@�=q@�-@�5?@�=q@�-@���@�V@��w@�;d@�@�M�@�E�@���@��-@���@�X@�%@�r�@�ƨ@�"�@���@�-@��#@��7@�hs@�X@��`@�C�@�@��H@���@���@���@��+@�n�@�n�@�^5@��T@�?}@��/@�Ĝ@��9@���@�Q�@��;@�dZ@��H@�~�@�M�@�V@�-@���@���@�?}@��`@�Ĝ@���@�Ĝ@�Z@��@�1@��;@��
@���@���@�S�@���@�-@���@�@�&�@��j@��D@�j@��@��@�l�@�K�@��@��\@�ff@�M�@�5?@�-@�-@��@���@��@���@���@��@�X@��@���@���@�Ĝ@�z�@�bN@�bN@�bN@�I�@�1'@�(�@�1'@�A�@�(�@���@�x�@���@��@�Q�@��F@���@�K�@�C�@��@��@��\@���@��@��@�\)@�l�@�\)@�+@�~�@�n�@��@�hs@�V@��`@��/@���@��`@��9@��D@�I�@�9X@��@��m@���@�t�@�C�@�
=@��H@�ȴ@��!@��+@�E�@�@���@�@�@���@�33@�V@�&@|[�@q�@fq�@`�I@Y}�@P_@G�0@Ac�@:�@5��@--w@'$t@#W?@��@��@�@{J@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A���A���A�%A�1A�%A���A���A�hsA�
=A���A�p�A��
A��;A�n�A�hsA��TA��;A�1A��wA��`A���A�dZA�bA�  A��A��
A�p�A�G�A���A�G�A�1A��FA�A�A��
A�E�A��A��A��A���A� �A��wA���A�^5A�&�A���A�ffA���A�x�A�hsA�\)A�1'A�1A���A���A�ĜA�C�A���A��wA�7LA�bA���A�C�A��A�\)A�ĜA�VA�&�A�+A�v�A��;A�l�A���A���A�l�A� �A�A��+A�K�A�JA���A�p�A���A���A�oA��`A��A��A��7A��A��TA�$�A���A�v�A�E�A�~�A�{A�A���A��
A�/A�%A�p�A��FA�9XA���A�oA���A���A��`A�(�A�C�A�
=A��A�~�A�%A�1'A���A��
A���A�S�A�$�A�A�XA���A�/A�VA�A~r�A{C�Ayt�Ax��AxZAw�;Av=qAs�ArJAo��An �AlZAkVAh�`AcXA`(�A^��A\�!A\$�AZ�\AX��AWp�AV��AV �AT��AT��AT��ASARbNAQ�AO�hAN�AM��AL$�AJ~�AI��AI&�AHbAF�!AF(�AFAE��AE/AD�uAC�TAC33AA��A>�9A<JA:��A:�yA:ffA9�A8��A7��A6�jA5�A2��A0�DA/��A.bNA,��A*��A)A'��A%�TA$�!A$jA$(�A#�A"Q�A!�A ��A ffA�
A�mA�!Ar�A��A��AE�A7LA��A��A��A�A��A�uA|�A�AVA�PA
��A	;dAO�A�^A\)A&�A��AZA��A?}A b@���@�|�@�|�@�l�@���@���@���@�K�@���@���@�w@�V@�7@��@���@� �@�|�@�R@�7L@�j@�1'@���@��@���@�@�V@�D@�bN@��@�t�@�\@�7L@�@�@�?}@��@߾w@݁@ܴ9@��;@�"�@�=q@�7L@أ�@�|�@��@��@Ԭ@�Z@�b@�ƨ@�;d@�^5@с@�V@�(�@���@�hs@���@���@��@ŉ7@�x�@���@�  @��@�v�@�hs@�
=@��#@�G�@�%@�Ĝ@�bN@� �@��@�|�@���@�v�@���@�/@�1'@��@��@��#@��7@�?}@�Ĝ@��9@��@�bN@��
@��F@��F@���@�
=@�M�@��^@��@��@�Ĝ@�ƨ@��\@�n�@�=q@�-@�5?@�=q@�-@���@�V@��w@�;d@�@�M�@�E�@���@��-@���@�X@�%@�r�@�ƨ@�"�@���@�-@��#@��7@�hs@�X@��`@�C�@�@��H@���@���@���@��+@�n�@�n�@�^5@��T@�?}@��/@�Ĝ@��9@���@�Q�@��;@�dZ@��H@�~�@�M�@�V@�-@���@���@�?}@��`@�Ĝ@���@�Ĝ@�Z@��@�1@��;@��
@���@���@�S�@���@�-@���@�@�&�@��j@��D@�j@��@��@�l�@�K�@��@��\@�ff@�M�@�5?@�-@�-@��@���@��@���@���@��@�X@��@���@���@�Ĝ@�z�@�bN@�bN@�bN@�I�@�1'@�(�@�1'@�A�@�(�@���@�x�@���@��@�Q�@��F@���@�K�@�C�@��@��@��\@���@��@��@�\)@�l�@�\)@�+@�~�@�n�@��@�hs@�V@��`@��/@���@��`@��9@��D@�I�@�9X@��@��m@���@�t�@�C�@�
=@��H@�ȴ@��!@��+@�E�@�@���@�@�G�O�@�33@�V@�&@|[�@q�@fq�@`�I@Y}�@P_@G�0@Ac�@:�@5��@--w@'$t@#W?@��@��@�@{J@(�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�?B
�?B
�?B
�?B
�?B
�?B
�LB
�?B
�dBW
B�{B��B�BÖBɺB�B
=B'�B?}B�B�DB�1B�B~�By�Bu�B|�B|�B}�B~�B�B�B�+B�7B�=B�VB�bB�uB��B��B��B��B�B�B�B�'B�3B�?B�LB�FB�FB�FB�FB�FB�?B�3B�-B�!B�FB�-B�!B�B��B��B��B��B��B�uB�JB�+B�B�B�B� B|�Bx�Bu�Br�Bp�Bn�Bk�BhsBe`BcTBgmBhsBe`BbNBS�BC�B=qB2-B,B�B{B+BB��B��B�yB�;B��B��B�-B��B��B��B�Bo�BbNBT�B=qB-B!�B�BoB1B
��B
�yB
�HB
��B
��B
��B
�^B
��B
��B
��B
��B
�PB
y�B
o�B
jB
gmB
cTB
ZB
J�B
=qB
-B
!�B
uB

=B	��B	�B	��B	�XB	��B	��B	�{B	�%B	{�B	w�B	q�B	jB	gmB	e`B	`BB	VB	M�B	D�B	@�B	9XB	1'B	'�B	"�B	�B	�B	hB	VB	JB	DB	1B	B��B��B��B��B�mB�mB�yB�yB�fB�NB�5B�B��B��B��B�qB�XB�3B�B��B��B��B��B��B�{B�hB�JB�%B�B� Bz�Bu�B{�B� B� B}�B|�Bz�Bv�Bt�Bq�Bm�BjBhsBhsBffBe`BdZBbNB_;B\)BYBXBVBS�BO�BL�BK�BO�BP�B^5BhsBiyBo�Br�Bs�Bt�Bs�Br�By�B{�B|�B~�B� B� B~�B�B}�B}�B}�B|�B{�B�B�7B�\B�uB�uB�{B�uB�oB�\B�VB�PB�=B�7B�PB�uB�{B�{B�{B��B��B��B��B�uB�hB�hB�bB�bB�bB�\B�\B�oB�oB�uB�\B�DB�+B�B�%B�=B�=B�PB�bB�uB�uB�{B��B��B�B�B�3B�LB�RB�^B�dB�jBBȴB��B�
B�/B�TB�TB�ZB�`B�B�B�B�B�B�B�B��B��B	%B	
=B	
=B	DB	PB	{B	�B	�B	 �B	!�B	#�B	$�B	%�B	'�B	)�B	/B	/B	0!B	2-B	5?B	9XB	;dB	;dB	<jB	>wB	?}B	@�B	B�B	D�B	F�B	G�B	H�B	I�B	I�B	K�B	S�B	T�B	VB	W
B	XB	YB	YB	[#B	\)B	]/B	`BB	bNB	dZB	dZB	e`B	e`B	iyB	l�B	p�B	r�B	u�B	x�B	~�B	�B	�B	�+B	�=B	�DB	�JB	�VB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�9B	�B	�?B	�?B	�?B	�LB	�LB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	ÖB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	ɺB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�;B	�HB	�NB	�ZB	�`B	�`B	�ZB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
uB
�B
	B
(�B
-�B
1�B
:DB
B�B
G�B
N�B
Q�B
T,B
]/B
a|B
c:B
f�B
m�B
sB
u�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B
��B
��B
��B
��B
��B
��B
��BEqB��B�B�bB��B�B�[B��B<B-�BsaBy�BvuBr\Bm?Bh BdBk6Bk5Bl8BmDBr\BsdBuuBwyBx�B|�B~�B��B��B�B�0B�;B�BB�LB�VB�gB�qB��B��B��B��B��B��B��B�~B�oB�lB�aB��B�oB�dB�PB�2B�)B�%B�
B��B��Bz�BuxBsmBr_BpVBnKBk:Bg#BdB`�B^�B\�BY�BV�BS�BQ�BU�BV�BS�BP�BBHB1�B+�B �BdBB�B��B�wB�ZB� B��B͢B�-B��B��B�hB�3B��Bo|B^BP�BC{B+�B�BNBB �B
��B
�PB
�B
��B
ÏB
�~B
�cB
��B
�uB
�5B
�,B
�B
{�B
hzB
^<B
YB
VB
Q�B
H�B
9bB
,B
�B
uB
B	��B	�B	��B	�GB	�B	��B	�B	�:B	t�B	j�B	f�B	`uB	YJB	V7B	T*B	OB	D�B	<�B	3lB	/RB	((B	�B	�B	�B	�B	pB	 >B�1B�"B�B�
B��B��B�B�B�B�NB�JB�XB�YB�JB�.B�B��B��B��B�rB�XB�>B�B��B��B��B��B�zB�qB�kB�WB{:BuBsBn�Bi�Bd�Bj�Bn�Bn�Bl�Bk�Bi�Be�Bc�B`�B\�BYvBWoBWnBUbBT_BSWBQHBN7BK&BHBGBE BB�B>�B;�B:�B>�B?�BM7BWoBXxB^�Ba�Bb�Bc�Bb�Ba�Bh�Bj�Bk�Bm�Bo Bn�Bm�BpBl�Bl�Bl�Bk�Bj�BrBx3B~WB�qB�pB�yB�lB�fB~WB}RB|JBy:Bx3B|LB�oB�xB�uB�yB�zB��B��B�wB�sB�fB�hB[B]B^B~XB~VB�oB�lB�rB~[BzDBv+BsBu#By<By=B|OBbB�tB�vB�yB��B��B��B�B�1B�FB�OB�YB�`B�hB��B��B��B�B�&B�LB�JB�PB�TB܆BޓBޓBޒB�B��B�B�B��B�B�-B�-B�7B�@B	nB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	!B	$*B	(HB	*QB	*PB	+XB	-cB	.gB	/pB	1|B	3�B	5�B	6�B	7�B	8�B	8�B	:�B	B�B	C�B	D�B	E�B	F�B	HB	HB	JB	KB	LB	O+B	Q8B	SBB	SEB	TLB	TMB	XbB	[tB	_�B	a�B	d�B	g�B	m�B	p�B	r�B	vB	y"B	z)B	{,B	}=B	FB	CB	�RB	�_B	�pB	�lB	�tB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�!B	�!B	�B	�'B	�*B	�0B	�5B	�=B	�KB	�TB	�YB	�]B	�]B	�tB	�B	�B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�&B	�/B	�<B	�:B	�3B	�:B	�9B	�DB	�GB	�[B	�uB	�wB	�|B	�zB	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��G�O�B	�KB	��B
	�B
cB
�B
 �B
)B
1�B
6bB
=xB
@�B
B�B
K�B
PPB
RB
U�B
\�B
a�B
dzB
j�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.017(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941462019060409414620190604094146  AO  ARCAADJP                                                                    20180704220050    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180704220050  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180704220050  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094146  IP                  G�O�G�O�G�O�                