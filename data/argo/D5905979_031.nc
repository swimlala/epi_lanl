CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:00Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170900  20220204114413  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؎�Of�1   @؎��-� @8�
=p���c١���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B���B�  B�33B�33B�  B���B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�D�)D�P�D���D��=D�"�D�[�D���D���D�!HD�d{D��D���D��D�L{Dڢ=D�ڏD�fD�O\D�{D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_Bh(�BoBwBB��B��HB�{B�{B��HB��B��B��B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Dy�=D�=D�O
D���D��QD� �D�Y�D���D���D�\D�b�D��(D���D��D�J�DڠQD�أD�zD�MpD�D�Å111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�A�  A�A�A�  A�  A�  A�%A�1A�1A�1A�bA�$�A�+A�-A�$�A� �A�oA�VA��A�oA��A�{A�oA���A��A�ƨAҁA��HA�"�A϶FA�`BA�hsAʉ7AȅA�`BA�bA�9XA�5?A�bNA��;A���A�z�A�(�A��7A�M�A�t�A�^5A�
=A��wA�C�A��A��A�`BA��9A�ƨA�7LA��9A��`A� �A��
A���A�ĜA��A��A�/A���A�`BA�&�A�jA��A��/A���A�K�A��A�K�A��A�/A��A��A���A�t�A���A��
A��9A��A�ffA�"�A���A�E�A��RA�JA��-A��yA�&�A��;A�bA�ȴA��7A�Q�A���A�\)A��A��DA�A�=qA��PA�
=A��PA�K�A��^A���A�A|n�Az��Ax�At�HAr�yAr9XAp�Ao
=AmO�AjĜAcS�Aa
=A`�jA`�A_��A`A�A]��A\�RA\A[?}AZ{AYO�AXv�AW/AV��AU��AUO�AT(�ARjAP�uAO\)ALȴAL�AKt�AI�#AH�AF��AE�
AE
=AB�HABbAA��A@�A?�A=��A:ffA7��A5��A4��A3�
A2�uA1�A1��A1dZA0��A0E�A/7LA-ƨA,n�A*�`A*ffA* �A)33A'�A&�A%�A#�^A"��A"v�A!��A �jAƨA?}AȴAA�A��A�\AA�RAVAE�AdZA��A�A�`AĜA �A�AjA��A�AZA�A�TA�-AC�A&�A��AjA"�A{A
�uA	A	��A	�hA�9A�Al�AO�A7LA��A1'A��A1'A7LA�\AA�A�FAO�A �!A �@��R@���@�
=@�Q�@�v�@��@��^@�&�@�C�@�{@�-@�O�@�  @�V@�`B@�ȴ@���@��@��@�w@畁@�K�@���@�ff@��@�@���@��@��T@��#@���@噚@�9@�\)@�-@�j@�;d@�V@��@��@���@ղ-@��@���@�1@ҟ�@���@�bN@��
@�;d@�J@͡�@���@˶F@�=q@�7L@ȼj@�l�@�O�@���@�Q�@��;@�dZ@§�@��@��@�b@���@��m@��@�@�5?@�p�@�9X@�ƨ@��P@�\)@�
=@���@���@���@�r�@�(�@��@�@���@�@�?}@��/@���@��F@�dZ@�@��@�ȴ@���@�n�@��@���@��-@�`B@�O�@�7L@��@���@���@��@�5?@�@��@��@���@�Ĝ@�bN@�(�@�  @���@�E�@�p�@�/@��/@��9@�j@�I�@� �@��@��m@��@�t�@�
=@�ȴ@��\@�5?@���@�7L@��u@���@�ƨ@���@��P@�t�@�+@��H@�~�@�V@�E�@�=q@��@�{@��@�E�@�-@���@�z�@���@��y@���@��y@��h@��@��-@���@�$�@��+@���@�+@��@���@�-@��j@�Q�@�9X@�I�@�1'@�b@��@���@���@���@���@��/@���@��@���@�33@��@�ȴ@���@�J@���@��h@��@�Ĝ@�j@�Q�@�33@�^5@��7@�`B@�7L@��`@��@�Z@��@��@�S�@�
=@�@��+@�^5@�-@�@��@�`B@�G�@�7L@�&�@��/@��9@���@�A�@�1'@�(�@��@���@���@��w@��@��F@��@��P@�t�@�K�@�33@�33@�;d@�33@�;d@��R@���@���@�~�@�n�@�-@���@���@��@�x�@�O�@�V@��/@���@��/@���@��8@{�m@o��@i�@`u�@V�@Ov`@I�@ES&@>kQ@8l"@0�@*xl@% \@"��@e@5?@�@��@�<@
�x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A�A�  A�A�A�  A�  A�  A�%A�1A�1A�1A�bA�$�A�+A�-A�$�A� �A�oA�VA��A�oA��A�{A�oA���A��A�ƨAҁA��HA�"�A϶FA�`BA�hsAʉ7AȅA�`BA�bA�9XA�5?A�bNA��;A���A�z�A�(�A��7A�M�A�t�A�^5A�
=A��wA�C�A��A��A�`BA��9A�ƨA�7LA��9A��`A� �A��
A���A�ĜA��A��A�/A���A�`BA�&�A�jA��A��/A���A�K�A��A�K�A��A�/A��A��A���A�t�A���A��
A��9A��A�ffA�"�A���A�E�A��RA�JA��-A��yA�&�A��;A�bA�ȴA��7A�Q�A���A�\)A��A��DA�A�=qA��PA�
=A��PA�K�A��^A���A�A|n�Az��Ax�At�HAr�yAr9XAp�Ao
=AmO�AjĜAcS�Aa
=A`�jA`�A_��A`A�A]��A\�RA\A[?}AZ{AYO�AXv�AW/AV��AU��AUO�AT(�ARjAP�uAO\)ALȴAL�AKt�AI�#AH�AF��AE�
AE
=AB�HABbAA��A@�A?�A=��A:ffA7��A5��A4��A3�
A2�uA1�A1��A1dZA0��A0E�A/7LA-ƨA,n�A*�`A*ffA* �A)33A'�A&�A%�A#�^A"��A"v�A!��A �jAƨA?}AȴAA�A��A�\AA�RAVAE�AdZA��A�A�`AĜA �A�AjA��A�AZA�A�TA�-AC�A&�A��AjA"�A{A
�uA	A	��A	�hA�9A�Al�AO�A7LA��A1'A��A1'A7LA�\AA�A�FAO�A �!A �@��R@���@�
=@�Q�@�v�@��@��^@�&�@�C�@�{@�-@�O�@�  @�V@�`B@�ȴ@���@��@��@�w@畁@�K�@���@�ff@��@�@���@��@��T@��#@���@噚@�9@�\)@�-@�j@�;d@�V@��@��@���@ղ-@��@���@�1@ҟ�@���@�bN@��
@�;d@�J@͡�@���@˶F@�=q@�7L@ȼj@�l�@�O�@���@�Q�@��;@�dZ@§�@��@��@�b@���@��m@��@�@�5?@�p�@�9X@�ƨ@��P@�\)@�
=@���@���@���@�r�@�(�@��@�@���@�@�?}@��/@���@��F@�dZ@�@��@�ȴ@���@�n�@��@���@��-@�`B@�O�@�7L@��@���@���@��@�5?@�@��@��@���@�Ĝ@�bN@�(�@�  @���@�E�@�p�@�/@��/@��9@�j@�I�@� �@��@��m@��@�t�@�
=@�ȴ@��\@�5?@���@�7L@��u@���@�ƨ@���@��P@�t�@�+@��H@�~�@�V@�E�@�=q@��@�{@��@�E�@�-@���@�z�@���@��y@���@��y@��h@��@��-@���@�$�@��+@���@�+@��@���@�-@��j@�Q�@�9X@�I�@�1'@�b@��@���@���@���@���@��/@���@��@���@�33@��@�ȴ@���@�J@���@��h@��@�Ĝ@�j@�Q�@�33@�^5@��7@�`B@�7L@��`@��@�Z@��@��@�S�@�
=@�@��+@�^5@�-@�@��@�`B@�G�@�7L@�&�@��/@��9@���@�A�@�1'@�(�@��@���@���@��w@��@��F@��@��P@�t�@�K�@�33@�33@�;d@�33@�;d@��R@���@���@�~�@�n�@�-@���@���@��@�x�@�O�@�V@��/@���@��/G�O�@��8@{�m@o��@i�@`u�@V�@Ov`@I�@ES&@>kQ@8l"@0�@*xl@% \@"��@e@5?@�@��@�<@
�x111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�5B�;B�NB�`B�sB�mB�yB�yB�BB�B-B;dBD�BE�BE�BE�BM�BN�B`BBo�B\)BQ�BYBm�B�%B~�B{�B�7B��B��B�VB�\B�DB�7B�=B�B� B}�B{�B|�Bx�Bv�Br�Bm�B^5BT�BN�BI�B/B�BbB%B�B�yB�`B�NB�HB�BB�B��B��B��BȴBȴBɺBȴBŢBB��B�wB�jB�RB�3B�B��B��B�%Bl�B\)BW
BH�B@�B;dB6FB,B�B
��B
��B
�B
�5B
��B
�}B
��B
�B
r�B
dZB
G�B
&�B
uB	��B	�/B	B	ÖB	��B	�
B	��B	�?B	s�B	S�B	R�B	O�B	W
B	}�B	�B	y�B	u�B	q�B	k�B	e`B	`BB	ZB	T�B	R�B	N�B	J�B	?}B	:^B	2-B	)�B	"�B	$�B	�B	bB	+B	B��B��B�B�B�fB�/B�BB�?B�!B��B��B��B��B��B��B��B��B��B��B��B�\B�PB�VB�7B�B}�Bw�Bp�Bm�Bk�BjBjBhsBffBdZBaHB^5BXBW
BW
BVBT�BR�BN�BI�BG�BF�BD�BC�B>wB=qB=qB;dB:^B:^B:^B9XB8RB7LB8RB5?B49B33B0!B0!B/B1'B/B.B.B-B-B,B,B,B(�B'�B'�B&�B$�B#�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B$�B&�B&�B'�B(�B)�B+B,B0!B5?B8RB:^B<jB=qB<jB?}BD�BE�BG�BG�BG�BI�BM�BP�BP�BQ�BS�BVBVBW
BW
BZBZB[#B]/BaHBaHBbNBcTBdZBffBjBl�Bn�Bo�Bq�Bq�Bt�Bu�Bw�B{�B}�B~�B� B� B�B�1B�VB�\B�bB�{B��B��B��B��B��B��B�B�B�B�B�!B�-B�9B�dB�}BBɺB��B��B��B�B�5B�NB�`B�mB�B�B�B��B��B��B��B��B	1B	JB	\B	bB	hB	hB	oB	uB	�B	�B	�B	�B	�B	 �B	#�B	$�B	&�B	'�B	(�B	'�B	+B	-B	.B	.B	-B	1'B	49B	5?B	5?B	5?B	5?B	6FB	7LB	8RB	:^B	<jB	=qB	>wB	;dB	7LB	2-B	0!B	0!B	2-B	33B	6FB	;dB	=qB	B�B	D�B	C�B	E�B	B�B	@�B	C�B	G�B	G�B	G�B	H�B	I�B	K�B	M�B	P�B	T�B	YB	\)B	^5B	`BB	bNB	e`B	iyB	n�B	q�B	t�B	x�B	|�B	�B	�B	�B	�%B	�+B	�+B	�+B	�1B	�7B	�=B	�=B	�=B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�?B	�?B	�FB	�RB	�dB	�dB	�dB	�wB	�}B	��B	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�5B	�HB	�HB	�HB	�NB	�NB	�NB	�TB	�`B	�`B	�B	��B
JB
oB
xB
&�B
+QB
1�B
88B
@ B
FB
N�B
TFB
X�B
\]B
a-B
dB
i�B
lqB
p�B
tT111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B� B�PB�VB�uBՇB֍B٠BܱB��B޾B��B��B��B�nB�B$[B2�B;�B<�B<�B<�BEBF%BW�Bf�BSuBI9BPdBd�B}pBvFBs3B��B��B��B��B��B��B��B��ByYBwMBuBBs5Bt<Bp#BnBi�Bd�BU�BLPBF+BAB&pBB�B�}B�B��BܺB٩BأBםB�rB�TB�;B�B�B�B�B�B��B��B��B��B��B��B��B�gB�1B��B}�Bc�BS�BNpB@B7�B2�B-�B#qB�B
�`B
�<B
�B
դB
�\B
��B
� B
y�B
j'B
[�B
?(B
eB

�B	�sB	ԱB	�B	�B	�PB	΍B	�EB	��B	k@B	K�B	JB	GlB	N�B	u~B	|�B	qfB	mNB	i5B	cB	\�B	W�B	Q�B	L�B	J�B	FgB	BPB	7B	1�B	)�B	!�B	cB	oB	9B	�B��B��B�B�RB�:B�"B��B��B͜B�)B��B��B��B��B��B�nB�nB�hB�nB�tB�VB�JB�1B��B��B��B��By�Bu�BopBhFBe3Bc'Bb!Bb"B`B^	B[�BX�BU�BO�BN�BN�BM�BL�BJ�BF~BA`B?TB>NB<BB;=B6B5B5B3B2B2B2B1 B/�B.�B/�B,�B+�B*�B'�B'�B&�B(�B&�B%�B%�B$�B$�B#�B#�B#�B �B�B�B�B�B�B|BvBpBpBjBdB^BXB^B^B^BXBXB^BRBMBYB_BeBeBkBkBxB~B�B�B�B�B �B!�B"�B#�B'�B,�B/�B2	B4B5B4B7(B<GB=MB?YB?YB?YBAeBE~BH�BH�BI�BK�BM�BM�BN�BN�BQ�BQ�BR�BT�BX�BX�BY�BZ�B\B^Bb)Bd5BfBBgHBiTBiTBlfBmmBoyBs�Bu�Bv�Bw�Bw�By�B�B��B�B�
B�#B�5B�BB�mB��B��B��B��B��B��B��B��B��B��B�
B�#B�5B�_B�rB�~BʗBͩB��B��B�B�B�4B�MB�MB�_B�rB�~B�B��B��B	�B	�B	B			B			B	
B	B	"B	"B	4B	AB	YB	eB	wB	}B	�B	�B	 �B	�B	"�B	$�B	%�B	%�B	$�B	(�B	+�B	,�B	,�B	,�B	,�B	-�B	.�B	/�B	1�B	4	B	5B	6B	3B	.�B	)�B	'�B	'�B	)�B	*�B	-�B	3B	5B	:.B	<;B	;5B	=AB	:.B	8"B	;5B	?MB	?MB	?MB	@SB	AYB	CfB	ErB	H�B	L�B	P�B	S�B	U�B	W�B	Y�B	\�B	aB	f5B	iGB	lYB	pqB	t�B	x�B	{�B	|�B	}�B	~�B	~�B	~�B	�B	��B	��B	��B	��B	��B	�
B	�(B	�5B	�AB	�FB	�XB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�4B	�RB	�kB	�pB	�vB	�|B	̕B	̕B	͛B	ΡB	ϧB	ҺB	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�PB	�B
�B

B
B
IB
"�B
)qB
/�B
7�B
=�B
FlB
K�B
P�B
S�B
X�B
[�B
a%B
dB
h5B
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144132022020411441320220204114413  AO  ARCAADJP                                                                    20200619170900    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170900  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170900  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114413  IP                  G�O�G�O�G�O�                