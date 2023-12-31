CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:37Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141337  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؂��1   @؂���]�@7	7KƧ��c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�33A�  B   B  B  B  B   B(  B0  B933B>ffBG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CE�fCG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQy�DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�fD�b�D���D��3D�	�D�eD��=D���D�)D�^fD��D��fD��D�MDڏ\D��\D��D�b=D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�{@�{A
=A?
=A_
=A
=A��A��A��A��A�Q�A޸RA�A��BBBBB'B/B8��B>(�BG\)BOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB�{B��HB��HBǮB��HB��HB��HB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�
CG�
CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��C��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)Du�D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D��D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5u�D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQu�DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DW�DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ�)D[u�D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Dy��D�zD�`�D��
D��GD��D�c3D��QD���D�=D�\zD��3D��zD��D�K3DڍpD��pD��D�`QD�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӴ9AӴ9AӴ9AӴ9AӶFAӸRAӺ^AӺ^AӺ^AӸRAӺ^AӼjAӴ9AӼjA��TA� �A�I�A�?}A�%A��A��yA�\)A���A��AǸRA�bNA�dZA�  A��hA�I�A�ȴA���A���A��hA�  A�/A���A���A���A�oA���A��A�p�A�A�S�A�&�A��mA��A�jA� �A�p�A�A���A��mA�dZA�1'A�/A�dZA��;A�33A�ZA��A�1A���A��!A��A���A���A�dZA�I�A�
=A�^5A�A���A�z�A�n�A�I�A�n�A�M�A�M�A���A�v�A�A�A��A�^5A�C�A�  A�A�A��TA��A�oA���A�+A�XA��A���A��A�A"�A}��A{K�Ay��Ax^5Awl�Au��AtjAs/Aq��Ao��Am?}Aip�Ah�AhjAg+AfE�Ae��Ae+Ad�yAb1'Aa33A`I�A^�uA]��A]?}A\��AZ�jAY|�AX�uAVA�AS��APr�AO+AN^5AMdZALbAJVAH  AE�TAEVADM�ACS�AC7LAB�jAA�-A@ZA@1'A@1A??}A>~�A=�wA<��A<�A;&�A:^5A9��A9`BA9A8��A7��A7\)A6��A6I�A5�A533A4�9A3p�A1l�A0A/��A/�A-dZA+��A*��A)�
A(�9A(=qA'�PA&ĜA%�A$I�A#��A#%A"�\A"5?A"�A!�;A!A!��A!�hA!O�A��Av�A�uA��AVA�A9XA��A�yA��A=qA��AA�jA�-AO�A�AjA��A��AG�A��A
n�A	�PA	
=A	
=A	A��AbNA1A�/A�wAt�AO�A�RA$�A;dAĜA��A�A\)A7LA�A ��A ��A �+A b@�dZ@��R@�I�@�^5@�9X@�ƨ@�9X@�P@�+@�J@�j@��@��H@�@��@�M�@�X@���@ܣ�@�ȴ@���@ف@���@�(�@���@Ցh@Ԭ@ӕ�@��@ЋD@�l�@�o@�ȴ@�J@̣�@���@ɺ^@��/@��m@���@�=q@Ĭ@�;d@�@�-@�@���@���@��@�~�@��@���@�dZ@��H@�ff@�J@���@���@���@���@�J@��7@�bN@�@���@�ff@�@��@��u@�A�@�K�@���@�Ĝ@���@���@�J@��@��@�z�@�9X@��@�ƨ@���@���@�p�@�/@��@��9@�bN@��
@���@�dZ@��y@��R@�K�@��F@��@��\@�M�@���@�/@�j@��9@�z�@�1'@��m@��P@��@���@��\@�^5@���@�hs@���@��u@��@�ƨ@��F@�l�@�v�@�E�@�=q@�=q@���@��h@�@�@���@�x�@�/@���@�@�5?@�G�@�/@�%@��/@��D@�1'@�bN@�z�@�(�@���@���@�O�@�V@��j@��@�Z@�ƨ@�l�@�C�@��
@�1@�  @��
@��@���@�K�@��@��\@�ff@�-@���@�&�@�Ĝ@�(�@��;@��@�dZ@��@��y@�\)@��m@���@��
@��@��@�l�@�K�@��@�~�@�$�@��^@�?}@���@���@���@��u@�r�@�1'@�b@���@���@��F@�S�@�o@�@���@��@��@��+@�n�@�ff@�^5@�V@�E�@�-@�{@�{@�@��T@��^@�7L@�V@�V@���@��9@���@�V@��@���@�Ĝ@���@��@�r�@�Q�@� �@��m@��
@��F@���@�K�@�o@��@�ȴ@���@�~�@�V@��@��#@��^@���@�X@�&�@��@���@���@��j@���@��D@�A�@�Ĝ@}�@t��@kƨ@d1@]8�@U�@O�q@F��@=��@8�@2($@,7�@&��@!�@�@�x@ϫ@u%@��@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AӴ9AӴ9AӴ9AӴ9AӶFAӸRAӺ^AӺ^AӺ^AӸRAӺ^AӼjAӴ9AӼjA��TA� �A�I�A�?}A�%A��A��yA�\)A���A��AǸRA�bNA�dZA�  A��hA�I�A�ȴA���A���A��hA�  A�/A���A���A���A�oA���A��A�p�A�A�S�A�&�A��mA��A�jA� �A�p�A�A���A��mA�dZA�1'A�/A�dZA��;A�33A�ZA��A�1A���A��!A��A���A���A�dZA�I�A�
=A�^5A�A���A�z�A�n�A�I�A�n�A�M�A�M�A���A�v�A�A�A��A�^5A�C�A�  A�A�A��TA��A�oA���A�+A�XA��A���A��A�A"�A}��A{K�Ay��Ax^5Awl�Au��AtjAs/Aq��Ao��Am?}Aip�Ah�AhjAg+AfE�Ae��Ae+Ad�yAb1'Aa33A`I�A^�uA]��A]?}A\��AZ�jAY|�AX�uAVA�AS��APr�AO+AN^5AMdZALbAJVAH  AE�TAEVADM�ACS�AC7LAB�jAA�-A@ZA@1'A@1A??}A>~�A=�wA<��A<�A;&�A:^5A9��A9`BA9A8��A7��A7\)A6��A6I�A5�A533A4�9A3p�A1l�A0A/��A/�A-dZA+��A*��A)�
A(�9A(=qA'�PA&ĜA%�A$I�A#��A#%A"�\A"5?A"�A!�;A!A!��A!�hA!O�A��Av�A�uA��AVA�A9XA��A�yA��A=qA��AA�jA�-AO�A�AjA��A��AG�A��A
n�A	�PA	
=A	
=A	A��AbNA1A�/A�wAt�AO�A�RA$�A;dAĜA��A�A\)A7LA�A ��A ��A �+A b@�dZ@��R@�I�@�^5@�9X@�ƨ@�9X@�P@�+@�J@�j@��@��H@�@��@�M�@�X@���@ܣ�@�ȴ@���@ف@���@�(�@���@Ցh@Ԭ@ӕ�@��@ЋD@�l�@�o@�ȴ@�J@̣�@���@ɺ^@��/@��m@���@�=q@Ĭ@�;d@�@�-@�@���@���@��@�~�@��@���@�dZ@��H@�ff@�J@���@���@���@���@�J@��7@�bN@�@���@�ff@�@��@��u@�A�@�K�@���@�Ĝ@���@���@�J@��@��@�z�@�9X@��@�ƨ@���@���@�p�@�/@��@��9@�bN@��
@���@�dZ@��y@��R@�K�@��F@��@��\@�M�@���@�/@�j@��9@�z�@�1'@��m@��P@��@���@��\@�^5@���@�hs@���@��u@��@�ƨ@��F@�l�@�v�@�E�@�=q@�=q@���@��h@�@�@���@�x�@�/@���@�@�5?@�G�@�/@�%@��/@��D@�1'@�bN@�z�@�(�@���@���@�O�@�V@��j@��@�Z@�ƨ@�l�@�C�@��
@�1@�  @��
@��@���@�K�@��@��\@�ff@�-@���@�&�@�Ĝ@�(�@��;@��@�dZ@��@��y@�\)@��m@���@��
@��@��@�l�@�K�@��@�~�@�$�@��^@�?}@���@���@���@��u@�r�@�1'@�b@���@���@��F@�S�@�o@�@���@��@��@��+@�n�@�ff@�^5@�V@�E�@�-@�{@�{@�@��T@��^@�7L@�V@�V@���@��9@���@�V@��@���@�Ĝ@���@��@�r�@�Q�@� �@��m@��
@��F@���@�K�@�o@��@�ȴ@���@�~�@�V@��@��#@��^@���@�X@�&�@��@���@���@��j@���@��DG�O�@�Ĝ@}�@t��@kƨ@d1@]8�@U�@O�q@F��@=��@8�@2($@,7�@&��@!�@�@�x@ϫ@u%@��@
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
��BB�Bk�B�FB�B#�BA�B[#BhsBl�Bx�B�B�{B��B�^B�dB�wBŢBɺB��B�B��B��B��B��BB�^B�?B�-B�B�B�B�B�B�B��B��B��B��B�B� B~�B|�B{�BjBcTBZBVBK�BD�B,B �BuB��B�B�B�;B�
B��B��BǮB�LB��B��B��B��B��B�1B|�Bt�B`BBM�B;dB!�B\B
��B
�TB
��B
�}B
�3B
�B
��B
��B
�{B
�\B
�PB
�+B
� B
u�B
gmB
\)B
P�B
K�B
@�B
5?B
.B
"�B
�B
DB	�B	�B	�yB	�`B	�BB	�5B	�B	�B	��B	B	�qB	�LB	�B	�B	��B	��B	�uB	�JB	� B	q�B	YB	N�B	G�B	@�B	8RB	+B	�B	\B	PB	JB	%B	%B	B	B	B	B	%B	%B	B	  B��B��B��B�B�B�B�B�B�fB�ZB�TB�BB�/B�B�B��BȴB��B�wB�jB�LB�9B�'B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�+B�B� Bz�Bx�Bv�Bt�Br�Bk�BiyBdZB`BB\)BZBXBVBS�BP�BO�BO�BL�BK�BH�BH�BH�BG�BF�BF�BF�BC�BB�BA�BB�B@�B?}B=qB=qB=qB<jB;dB;dB:^B:^B9XB<jB:^B9XB:^B7LB33B5?B1'B1'B0!B.B.B0!B,B-B/B.B1'B49B49B?}B@�B@�B@�BA�BD�BF�BG�BG�BI�BK�BL�BM�BM�BN�BP�BR�BS�BVBXBYBZB_;BcTBdZBdZBe`BgmBk�Bl�Bm�Bm�Bp�Bq�Bq�Br�Br�Br�Bs�Br�Br�Bs�Bs�Bw�By�Bz�Bz�B~�B�B�B�B�7B�7B�DB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�XB��BǮB��B��B��B��B��B�
B�B�#B�5B�HB�ZB�ZB�ZB�`B�yB�B�B�B��B��B��B	  B	
=B	DB	JB	hB	�B	�B	�B	 �B	#�B	$�B	&�B	%�B	#�B	"�B	!�B	"�B	#�B	$�B	#�B	#�B	%�B	/B	/B	;dB	E�B	H�B	I�B	I�B	I�B	I�B	I�B	J�B	O�B	XB	\)B	cTB	dZB	ffB	gmB	iyB	k�B	m�B	n�B	p�B	q�B	s�B	s�B	t�B	t�B	t�B	t�B	u�B	w�B	{�B	�B	�B	�B	�%B	�+B	�+B	�1B	�1B	�1B	�1B	�7B	�DB	�JB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�3B	�LB	�RB	�^B	�jB	�wB	��B	��B	B	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�DB	�+B
�B
�B
B
 vB
($B
/�B
8RB
A�B
F�B
L�B
R:B
X�B
]dB
bB
ezB
j�B
m�B
q�B
v�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
��B}BbVB�B�rB�B8KBQ�B_3BcJBo�By�B�9B�|B�B�!B�4B�^B�vBÉB̿BȨBÊBÊBÊB�MB�B��B��B��B��B��B��B��B��B��B��B��B�cBz�Bv�Bu�Bs�Br�BaFBZBP�BL�BB�B;gB"�B�B
DB�B�B�QB�B��B��BǹB��B�"B��B��B��B��B�fBBs�Bk�BW BD�B2EB�B@B
��B
�<B
÷B
�hB
�B
��B
��B
��B
�iB
�KB
�?B
~B
v�B
l�B
^_B
SB
G�B
B�B
7xB
,5B
%B
�B
�B
>B	�B	�B	�vB	�]B	�?B	�3B	�B	�B	��B	��B	�qB	�MB	�B	�B	��B	��B	�yB	�OB	wB	h�B	P B	E�B	>�B	7�B	/^B	"B	�B	kB	_B	ZB�5B�5B�)B�B�B�0B�6B�6B�B�B��B��B��B��B�B�B�B�B�zB�nB�hB�VB�CB�2B�B�B��B��B��B��B�dB�QB�@B�FB�:B�4B�4B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�yB~HB{7BwBr Bo�Bm�Bk�Bi�Bb�B`�B[{BWcBSKBQ?BO2BM&BKBHBGBGBC�BB�B?�B?�B?�B>�B=�B=�B=�B:�B9�B8�B9�B7�B6�B4�B4�B4�B3�B2�B2�B1�B1�B0~B3�B1�B0~B1�B.sB*ZB,fB(OB(OB'IB%<B%<B'IB#1B$7B&DB%=B(PB+bB+bB6�B7�B7�B7�B8�B;�B=�B>�B>�B@�BB�BC�BD�BD�BFBHBJBK BM,BO8BP?BQEBVcBZ{B[�B[�B\�B^�Bb�Bc�Bd�Bd�Bg�Bh�Bh�Bi�Bi�Bi�Bj�Bi�Bi�Bj�Bj�Bn�BqBrBrBv!Bx-B{?B|EB�]B�]B�jB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�3B�WB�|B��B��B��B��B��B��B�B�,B�?B�EB�VB�iB�{B�{B�{B܁B��B�B��B��B��B��B�B� B	\B	cB	iB	�B	�B	�B	�B	�B	�B	�B	B	 B	�B	�B	�B	�B	�B	�B	�B	�B	 B	&8B	&8B	2�B	<�B	?�B	@�B	@�B	@�B	@�B	@�B	A�B	F�B	O*B	SCB	ZmB	[sB	]B	^�B	`�B	b�B	d�B	e�B	g�B	h�B	j�B	j�B	k�B	k�B	k�B	k�B	l�B	n�B	r�B	y$B	z)B	{0B	}<B	~BB	~BB	HB	HB	HB	HB	�NB	�[B	�aB	�gB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�0B	�0B	�0B	�<B	�HB	�`B	�fB	�rB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�"B	�)B	�/B	�5B	�;B	�AB	�GB	�MB	�MB	�MB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fG�O�B	�UB	�<B	�B
�B
B
�B
3B
&�B
/`B
8�B
=�B
C�B
IHB
P	B
TqB
Y&B
\�B
a�B
d�B
iB
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.003) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141337    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141337  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141337  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                