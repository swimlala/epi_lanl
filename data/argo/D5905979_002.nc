CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-18T14:13:35Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200618141335  20220204114410  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @؁|Oa�1   @؁|��\�@6У�
=q�c���S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=fD=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`fD`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�{D��D�b�D��D��RD�{D�S�D���D�ָD��D�a�D���D��qD�3D�L)Dڠ�D���D�)HD�^�D�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BH(�BOBWB_BgBoBwBB��HB�{B��HB��HB��B��HB��HB��HB��HB��HB��B��B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C:
>C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��D |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�D|)D�)D |)D �)D!|)D!�)D"u�D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D=�D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DAu�DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DX|)DX�)DY|)DY�)DZ|)DZ��D[|)D[�)D\|)D\�)D]|)D]�)D^|)D^�)D_|)D`�D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt��Dy��D� D�`�D��(D��fD��D�Q�D���D���D��D�_�D���D�ۅD�GD�J=Dڞ�D���D�'\D�\�D�(D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�^5A�^5A�XA�VA�S�A�VA�dZA�bNA�S�A�5?A�33A�;dA�1'A�"�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�
=A�-A��A̗�A��;AƝ�A�~�A�hsA�M�A��TA���A�=qA��HA�x�A�r�A��A��/A��!A��7A�1A�A�A��A�v�A�E�A���A���A��#A�A�z�A��A���A�A�K�A��!A�O�A�A��A�x�A�p�A�
=A�9XA���A�VA��A�(�A�?}A��A�p�A��9A��;A�jA�`BA�A�VA�jA���A���A�bNA�`BA��TA�|�A��hA�;dA�VA�?}A��A�`BA��A��^A�r�A���A�oA��^A�?}A���A���A�(�A�z�A�ĜA�p�A��A�x�A}�TA{
=Ayp�Ax�AwdZAuO�AsApQ�An~�Am��Al�HAl=qAj�RAi�Ai�AiAg�hAd��Aa��A`�A]l�A\E�A[�wA[�AZn�AX��AVr�AU�AS�^AQAP�RAP9XAO��ANȴAN{AM\)AKAI�AI
=AH5?AF-AE�ADv�ACVAB�!AB-AA33A?��A?/A>�9A=S�A<�yA<ZA<�A;��A:��A9��A8��A8VA7��A7�-A7l�A6�yA6r�A5��A5dZA3G�A2bA0-A/�wA/t�A.��A.  A,�!A+�FA*�9A)�A(-A'��A&ȴA%��A%A$�9A$��A$n�A#XA"��A!�TA!;dA bA�A��A��A��A��A�TAl�A�uAt�A�yAVA��AK�A��A&�A �A�PA�AȴA33A	�TA�RAE�AA+AG�AjAO�A�\A ��@�33@�n�@��
@�Ĝ@��@��+@���@���@�+@�$�@�hs@�V@��@�\@��#@�%@�Z@�n�@��/@�w@�"�@�J@�@�  @�!@�7@��@�1'@ߍP@�@�V@ܓu@� �@ە�@�"�@ڟ�@��T@أ�@��
@�+@և+@���@���@ԣ�@�j@�
=@���@���@θR@˶F@�M�@��#@ɩ�@�%@�K�@���@Ĭ@öF@��#@��D@��@�
=@��@���@���@�|�@��!@�`B@�S�@�J@��h@�&�@��@�  @�K�@���@��@�%@���@� �@�;d@��R@�n�@���@�?}@�Q�@���@��#@���@�j@�A�@�b@��F@�dZ@�o@�n�@�n�@���@���@�X@���@��@��m@��
@��w@���@�t�@�\)@�+@��@�ȴ@�^5@��@���@���@�O�@��`@��9@��u@��m@�"�@�^5@��T@��j@� �@���@�o@�^5@�J@�@�Ĝ@�A�@��m@��
@��F@��H@�$�@��#@�&�@��@�&�@��/@�Z@��
@�\)@�+@�@��@�ȴ@��+@�$�@�hs@��@��/@���@��w@�+@��@���@�^5@�5?@��@��@���@�X@��@���@�Q�@�A�@�I�@�1'@��w@�|�@�S�@�+@��@��+@�~�@�~�@�v�@�V@�=q@�-@�@��T@���@�@���@���@��T@��h@��@���@���@�z�@�r�@�Q�@�1@��@���@��m@��P@�S�@�33@�o@�@���@��y@��@��@���@�ȴ@���@�~�@�^5@�V@�=q@�5?@��@�{@��@��T@��#@���@�@���@�@��-@���@���@���@���@���@���@���@��h@��7@��@��@��@�`B@�?}@�V@���@��j@��D@��D@�r�@�Q�@�A�@���@��@��@�t�@�t�@�l�@�dZ@��@{@q�"@k�V@c�}@[��@Uu�@N{@I0�@B�@9�@4�I@.��@(�D@ �@��@t�@@��@x@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�^5A�^5A�XA�VA�S�A�VA�dZA�bNA�S�A�5?A�33A�;dA�1'A�"�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�
=A�-A��A̗�A��;AƝ�A�~�A�hsA�M�A��TA���A�=qA��HA�x�A�r�A��A��/A��!A��7A�1A�A�A��A�v�A�E�A���A���A��#A�A�z�A��A���A�A�K�A��!A�O�A�A��A�x�A�p�A�
=A�9XA���A�VA��A�(�A�?}A��A�p�A��9A��;A�jA�`BA�A�VA�jA���A���A�bNA�`BA��TA�|�A��hA�;dA�VA�?}A��A�`BA��A��^A�r�A���A�oA��^A�?}A���A���A�(�A�z�A�ĜA�p�A��A�x�A}�TA{
=Ayp�Ax�AwdZAuO�AsApQ�An~�Am��Al�HAl=qAj�RAi�Ai�AiAg�hAd��Aa��A`�A]l�A\E�A[�wA[�AZn�AX��AVr�AU�AS�^AQAP�RAP9XAO��ANȴAN{AM\)AKAI�AI
=AH5?AF-AE�ADv�ACVAB�!AB-AA33A?��A?/A>�9A=S�A<�yA<ZA<�A;��A:��A9��A8��A8VA7��A7�-A7l�A6�yA6r�A5��A5dZA3G�A2bA0-A/�wA/t�A.��A.  A,�!A+�FA*�9A)�A(-A'��A&ȴA%��A%A$�9A$��A$n�A#XA"��A!�TA!;dA bA�A��A��A��A��A�TAl�A�uAt�A�yAVA��AK�A��A&�A �A�PA�AȴA33A	�TA�RAE�AA+AG�AjAO�A�\A ��@�33@�n�@��
@�Ĝ@��@��+@���@���@�+@�$�@�hs@�V@��@�\@��#@�%@�Z@�n�@��/@�w@�"�@�J@�@�  @�!@�7@��@�1'@ߍP@�@�V@ܓu@� �@ە�@�"�@ڟ�@��T@أ�@��
@�+@և+@���@���@ԣ�@�j@�
=@���@���@θR@˶F@�M�@��#@ɩ�@�%@�K�@���@Ĭ@öF@��#@��D@��@�
=@��@���@���@�|�@��!@�`B@�S�@�J@��h@�&�@��@�  @�K�@���@��@�%@���@� �@�;d@��R@�n�@���@�?}@�Q�@���@��#@���@�j@�A�@�b@��F@�dZ@�o@�n�@�n�@���@���@�X@���@��@��m@��
@��w@���@�t�@�\)@�+@��@�ȴ@�^5@��@���@���@�O�@��`@��9@��u@��m@�"�@�^5@��T@��j@� �@���@�o@�^5@�J@�@�Ĝ@�A�@��m@��
@��F@��H@�$�@��#@�&�@��@�&�@��/@�Z@��
@�\)@�+@�@��@�ȴ@��+@�$�@�hs@��@��/@���@��w@�+@��@���@�^5@�5?@��@��@���@�X@��@���@�Q�@�A�@�I�@�1'@��w@�|�@�S�@�+@��@��+@�~�@�~�@�v�@�V@�=q@�-@�@��T@���@�@���@���@��T@��h@��@���@���@�z�@�r�@�Q�@�1@��@���@��m@��P@�S�@�33@�o@�@���@��y@��@��@���@�ȴ@���@�~�@�^5@�V@�=q@�5?@��@�{@��@��T@��#@���@�@���@�@��-@���@���@���@���@���@���@���@��h@��7@��@��@��@�`B@�?}@�V@���@��j@��D@��D@�r�@�Q�@�A�@���@��@��@�t�@�t�@�l�G�O�@��@{@q�"@k�V@c�}@[��@Uu�@N{@I0�@B�@9�@4�I@.��@(�D@ �@��@t�@@��@x@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBM�BM�BM�BM�BL�BM�BM�BM�BM�BK�BK�BL�BK�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BL�BM�BM�BM�BN�BN�BN�BW
Bo�B�7B�LB��B��B%�B/B$�BuBH�BiyB[#BW
BYBXBZBcTBs�Bt�Bv�Bz�B}�Br�B�B�+B�1B�7B�DB�B� Bz�Bs�BcTB_;B\)B]/B\)B_;B\)BT�BP�BN�BA�B0!B �BPB��B�mB�5B��BǮBƨB�^B�!B��B��B��B�Bo�B_;BW
BS�BH�B7LB'�B"�B�B�BVB
��B
�B
�mB
�/B
��B
�XB
�B
��B
��B
�oB
�PB
v�B
e`B
W
B
Q�B
H�B
=qB
.B
 �B
oB
JB
1B
B	��B	��B	��B	�B	�fB	�B	��B	�3B	��B	��B	��B	��B	�hB	�1B	{�B	s�B	p�B	hsB	aHB	^5B	[#B	W
B	R�B	N�B	H�B	>wB	:^B	49B	-B	$�B	!�B	�B	�B	uB	VB	+B	B	B��B��B��B��B��B��B�B�B�B�B�yB�sB�fB�ZB�HB�;B��B��B��BȴBƨBƨBĜB��B�wB�dB�LB�3B�'B�B��B��B��B��B��B��B��B��B��B��B�oB�PB�=B�B�B�B}�B|�Bv�Bv�Bt�Br�Bp�Bo�Bl�BhsBffBbNB[#BT�BM�BJ�BJ�BJ�BL�BJ�BI�BE�BD�B@�B:^B9XB<jB9XB8RB6FB5?B49B49B33B2-B2-B2-B2-B2-B1'B1'B1'B0!B0!B/B0!B/B/B1'B0!B1'B2-B33B49B5?B5?B49B5?B49B49B49B6FB6FB8RB:^B;dB<jB;dB;dB>wB=qBA�BC�BJ�BK�BO�BQ�BT�BYB[#BYBW
BYB]/B[#B[#B\)B^5B`BB`BBbNBcTBhsBjBk�Bl�Bm�Bn�Bp�Bt�Bu�Bv�Bv�Bx�B{�B{�B|�B~�B� B�B�1B�JB�bB�hB�uB�{B��B��B��B��B�B�!B�3B�FB�XB�jB��B��BÖBŢBƨBƨBǮBȴBɺB��B��B��B��B�B�B�B�B�;B�TB�`B�fB�yB�B�B�B�B��B��B��B	B	+B	1B	1B	PB	bB	bB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	#�B	%�B	(�B	1'B	5?B	6FB	6FB	=qB	@�B	A�B	B�B	E�B	F�B	G�B	G�B	J�B	N�B	Q�B	T�B	XB	[#B	]/B	`BB	cTB	ffB	gmB	hsB	l�B	r�B	t�B	y�B	z�B	{�B	z�B	{�B	|�B	}�B	� B	�B	�B	�%B	�7B	�\B	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�LB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�qB	�}B	��B	B	ÖB	ŢB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�HB	�NB	�TB	�TB	�TB	�TB	��B	��B
�B
^B
aB
�B
%�B
/iB
4B
:�B
BAB
HB
L0B
T{B
Z7B
^�B
cTB
gRB
k�B
r|B
wL111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BD�BD�BD�BD�BC�BD�BD�BD�BD�BB�BB�BC�BB�BB�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BD�BD�BE�BE�BE�BM�BfZB�B�B�@B�B�B%�B�B
(B?dB`'BQ�BM�BO�BN�BP�BZBjdBkjBmwBq�Bt�Bi_Bw�B}�B~�B�B��Bz�Bv�Bq�BjfBZBU�BR�BS�BR�BU�BR�BK�BG�BE�B8>B&�B}B
B�B�*B��BȫB�mB�hB�B��B��B�uB�EBx�BfeBVBM�BJ�B?~B.B�B�B�BaB%B
�B
�oB
�?B
�B
áB
�.B
��B
��B
�xB
�HB
�)B
m�B
\=B
M�B
H�B
?�B
4QB
$�B
�B
	SB
.B	�B	�B	��B	�B	�B	�B	�MB	��B	�nB	�B	��B	��B	�{B	�oB	�WB	!B	r�B	j�B	g�B	_fB	X;B	U(B	RB	M�B	I�B	E�B	?�B	5mB	1UB	+0B	$B	�B	�B	�B	�B	
oB	QB�&B�B�B��B��B��B��B��B�B�B�B�B�}B�wB�qB�eB�YB�GB�:B��B��B��B��B��B��B��B��B�zB�gB�OB�7B�+B�B�B��B��B��B��B��B��B��B��B��B�vB�XB�EB|'ByBxBt�Bs�Bm�Bm�Bk�Bi�Bg�Bf�Bc�B_B]rBYZBR0BLBD�BA�BA�BA�BC�BA�B@�B<�B;�B7�B1oB0iB3{B0jB/dB-XB,QB+KB+KB*EB)@B)@B)@B)@B)@B(:B(:B(:B'4B'4B&.B'4B&/B&/B(;B'5B(;B)AB*GB+MB,SB,SB+MB,SB+MB+MB+MB-ZB-ZB/fB1rB2xB3~B2xB2xB5�B4�B8�B:�BA�BB�BF�BH�BLBP*BR6BP*BNBP*BTBBR6BR6BS<BUHBWUBWUBYaBZgB_�Ba�Bb�Bc�Bd�Be�Bg�Bk�Bl�Bm�Bm�Bo�Br�Br�Bt BvBwBz$BCB�[B�sB�yB��B��B��B��B��B��B�B�0B�BB�UB�gB�xB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�$B�*B�*B�GB�`B�lB�rB��B�B�B�B�B��B��B�B�B�5B�;B�;B	ZB	lB	lB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	(/B	,GB	-NB	-NB	4xB	7�B	8�B	9�B	<�B	=�B	>�B	>�B	A�B	E�B	H�B	LB	OB	R(B	T4B	WGB	ZYB	]kB	^rB	_wB	c�B	i�B	k�B	p�B	q�B	r�B	q�B	r�B	s�B	t�B	wB	zB	|"B	}(B	�9B	�^B	�jB	�qB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�KB	�QB	�QB	�WB	�]B	�cB	�iB	�iB	�pB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�,B	�,B	�2B	�8B	�EB	�KB	�QB	�QB	�QG�O�B	��B	��B	��B
YB
\B
�B
�B
&bB
*�B
1�B
9:B
?B
C)B
KsB
Q/B
U�B
ZLB
^JB
b�B
isB
nC111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.06 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.009(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144102022020411441020220204114410  AO  ARCAADJP                                                                    20200618141335    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200618141335  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200618141335  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114410  IP                  G�O�G�O�G�O�                