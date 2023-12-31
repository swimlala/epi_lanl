CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-05-13T12:37:46Z creation;2019-05-13T12:37:51Z conversion to V3.1;2019-12-18T07:15:09Z update;2022-11-21T05:28:58Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z          :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   B4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       D<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   LP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       NX   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   Vl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       Xt   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       b�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   j�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       l�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   t�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       v�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ~�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190513123746  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_173                     2C  Dd/9NAVIS_A                         0397                            ARGO 011514                     863 @ؽ�$8!�1   @ؽ��Q��@;���s��d/9����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DXy�DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�L�D�c31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��H@�z�@�{@�{A
=A?
=A_
=A
=A��A��A��A��AυA߅A�A��BBBBB'B/B7B?BGBOBWB_BgBoBwBB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HC�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RC��RD |)D �)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D	|)D	�)D
|)D
�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D|)D�)D |)D �)D!|)D!�)D"|)D"�)D#|)D#�)D$|)D$�)D%|)D%�)D&|)D&�)D'|)D'�)D(|)D(�)D)|)D)�)D*|)D*�)D+|)D+�)D,|)D,�)D-|)D-�)D.|)D.�)D/|)D/�)D0|)D0�)D1|)D1�)D2|)D2�)D3|)D3�)D4|)D4�)D5|)D5�)D6|)D6�)D7|)D7�)D8|)D8�)D9|)D9�)D:|)D:�)D;|)D;�)D<|)D<�)D=|)D=�)D>|)D>�)D?|)D?�)D@|)D@�)DA|)DA�)DB|)DB�)DC|)DC�)DD|)DD�)DE|)DE�)DF|)DF�)DG|)DG�)DH|)DH�)DI|)DI�)DJ|)DJ�)DK|)DK�)DL|)DL�)DM|)DM�)DN|)DN�)DO|)DO�)DP|)DP�)DQ|)DQ�)DR|)DR�)DS|)DS�)DT|)DT�)DU|)DU�)DV|)DV�)DW|)DW�)DXu�DX�)DY|)DY�)DZ|)DZ�)D[|)D[�)D\|)D\�)D]��D]�)D^|)D^�)D_|)D_�)D`|)D`�)Da|)Da�)Db|)Db�)Dc|)Dc�)Dd|)Dd�)De|)De�)Df|)Df�)Dg|)Dg�)Dh|)Dh�)Di|)Di�)Dj|)Dj�)Dk|)Dk�)Dl|)Dl�)Dm|)Dm�)Dn|)Dn�)Do|)Do�)Dp|)Dp�)Dq|)Dq�)Dr|)Dr�)Ds|)Ds�)Dt|)Dt�)Du|)Du�)Dv|)Dv�)Dw|)Dw�)Dx|)Dx�)Dy|)Dy�)Dz|)Dz�)D{|)D{�)D||)D|�)D}|)D}�)D~|)D~�)D|)D�)D�>D�~D��D��D�J�D�aH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��\A��A�bA�=qA��/A��-A���A��DA�z�A�jA�\)A�K�A�?}A�&�A���A�-A��;A��9A�jA��A���A�x�A�A�?}A�I�A�=qA�?}A�^5A�~�A�E�A�dZA�VA�jA��\A���A��FA�Q�A�ȴA�z�A��A��A�7LA���A�`BA��A��A��
A���A���A�A��A��A�+A��FA�&�A��jA���A���A��A�1'A�5?A�l�A�v�A��wA�=qA���A���A���A�-A��A��A��wA�r�A���A���A���A�VAƨA}�A{��Az�9AxAu�hAt$�ArjAp�An��AmAkdZAj��Aj��Ah�+AhE�Af��Ae��Acl�AcoAb��Ab��Ab�+AbbAa��Aa��AaO�A`ZA^��A]�FA\��AZ��AX�DAW�-AWx�AW33AV��AVffAS|�AQt�AP�/AP�DAN�jAL��AL��AKO�AJ�`AJ1'AI&�AF~�AE�;AEp�AE�ADjAC��ACx�ACC�AB��AB5?AA\)A@�!A@z�A?��A>�\A;�A;?}A:�A:�uA:M�A9dZA8��A8�\A8I�A7�wA7|�A7/A6��A5�hA3�A3`BA3C�A2�jA2A�A1��A0�A01A/;dA-�A,�RA+t�A*�A*I�A)��A((�A'`BA&��A&�+A%��A%%A$I�A#�A#`BA"n�A!�A �9AoA(�AA|�A�A�`Ar�A�TAp�A?}A+A�\A��A�RA1AK�Az�A �A��A
=A�#AS�A�uA�#A�A��A^5A��A��A�
A��A\)A
�A	��AbA/A�AG�A1'A&�A �/A -@���@��@��@���@�9X@�o@��@��9@�I�@��w@�K�@��@��@�F@�7@��
@�@�dZ@��H@�F@ꟾ@�V@�7@��@�V@��@��@�@�@�@�j@��m@�+@�J@�O�@�r�@�V@�1@�ȴ@���@׍P@�J@�O�@�Q�@ӍP@�-@���@��@�K�@��@���@�~�@�E�@��@�X@̼j@�9X@˾w@���@��/@�Q�@�(�@�;d@�hs@��@���@���@�dZ@��\@��7@�Q�@���@��@�t�@�S�@��H@�M�@���@��@�Q�@��@��@�G�@�z�@���@��R@��7@��/@�1@��@�1@�t�@�"�@��@��@��@��@�-@�@�X@���@��@��@���@��@�Q�@���@�n�@��T@��@��@�j@��m@��@�|�@�33@���@���@�~�@�ff@�M�@�-@���@���@�7L@��u@�b@���@���@��@�\)@��@�ȴ@��@��@��/@�(�@��P@�t�@�t�@�K�@�o@���@�J@�`B@�Ĝ@��@�Z@��@��F@�S�@�@��!@�ff@���@��T@��-@���@�p�@�?}@��@��@�1'@���@��m@���@��F@���@���@��P@�\)@�33@�o@�
=@��y@���@�^5@�-@���@��T@���@���@�G�@�1@�C�@���@�v�@���@��7@�X@���@��j@���@�j@� �@���@��@��;@��
@��F@���@���@���@���@���@���@���@��@��P@���@�l�@�\)@�K�@�;d@�33@�+@�+@�"�@�o@��H@�ȴ@��R@���@�=q@�@��T@��^@��h@�p�@�O�@�G�@��/@�Q�@�(�@�  @�@~v�@~ff@~$�@}�@}�@}V@|�@{��@{�@{dZ@{S�@z�H@y�#@yhs@y7L@x��@x1'@w|�@v��@v5?@u��@t�@t�D@tz�@t9X@s�m@s��@sdZ@r�@r��@r�\@rn�@r^5@rM�@r=q@r�@rJ@q�#@qx�@p��@o
=@n5?@m�h@l�/@k��@k��@k�@j��@j^5@ihs@iG�@i7L@i&�@i�@i�@h��@h��@h��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��\A��A�bA�=qA��/A��-A���A��DA�z�A�jA�\)A�K�A�?}A�&�A���A�-A��;A��9A�jA��A���A�x�A�A�?}A�I�A�=qA�?}A�^5A�~�A�E�A�dZA�VA�jA��\A���A��FA�Q�A�ȴA�z�A��A��A�7LA���A�`BA��A��A��
A���A���A�A��A��A�+A��FA�&�A��jA���A���A��A�1'A�5?A�l�A�v�A��wA�=qA���A���A���A�-A��A��A��wA�r�A���A���A���A�VAƨA}�A{��Az�9AxAu�hAt$�ArjAp�An��AmAkdZAj��Aj��Ah�+AhE�Af��Ae��Acl�AcoAb��Ab��Ab�+AbbAa��Aa��AaO�A`ZA^��A]�FA\��AZ��AX�DAW�-AWx�AW33AV��AVffAS|�AQt�AP�/AP�DAN�jAL��AL��AKO�AJ�`AJ1'AI&�AF~�AE�;AEp�AE�ADjAC��ACx�ACC�AB��AB5?AA\)A@�!A@z�A?��A>�\A;�A;?}A:�A:�uA:M�A9dZA8��A8�\A8I�A7�wA7|�A7/A6��A5�hA3�A3`BA3C�A2�jA2A�A1��A0�A01A/;dA-�A,�RA+t�A*�A*I�A)��A((�A'`BA&��A&�+A%��A%%A$I�A#�A#`BA"n�A!�A �9AoA(�AA|�A�A�`Ar�A�TAp�A?}A+A�\A��A�RA1AK�Az�A �A��A
=A�#AS�A�uA�#A�A��A^5A��A��A�
A��A\)A
�A	��AbA/A�AG�A1'A&�A �/A -@���@��@��@���@�9X@�o@��@��9@�I�@��w@�K�@��@��@�F@�7@��
@�@�dZ@��H@�F@ꟾ@�V@�7@��@�V@��@��@�@�@�@�j@��m@�+@�J@�O�@�r�@�V@�1@�ȴ@���@׍P@�J@�O�@�Q�@ӍP@�-@���@��@�K�@��@���@�~�@�E�@��@�X@̼j@�9X@˾w@���@��/@�Q�@�(�@�;d@�hs@��@���@���@�dZ@��\@��7@�Q�@���@��@�t�@�S�@��H@�M�@���@��@�Q�@��@��@�G�@�z�@���@��R@��7@��/@�1@��@�1@�t�@�"�@��@��@��@��@�-@�@�X@���@��@��@���@��@�Q�@���@�n�@��T@��@��@�j@��m@��@�|�@�33@���@���@�~�@�ff@�M�@�-@���@���@�7L@��u@�b@���@���@��@�\)@��@�ȴ@��@��@��/@�(�@��P@�t�@�t�@�K�@�o@���@�J@�`B@�Ĝ@��@�Z@��@��F@�S�@�@��!@�ff@���@��T@��-@���@�p�@�?}@��@��@�1'@���@��m@���@��F@���@���@��P@�\)@�33@�o@�
=@��y@���@�^5@�-@���@��T@���@���@�G�@�1@�C�@���@�v�@���@��7@�X@���@��j@���@�j@� �@���@��@��;@��
@��F@���@���@���@���@���@���@���@��@��P@���@�l�@�\)@�K�@�;d@�33@�+@�+@�"�@�o@��H@�ȴ@��R@���@�=q@�@��T@��^@��h@�p�@�O�@�G�@��/@�Q�@�(�@�  @�@~v�@~ff@~$�@}�@}�@}V@|�@{��@{�@{dZ@{S�@z�H@y�#@yhs@y7L@x��@x1'@w|�@v��@v5?@u��@t�@t�D@tz�@t9X@s�m@s��@sdZ@r�@r��@r�\@rn�@r^5@rM�@r=q@r�@rJ@q�#@qx�@p��@o
=@n5?@m�h@l�/@k��@k��@k�@j��@j^5@ihs@iG�@i7L@i&�@i�@i�@h��@h��@h��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B5?B.B)�B'�B'�B'�B'�B'�B'�B&�B&�B%�B$�B�B�B�BuBbBPB	7BB  B��B�B�BȴBÖB�wB�^B�-B�B��B��B�\B�B� B|�B|�B|�B{�Bo�BdZBZBO�B?}B1'B1'B49B"�B{BJBB��B�B�HB��B�dB��B��B�+Bw�BhsB[#BQ�BJ�B49B#�B�BhBB
��B
��B
�fB
��B
�9B
��B
�B
o�B
cTB
YB
F�B
6FB
+B
�B
bB
1B	��B	�B	�B	�B	�BB	�)B	��B	��B	�}B	�qB	�jB	�dB	�^B	�RB	�FB	�3B	�!B	��B	��B	��B	�hB	�B	w�B	v�B	v�B	v�B	u�B	r�B	e`B	YB	T�B	P�B	F�B	<jB	9XB	2-B	/B	+B	$�B	�B	�B	�B	�B	{B	hB	bB	bB	VB	JB	1B	%B	B	B��B�B�B�B�mB�sB�mB�TB�NB�HB�;B�5B�/B�#B�B��B��B��B��BȴBŢB��B�qB�^B�LB�-B�B��B��B��B��B��B��B��B��B��B�uB�bB�VB�=B�B~�B{�By�Bw�Bu�Bq�Bl�BjBiyBiyBhsBhsBhsBgmBdZBbNB_;B]/B_;B^5B[#BS�BQ�BN�BK�BI�BG�BF�BC�B?}B<jB;dB:^B8RB33B0!B-B(�B$�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{B�B�B�B�B�B�B�B{B{B{B{BuBuBuBoBoBhBbB\BVBPBDBJBPBVBVBVBVBVB\B\B\B\B\B\B\B\B\B\B\B\BhBhBbBhBoB{B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B%�B&�B'�B(�B)�B,B/B0!B2-B5?B>wB@�BA�BD�BH�BI�BL�BQ�BS�BT�BVBW
BYB\)BcTBffBk�Bm�Bo�Bs�Bt�Bu�Bw�Bx�By�Bz�B|�B}�B~�B~�B� B� B�B�B�B�1B�=B�DB�DB�JB�PB�VB�\B�{B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�FB�RB�^B�dB�jB�wBBÖBĜBŢBƨBƨBǮBǮB��B��B��B��B��B��B��B��B�B�
B�B�B�B�)B�5B�;B�HB�HB�NB�TB�ZB�B�B��B��B	B	1B	DB	\B	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	#�B	#�B	#�B	#�B	%�B	%�B	&�B	&�B	+B	-B	.B	0!B	2-B	49B	5?B	5?B	7LB	<jB	=qB	>wB	@�B	C�B	C�B	E�B	H�B	K�B	K�B	L�B	Q�B	R�B	R�B	R�B	T�B	ZB	\)B	\)B	^5B	`BB	bNB	e`B	gmB	iyB	n�B	o�B	o�B	p�B	q�B	s�B	t�B	v�B	v�B	x�B	y�B	z�B	{�B	{�B	|�B	}�B	}�B	~�B	�B	�%B	�7B	�DB	�VB	�bB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��B72B/�B*�B(>B($B($B($B($B(
B'B'8B&�B%�B �BQBB,BB"B
=B�BAB��B��B�B��B�SB�}B��B��B�)B��B��B�4B�%B�;B}�B~]B~�B}�Bp�Be�B[�BR:BA�B3B2aB5�B$@B�B�BMB�*B��B�B�4B��B��B��B�7By�BjB\xBS@BM�B6B%FB�B�BB
�B
��B
�B
��B
�B
�B
��B
qvB
eFB
\CB
IlB
8RB
-]B
"NB
:B

XB	��B	�hB	�B	��B	�-B	�B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�KB	��B	�YB	x�B	w2B	wLB	w�B	w2B	u�B	g�B	ZB	VB	SB	H�B	=VB	:�B	3B	0UB	,�B	'�B	�B	7B	EB	sB	2B	�B	�B	 B	\B	PB		B	�B	9B	-B��B��B�B�=B�
B�B�XB�B��B��B߾B��B�5B��B��BѝB�HBϫB͟BɺBƨB��B��B�PB��B��B�)B��B�$B��B��B�jB�xB��B��B��B�FB�B��B�xB��B��B}BzxBx�BwBs�Bm]BkQBjBi�Bh�Bi_Bi�Bh�Be`BcTB`BB]�B_�B_�B\�BT�BS&BO�BL�BJ�BHfBG�BD�B@�B<�B<B;dB:DB5ZB1�B/�B+�B&fB#�B"NB!�B BB BBpBOBIBxB]BWB�BBBB�B�B�BgB�B�BgB?B+B
BYB�B�B�B�B�B�B�B�B�B,B&BB:B�B�BBB<B�BPBBB�BBB(B�B�B�B�B�B�B�B�B�B�B�B.BbB�B�BNB�B�BgBmB�BEBeB�BCB�B�B�BB5B5BpB!|B"�B&�B'�B(�B)�B*�B,�B/�B0�B3MB6�B>�BABB[BEmBI7BJ�BM�BRTBTaBU�BVmBW�BZB]IBdBgmBlBnBpUBtBuBv+BxBy$Bz*B{0B}<B~BB.B�4B�OB��B�{B��B��B�rB��B�xB��B��B��B�B��B�
B�B�B��B��B��B� B�ZB�zB��B�wB�vB�hB��B��B��B��B��B��B��BªB��B��B��B��B��B�B�1B�B� B� B�B�B�2B�B�2B�SB�$B�EB�_B�kB�]BބB�pB�|B�|B�B��B�FB�/B�B�2B�jB	MB	fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	$B	#�B	#�B	$B	&B	&B	'B	'RB	+6B	-)B	.IB	0oB	2GB	4TB	5ZB	5�B	7�B	<�B	=�B	>�B	@�B	C�B	C�B	E�B	IB	K�B	LB	M6B	RB	SB	SB	S@B	UgB	ZQB	\CB	\]B	^�B	`�B	b�B	e�B	g�B	i�B	n�B	o�B	o�B	p�B	q�B	s�B	t�B	v�B	v�B	x�B	zB	{B	|B	|B	}B	~B	~BB	}B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�
B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.06(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905240032402019052400324020190524003240202211182138572022111821385720221118213857201905250018302019052500183020190525001830  JA  ARFMdecpA19c                                                                20190513213650  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190513123746  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190513123748  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190513123749  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190513123750  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190513123750  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190513123750  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190513123750  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190513123751                      G�O�G�O�G�O�                JA  ARUP                                                                        20190513125805                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190513153123  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190523153240  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190523153240  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190524151830  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231518                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123857  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                