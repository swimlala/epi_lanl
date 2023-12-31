CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-09-15T05:23:30Z creation; 2023-02-10T23:09:45Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   U@   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     8  [P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   s�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     8  y�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     8  �`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     8 #(   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ;`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ;�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   A�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   G�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T M�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   N   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   N   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   N$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   N,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � N4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   N�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   N�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    N�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        N�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        O    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       O   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    OArgo profile    3.1 1.2 19500101000000  20220915052330  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_228                 6810_008521_228                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���qu�"@���qu�"11  @��ɥ��@��ɥ��@1�"h	ԕ@1�"h	ԕ�d��D��d��D�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@:�H@�  @��R@��R@޸R@��RA  A   A,(�A@��A`��A�Q�A��A�Q�A�  A��A�Q�A�  A�  B   B  B  B�
B�
B(  B/�
B8  B@  BH  BO�
BW�
B`(�Bh  Bo�
Bw�
B�
B�  B�(�B�{B�{B�  B�  B�{B�  B��B�  B�  B�{B�  B��B��
B��
B��B�  B�{B�  B�  B��B��B�  B�{B�(�B�(�B�(�B��B�  B�=qC   C�C��C��C  C
  C
=C  C  C
=C��C�C�C�C��C
=C   C"  C$  C&  C(  C)��C+�C-�C/��C1��C3��C6  C8  C:  C;��C>  C@  CA��CD
=CF  CG��CI��CL
=CN
=CO��CQ��CT  CV  CW��CY��C[��C^
=C`
=Ca��Cd
=Cf  Ch  Ci��Cl  Cn  Co�Cr
=Ct  Cv  Cx  Cz
=C|  C}��C��C���C���C�C�
=C���C���C���C���C���C���C�  C�
=C�C�  C�  C�C�C�  C�C�C���C�  C���C���C�  C�C�C�C�  C�C�C�C�  C���C�C�C�  C�  C�C�  C���C���C�  C�  C�  C�C���C���C�C�  C���C���C�  C���C���C�  C�  C���C�  C�  C���C���C���C���C�C�  C�  C�  C�  C�  C�C�  C�  C���C���C�  C�C�
=C�C�C���C�  C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�C�C�  C�  C�  C�  C�C���C�  C�  C���C���C���C���C���C���C�  C���C�  C�C�  C���C�  C�C�C�  C�  C�C���C���C�  C�C���C�  C�C���D � D�D� D�D� D  D}qD�qD}qD�qD� D  D� D  D}qD  D� D	  D	z�D	�qD
��DD�DD}qD  D��D  D� D  D� DD�DD��D�qD}qD�qD� D  D� D  D� D�qD�D  D}qD�D� DD��D�D� D�qD}qD  D��D  Dz�D  D��D�D��D �D � D!  D!��D"  D"}qD"�qD#� D$�D$��D%�D%��D&  D&��D'  D'� D(�D(� D(�qD)}qD*  D*� D+  D+�D,�D,� D,�qD-z�D-�qD.� D.��D/}qD0  D0��D1D1}qD2  D2�D3�D3� D3�qD4z�D5  D5��D5�qD6}qD7  D7��D8  D8}qD8�qD9� D:  D:� D:�qD;� D<  D<}qD<�qD=� D>�D>}qD?  D?}qD@  D@��DA  DA}qDB  DB� DC  DC}qDC��DD}qDD�qDE}qDE�qDFz�DF��DG� DH�DH�DI�DI� DI�qDJ� DK  DK� DK�qDL� DMDM��DM�qDN}qDO�DO��DO��DPxRDP��DQ}qDQ��DR}qDR�qDS� DT  DTz�DT��DU��DVDV� DV��DW}qDX�DX� DY  DY� DZDZ��DZ�qD[}qD\  D\��D]�D]� D]�qD^� D_D_� D_�qD`� D`�qDa��Db�Db}qDc  Dc� Dd  Dd� De  De��De�qDf� Dg  Dg� Dh  Dh� Dh�qDi��DjDj� Dj�qDk��Dl  Dl� Dm  Dm� Dn  Dn� Do  Do��Dp  Dp��Dq  Dq}qDq��Dr}qDs�Ds��Dt�Dt��Du�Du}qDv  Dv� Dw  Dw��Dx  Dx� Dx�qDy}qDy�qDz}qD{�D{� D{�qD|}qD|��D}z�D~�D~��D~��Dz�D��D�@ D��HD�� D�HD�B�D��HD���D��qD�>�D���D��HD�  D�>�D�}qD�� D�HD�AHD�� D���D���D�>�D�}qD�� D��D�B�D��HD��HD�  D�>�D�}qD���D���D�=qD�� D�� D�  D�=qD�~�D�� D���D�AHD���D��HD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D��HD���D�  D�@ D�� D��HD�  D�B�D�� D���D���D�@ D��HD�� D�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D���D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D��HD�HD�@ D��fD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?#�
?u?���?\?�G�@   @
=@!G�@5@E�@Tz�@h��@s33@��@���@�z�@��H@��@���@�
=@�  @˅@У�@޸R@��
@��@��HAG�A�A
=qAG�A�A��A   A"�\A(Q�A-p�A1G�A7�A;�A@��AEAH��AO\)AS�
AXQ�A]p�A`��Ag
=Aj�HAp  Atz�Ay��A~{A���A�(�A�A���A��\A�p�A��A���A���A�ffA��A��
A�{A�G�A��HA�{A��A��\A���A�\)A�=qA��
A��RA���A��A�ffA�  A�33A�p�A�\)Aʏ\A�(�A�
=A��AӅAָRA�Q�AۅA�A߮A�\A�z�A�
=A�=qA��
A�RA�A�33A�ffA�Q�A��HA�{A��Bp�B�\B�B�B�B�Bz�B	B33BQ�BG�B
=B�
B��B�\B�B�B=qB33B��BB\)BQ�B��B
=B   B!��B"�RB#�B%G�B&=qB'�B(��B)�B+�B,z�B-B/33B0  B1��B2�\B3�B5G�B6=qB733B8��B9��B;33B<z�B=p�B>ffB@(�B@��BB�RBC�BD��BF=qBG\)BHz�BJ{BK33BLQ�BMBN�HBPQ�BQBR�\BT  BUp�BVffBX  BY�BZ=qB[�
B\��B^{B_�
B`��Ba�Bc\)BdQ�Bf{Bf�HBhQ�BiBj�\Bl(�Bm��BnffBo�
Bq�Br{Bs�
Bt��Bv{Bw�Bx��By�B{�B|��B}�B�B�ffB��HB�B�=qB���B���B�{B���B�p�B�  B���B�p�B��
B���B�\)B��
B�z�B�33B�B�Q�B��B�B�=qB�
=B���B�Q�B�
=B��B�=qB�
=B�p�B�=qB���B�p�B�{B���B�\)B��
B���B�G�B��B�z�B��B��B�=qB���B�\)B��B��RB�33B�B��\B�
=B��B�ffB���B��B�(�B���B�G�B��B�ffB���B�B�(�B��RB�\)B��
B�z�B��B��B�=qB���B�33B��B���B���B��B�Q�B��RB��B��B�z�B��B��
B�=qB��HB���B�{B��\B�G�B�  B�ffB���B�B�=qB���B��B�  B�z�B�\)B�B�Q�B���B�p�B�{B�z�B�33B��
B�Q�B���BÙ�B�(�B�z�B�33B��B�Q�B��HBǙ�B�=qBȣ�B�\)B�  B�ffB��HB˙�B�{B̏\B�G�BͮB�(�B��HB�p�B�B�ffB�
=B�p�B�  BҸRB��B�Bԏ\B��Bՙ�B�{BָRB�p�B�{B�z�B�
=B�B�z�B��HB�p�B�=qBܸRB��B��
Bޏ\B���B߅B�{B��HB�\)B��
B�\B�33B㙚B�(�B��HB�p�B��B�\B�G�B�B�Q�B���B�\)B��B��B�G�B뙚B�=qB���B�B��B�RB�G�B�B�ffB��B�B�{B��HB�B�  B�\B�\)B�{B��\B��B�B��\B�
=B��B�ffB��B���B�{B��HB���B�  B���B�p�B��C =qC ��C  C=qC�\C��CG�C�C�
C=qC��C�
C�C�C�C33Cz�C�HC=qC�\C�
C33C��C�HC33C��C��C	=qC	�\C	��C
G�C
�\C  C\)C��C�HCG�C�C�C33C��C��C33C�C�C(�Cz�C�HC33Cp�C��C33C�C�RC{Cz�C�C
=Cp�C�C  CffCC
=CQ�C�RC  CG�C�RC  CQ�C�RC��CG�C�C
=C=qC��C��C33Cz�C�
C�C\)C��C  C33C\)C�RC��C�C\)C��C�RC
=C(�CQ�C��CC�
C{C=qCQ�C�\C�RC�HC �C G�C \)C ��C ��C �HC!(�C!Q�C!p�C!�C!�
C!�C"33C"ffC"z�C"�RC"�C#  C#G�C#p�C#�\C#��C$
=C$�C$ffC$�\C$��C$�C%{C%33C%\)C%��C%�C%�C&(�C&=qC&\)C&�C&��C&�C'  C'G�C'\)C'�C'C'�HC(
=C(=qC(Q�C(�C(�RC(�HC)  C)�C)Q�C)�\C)C)�HC*  C*�C*ffC*�C*��C*�
C+  C+�C+=qC+z�C+��C+�RC+��C,�C,Q�C,p�C,�\C,��C-  C-(�C-Q�C-p�C-�\C-��C.  C.33C.G�C.ffC.��C.�
C/
=C/33C/G�C/p�C/��C/��C/�C0�C0Q�C0p�C0�\C0�RC0�HC1�C1Q�C1p�C1�C1�C1�
C2  C233C2Q�C2p�C2�\C2C2��C3�C3=qC3\)C3�C3�C3�
C4
=C4G�C4p�C4�\C4�C4�HC5
=C5=qC5\)C5z�C5��C5��C6  C633C6G�C6ffC6�C6�RC6�C7{C7(�C7=qC7ffC7��C7��C7�C8  C8(�C8\)C8�\C8�C8C8�HC9
=C9G�C9ffC9�\C9��C9��C9�C:�C:Q�C:p�C:�\C:�C:�HC;{C;=qC;\)C;p�C;��C;��C<
=C<�C<=qC<\)C<��C<��C<��C={C=(�C=\)C=�C=�RC=�
C=��C>{C>G�C>z�C>�C>�
C>��C?{C?(�C?\)C?�\C?C?��C@{C@=qC@\)C@�C@�RC@�CA(�CAQ�CAz�CA��CACA��CB(�CB\)CB�\CBCB��CC�CCG�CCffCC��CC��CD
=CD=qCDffCD�\CD�RCD�
CD��CE(�CE\)CE�\CECE�HCF
=CF=qCF\)CF�CF�RCF�CG�CGQ�CG�CG��CGCG�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?k�?��H@:�H@�  @��R@��R@޸R@��RA  A   A,(�A@��A`��A�Q�A��A�Q�A�  A��A�Q�A�  A�  B   B  B  B�
B�
B(  B/�
B8  B@  BH  BO�
BW�
B`(�Bh  Bo�
Bw�
B�
B�  B�(�B�{B�{B�  B�  B�{B�  B��B�  B�  B�{B�  B��B��
B��
B��B�  B�{B�  B�  B��B��B�  B�{B�(�B�(�B�(�B��B�  B�=qC   C�C��C��C  C
  C
=C  C  C
=C��C�C�C�C��C
=C   C"  C$  C&  C(  C)��C+�C-�C/��C1��C3��C6  C8  C:  C;��C>  C@  CA��CD
=CF  CG��CI��CL
=CN
=CO��CQ��CT  CV  CW��CY��C[��C^
=C`
=Ca��Cd
=Cf  Ch  Ci��Cl  Cn  Co�Cr
=Ct  Cv  Cx  Cz
=C|  C}��C��C���C���C�C�
=C���C���C���C���C���C���C�  C�
=C�C�  C�  C�C�C�  C�C�C���C�  C���C���C�  C�C�C�C�  C�C�C�C�  C���C�C�C�  C�  C�C�  C���C���C�  C�  C�  C�C���C���C�C�  C���C���C�  C���C���C�  C�  C���C�  C�  C���C���C���C���C�C�  C�  C�  C�  C�  C�C�  C�  C���C���C�  C�C�
=C�C�C���C�  C�  C�  C�C�C�  C�  C�
=C�C�  C�  C�C�C�  C�  C�  C�  C�C���C�  C�  C���C���C���C���C���C���C�  C���C�  C�C�  C���C�  C�C�C�  C�  C�C���C���C�  C�C���C�  C�C���D � D�D� D�D� D  D}qD�qD}qD�qD� D  D� D  D}qD  D� D	  D	z�D	�qD
��DD�DD}qD  D��D  D� D  D� DD�DD��D�qD}qD�qD� D  D� D  D� D�qD�D  D}qD�D� DD��D�D� D�qD}qD  D��D  Dz�D  D��D�D��D �D � D!  D!��D"  D"}qD"�qD#� D$�D$��D%�D%��D&  D&��D'  D'� D(�D(� D(�qD)}qD*  D*� D+  D+�D,�D,� D,�qD-z�D-�qD.� D.��D/}qD0  D0��D1D1}qD2  D2�D3�D3� D3�qD4z�D5  D5��D5�qD6}qD7  D7��D8  D8}qD8�qD9� D:  D:� D:�qD;� D<  D<}qD<�qD=� D>�D>}qD?  D?}qD@  D@��DA  DA}qDB  DB� DC  DC}qDC��DD}qDD�qDE}qDE�qDFz�DF��DG� DH�DH�DI�DI� DI�qDJ� DK  DK� DK�qDL� DMDM��DM�qDN}qDO�DO��DO��DPxRDP��DQ}qDQ��DR}qDR�qDS� DT  DTz�DT��DU��DVDV� DV��DW}qDX�DX� DY  DY� DZDZ��DZ�qD[}qD\  D\��D]�D]� D]�qD^� D_D_� D_�qD`� D`�qDa��Db�Db}qDc  Dc� Dd  Dd� De  De��De�qDf� Dg  Dg� Dh  Dh� Dh�qDi��DjDj� Dj�qDk��Dl  Dl� Dm  Dm� Dn  Dn� Do  Do��Dp  Dp��Dq  Dq}qDq��Dr}qDs�Ds��Dt�Dt��Du�Du}qDv  Dv� Dw  Dw��Dx  Dx� Dx�qDy}qDy�qDz}qD{�D{� D{�qD|}qD|��D}z�D~�D~��D~��Dz�D��D�@ D��HD�� D�HD�B�D��HD���D��qD�>�D���D��HD�  D�>�D�}qD�� D�HD�AHD�� D���D���D�>�D�}qD�� D��D�B�D��HD��HD�  D�>�D�}qD���D���D�=qD�� D�� D�  D�=qD�~�D�� D���D�AHD���D��HD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D��HD���D�  D�@ D�� D��HD�  D�B�D�� D���D���D�@ D��HD�� D�  D�@ D�~�D�� D���D�@ D��HD��HD�  D�>�D�~�D�� D�HD�AHD�� D���D�HD�@ D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D��HD�HD�@ D��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?#�
?u?���?\?�G�@   @
=@!G�@5@E�@Tz�@h��@s33@��@���@�z�@��H@��@���@�
=@�  @˅@У�@޸R@��
@��@��HAG�A�A
=qAG�A�A��A   A"�\A(Q�A-p�A1G�A7�A;�A@��AEAH��AO\)AS�
AXQ�A]p�A`��Ag
=Aj�HAp  Atz�Ay��A~{A���A�(�A�A���A��\A�p�A��A���A���A�ffA��A��
A�{A�G�A��HA�{A��A��\A���A�\)A�=qA��
A��RA���A��A�ffA�  A�33A�p�A�\)Aʏ\A�(�A�
=A��AӅAָRA�Q�AۅA�A߮A�\A�z�A�
=A�=qA��
A�RA�A�33A�ffA�Q�A��HA�{A��Bp�B�\B�B�B�B�Bz�B	B33BQ�BG�B
=B�
B��B�\B�B�B=qB33B��BB\)BQ�B��B
=B   B!��B"�RB#�B%G�B&=qB'�B(��B)�B+�B,z�B-B/33B0  B1��B2�\B3�B5G�B6=qB733B8��B9��B;33B<z�B=p�B>ffB@(�B@��BB�RBC�BD��BF=qBG\)BHz�BJ{BK33BLQ�BMBN�HBPQ�BQBR�\BT  BUp�BVffBX  BY�BZ=qB[�
B\��B^{B_�
B`��Ba�Bc\)BdQ�Bf{Bf�HBhQ�BiBj�\Bl(�Bm��BnffBo�
Bq�Br{Bs�
Bt��Bv{Bw�Bx��By�B{�B|��B}�B�B�ffB��HB�B�=qB���B���B�{B���B�p�B�  B���B�p�B��
B���B�\)B��
B�z�B�33B�B�Q�B��B�B�=qB�
=B���B�Q�B�
=B��B�=qB�
=B�p�B�=qB���B�p�B�{B���B�\)B��
B���B�G�B��B�z�B��B��B�=qB���B�\)B��B��RB�33B�B��\B�
=B��B�ffB���B��B�(�B���B�G�B��B�ffB���B�B�(�B��RB�\)B��
B�z�B��B��B�=qB���B�33B��B���B���B��B�Q�B��RB��B��B�z�B��B��
B�=qB��HB���B�{B��\B�G�B�  B�ffB���B�B�=qB���B��B�  B�z�B�\)B�B�Q�B���B�p�B�{B�z�B�33B��
B�Q�B���BÙ�B�(�B�z�B�33B��B�Q�B��HBǙ�B�=qBȣ�B�\)B�  B�ffB��HB˙�B�{B̏\B�G�BͮB�(�B��HB�p�B�B�ffB�
=B�p�B�  BҸRB��B�Bԏ\B��Bՙ�B�{BָRB�p�B�{B�z�B�
=B�B�z�B��HB�p�B�=qBܸRB��B��
Bޏ\B���B߅B�{B��HB�\)B��
B�\B�33B㙚B�(�B��HB�p�B��B�\B�G�B�B�Q�B���B�\)B��B��B�G�B뙚B�=qB���B�B��B�RB�G�B�B�ffB��B�B�{B��HB�B�  B�\B�\)B�{B��\B��B�B��\B�
=B��B�ffB��B���B�{B��HB���B�  B���B�p�B��C =qC ��C  C=qC�\C��CG�C�C�
C=qC��C�
C�C�C�C33Cz�C�HC=qC�\C�
C33C��C�HC33C��C��C	=qC	�\C	��C
G�C
�\C  C\)C��C�HCG�C�C�C33C��C��C33C�C�C(�Cz�C�HC33Cp�C��C33C�C�RC{Cz�C�C
=Cp�C�C  CffCC
=CQ�C�RC  CG�C�RC  CQ�C�RC��CG�C�C
=C=qC��C��C33Cz�C�
C�C\)C��C  C33C\)C�RC��C�C\)C��C�RC
=C(�CQ�C��CC�
C{C=qCQ�C�\C�RC�HC �C G�C \)C ��C ��C �HC!(�C!Q�C!p�C!�C!�
C!�C"33C"ffC"z�C"�RC"�C#  C#G�C#p�C#�\C#��C$
=C$�C$ffC$�\C$��C$�C%{C%33C%\)C%��C%�C%�C&(�C&=qC&\)C&�C&��C&�C'  C'G�C'\)C'�C'C'�HC(
=C(=qC(Q�C(�C(�RC(�HC)  C)�C)Q�C)�\C)C)�HC*  C*�C*ffC*�C*��C*�
C+  C+�C+=qC+z�C+��C+�RC+��C,�C,Q�C,p�C,�\C,��C-  C-(�C-Q�C-p�C-�\C-��C.  C.33C.G�C.ffC.��C.�
C/
=C/33C/G�C/p�C/��C/��C/�C0�C0Q�C0p�C0�\C0�RC0�HC1�C1Q�C1p�C1�C1�C1�
C2  C233C2Q�C2p�C2�\C2C2��C3�C3=qC3\)C3�C3�C3�
C4
=C4G�C4p�C4�\C4�C4�HC5
=C5=qC5\)C5z�C5��C5��C6  C633C6G�C6ffC6�C6�RC6�C7{C7(�C7=qC7ffC7��C7��C7�C8  C8(�C8\)C8�\C8�C8C8�HC9
=C9G�C9ffC9�\C9��C9��C9�C:�C:Q�C:p�C:�\C:�C:�HC;{C;=qC;\)C;p�C;��C;��C<
=C<�C<=qC<\)C<��C<��C<��C={C=(�C=\)C=�C=�RC=�
C=��C>{C>G�C>z�C>�C>�
C>��C?{C?(�C?\)C?�\C?C?��C@{C@=qC@\)C@�C@�RC@�CA(�CAQ�CAz�CA��CACA��CB(�CB\)CB�\CBCB��CC�CCG�CCffCC��CC��CD
=CD=qCDffCD�\CD�RCD�
CD��CE(�CE\)CE�\CECE�HCF
=CF=qCF\)CF�CF�RCF�CG�CGQ�CG�CG��CGCG�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�l�A�v�A�p�A�r�A�A�A���A�=qA�5?A�A޶FA޴9AެAޗ�Aޏ\AޅA�r�A�hsA�jA�hsA�I�A�$�A�  A���AݾwAݩ�A�t�A�-A��A��#A�Aܗ�A�dZA�A�A��A��Aۺ^Aۧ�Aۛ�A�n�A�bNA��A�bA���A�A���A�~�A�ȴA���A��A�hsA�l�A�1'A��A��A�ȴA�A���A�\)A�  A��A̗�A̓uA�G�A���A�S�Aɉ7A���A�=qA��HA��A�bA��A�l�A���A���A��uA�/A�{A�VA���A�v�A�O�A��PA��^A�z�A�`BA�{A���A�  A���A��#A�$�A�A��!A��A���A�+A�`BA��7A�9XA�M�A�O�A�=qA��A���A��jA��A�O�A��-A���A�ZA�|�A���A��hA�"�A��uA���A��RA��DA�bA�K�A��yA�E�A��A��yA�E�A��`A~Q�A|�RAzQ�AxE�Aw`BAvQ�At$�Aq%Am��Al9XAjn�Ag?}AehsAd1'A^�HA]�FA]+A[�mAZ{AXJAU\)AT1'ARjAP�/AN{AKK�AJ^5AI"�AG;dAF�HAE%A?��A=��A<�RA;33A8�/A7A7�A6��A5�;A5C�A4ZA3`BA2{A0��A/�A.��A-&�A+oA*E�A)��A(��A(�DA'�7A&{A%?}A%�A$��A$��A$bNA#��A"��A!G�A��AC�A��AbA�A��A`BAhsA��A��A�;Al�AdZAA�!A��A�DA�A"�AffA�
AoA�jA�+A$�A��A��A\)A
Q�A	;dA��A�A��A�A �AAjA��A �`A M�@�t�@�n�@�K�@��^@�bN@�S�@��w@�@�t�@�@�X@���@�{@��@��;@�V@�&�@���@���@�E�@�-@���@�V@�I�@睲@��H@��T@�%@��@�@�ȴ@�V@��T@���@ᙚ@�9@޸R@��@��#@��@�b@۝�@ۅ@��H@�M�@ٺ^@���@�bN@�b@�ƨ@�dZ@և+@պ^@�&�@�bN@�(�@��;@�\)@���@҇+@���@�V@мj@�t�@��@Ώ\@���@̛�@��;@�\)@�"�@���@��@���@��@�"�@ƸR@�C�@�S�@�dZ@�t�@���@Ƈ+@�~�@�V@�?}@ļj@�Z@�t�@�@��T@��y@�V@�v�@��@��@��+@��@�hs@�1@�dZ@�S�@���@���@�+@��R@�^5@�G�@�Z@��@�
=@��@�@��h@�&�@�r�@�C�@��!@���@���@��u@�9X@��
@���@�dZ@�
=@��H@��y@�+@�K�@���@�=q@�$�@��h@�O�@�&�@�%@��9@�Q�@�  @��w@���@��F@���@���@�S�@�
=@�J@��h@���@���@�x�@��@�G�@��@��9@�9X@���@��@�"�@�ff@�ff@�V@�V@�^5@��T@�p�@�/@���@�bN@�9X@��@�b@���@��@��;@�ƨ@��@�t�@�o@���@�n�@�M�@�@�X@��@��D@�r�@�(�@��m@���@��@�@���@���@��+@��+@�^5@�J@���@�O�@�%@��j@��j@��j@���@�1'@�b@���@��F@�t�@�\)@�C�@�+@��!@�$�@�@�@�?}@��/@��D@�1'@��@��@�ƨ@��F@�\)@��@���@��+@�~�@�ff@�5?@��#@��^@��-@���@���@��7@�p�@��@��9@�r�@�(�@�1@��m@��m@��F@�\)@��y@���@�E�@�{@���@�p�@�G�@��@��@��/@���@�Ĝ@��j@���@�z�@�I�@��@��
@���@�C�@�
=@��@��H@���@���@��!@�E�@���@�x�@�`B@�O�@�/@�&�@�V@���@��/@��u@�9X@��@���@�l�@�;d@�33@�33@�+@��@���@�n�@�5?@�{@��^@��@�/@�%@�Ĝ@��u@�j@�9X@��m@��@��P@�l�@�K�@�C�@�o@���@��R@���@��\@��+@�ff@��@���@���@�x�@�?}@�V@���@���@�Ĝ@�A�@��@��
@�t�@��H@�ff@�{@��T@��h@�G�@��@��@��@�j@�9X@�b@|�@+@~v�@}�-@}/@|�D@{�m@y�^@y&�@x�9@xA�@w�w@w;d@v�@v��@u�@up�@uV@t�@t(�@s�
@sC�@r�!@rM�@q��@qhs@p��@pQ�@o�;@o+@n��@n5?@n$�@nV@nff@nV@nE�@n5?@n@m��@m`B@l�@l�D@lj@l(�@l1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�jA�t�A�t�A�r�A�t�A�z�A�r�A�n�A�r�A�t�A�t�A�r�A�t�A�l�A�l�A�7LA�5?A�/A�;dA���Aߥ�A�M�A�K�A�(�A�1'A�9XA�7LA�5?A޾wA���A޺^A޸RA޺^A޲-A޶FA޴9Aް!A޴9AޮAޮAެAޥ�Aާ�Aޡ�Aޛ�Aޝ�AޑhAޕ�Aޏ\AޑhAޑhAޑhAޑhAޏ\AޓuAލPAލPAޅAމ7AޅAރAޅA�z�A�|�A�t�A�r�A�t�A�jA�p�A�hsA�jA�hsA�hsA�jA�dZA�jA�jA�jA�l�A�hsA�l�A�n�A�jA�n�A�jA�n�A�n�A�dZA�^5A�Q�A�Q�A�Q�A�M�A�G�A�9XA�;dA�9XA�/A�$�A�$�A��A�{A�VA�VA�VA�A���A��A��`A��HA��A��A���A���A�ƨA�ĜA�A�ƨAݼjAݶFAݺ^Aݴ9Aݴ9Aݰ!Aݩ�Aݩ�Aݝ�Aݝ�Aݕ�A݇+A�~�A�r�A�ffA�ZA�G�A�=qA�/A�(�A�(�A�"�A�&�A�"�A�"�A� �A�bA�{A�JA���A���A��A��#A���A�ĜA�ƨA�ƨA�A�A�ƨAܾwA�Aܺ^Aܴ9AܮAܗ�A܋DA܉7A܃A�x�A�x�A�jA�bNA�\)A�M�A�M�A�I�A�A�A�C�A�;dA�7LA�9XA�/A�$�A�"�A��A�{A�%A���A���A��A��A��mA��#A���A���Aۺ^Aۺ^A۶FA۬AۮA۬Aۥ�Aۧ�Aۥ�Aۡ�Aۥ�Aۡ�Aۙ�A۝�Aۙ�Aە�Aۗ�AۅA�t�A�jA�dZA�jA�ffA�dZA�jA�l�A�jA�n�A�\)A�/A� �A�$�A��A��A��A��A��A�&�A�&�A�oA���A��A���A���A��A���A�A�A�%A�oA�{A�oA�%A��A��TA��TA��TA��#A��A�ȴA�ĜAڼjAڧ�AځA�t�A�x�A�n�A�hsA�bNA�"�A���A٧�A�~�A�jA�Q�A� �A�
=A���A���A��A��A���AؾwA؝�A�+A��mA�ĜAװ!A׏\A�t�A�1'A�dZA���A��
A���AլAՙ�A�t�AԍPA��mAӝ�A�S�A�Q�A�=qA�?}A�1'A��A�"�A��A�
=A�  A���A��A��mA���A���A���AҰ!AҋDA��mAыDA�v�A�n�A�G�A��A�1A��A�n�A�A�A�5?A�(�A�{A�JA���A��A��A��mA��;A��/A��#A���A���A���AϬAϝ�Aϙ�AϏ\A�t�A�ZA�7LA�  A·+A�XA�$�A���A��
AͼjAͬA̓A�dZA�-A�ƨA̩�A̛�A̛�A̝�A̕�A̕�A̛�A̗�A̕�A̗�A̙�A̕�Ȁ\Ȁ\A̕�A̓uA̋DA�|�A�jA�O�A�33A�+A�$�A�oA�1A�A���A��A��A��yA��TA��HA�ȴA˺^A�p�A�
=A�ĜA�A�A�A���Aɣ�A�r�A�I�A�%A���A�t�A��A�bNA�O�A�;dA�Aƛ�A�I�A�1'A�1A��A��`A��mA��`A��HA��/A��HA��HA��A���AŴ9A�33A��A��Aò-A�O�A�{A�1A��`A���A´9A¡�A�n�A��/A���A�5?A��A��A��A�bNA�(�A�"�A��A���A��
A���A��9A��!A���A���A��uA���A���A��hA���A���A��uA��uA���A���A��PA�p�A�G�A�5?A��A�1A��HA��9A�t�A� �A��
A��A���A��uA�z�A�S�A�M�A�E�A�/A�{A�1A���A��7A�`BA�VA�1'A�JA��^A�=qA�JA��HA��A�~�A�t�A�ffA�A�A��/A��7A�A�A�VA���A�1'A��uA�hsA��A��A���A���A���A��hA��hA��PA��A��A��A�|�A�~�A�x�A�v�A�x�A�t�A�p�A�t�A�r�A�l�A�n�A�hsA�dZA�ffA�`BA�ZA�XA�I�A��A���A�M�A�33A� �A�oA�%A�A���A��mA��A��jA���A�v�A�XA�-A���A��!A�t�A�ZA�I�A�5?A�/A�"�A��A��A��A�{A�{A�JA�  A��A��#A���A�ƨA��wA���A��^A��jA��wA��^A��^A��^A��!A��uA�z�A�p�A�hsA�G�A�(�A��A���A��A�XA�1'A��A��-A��-A��RA��^A��^A��!A���A��A�ZA�5?A�JA��mA���A�ffA�K�A�C�A�+A�&�A� �A�
=A��A���A�  A���A��yA��/A���A���A�ȴA���A��jA��jA��RA��!A��A���A���A���A���A���A���A���A���A��hA��\A��PA��+A��A�z�A�r�A�n�A�hsA�1'A�+A�(�A��A��7A��DA�|�A�~�A�z�A�x�A��A�XA�M�A�S�A�C�A�?}A�?}A�?}A�;dA�?}A�9XA�/A�(�A��A�  A��/A���A�ȴA�A��^A��FA��\A�l�A�G�A�=qA�5?A�&�A��A�  A�ƨA��A�1'A���A��A���A���A��\A��DA��+A�p�A�M�A��A��A�z�A�bA��A�jA�M�A�I�A�9XA� �A�  A��A�ƨA�|�A�9XA�{A��A���A���A��hA�v�A�G�A�"�A�
=A��A�ȴA��A��hA�l�A�/A�bA��yA�A�p�A�G�A� �A��
A��+A�+A���A���A��-A��hA�~�A�^5A�ZA�K�A��A��A��\A��
A�dZA�-A�{A�VA�JA�VA�VA�VA�
=A�
=A�%A�A�  A���A��A��/A��wA���A��\A�|�A�`BA�1'A� �A�oA�A���A��HA�ĜA��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�l�A�v�A�p�A�r�A�A�A���A�=qA�5?A�A޶FA޴9AެAޗ�Aޏ\AޅA�r�A�hsA�jA�hsA�I�A�$�A�  A���AݾwAݩ�A�t�A�-A��A��#A�Aܗ�A�dZA�A�A��A��Aۺ^Aۧ�Aۛ�A�n�A�bNA��A�bA���A�A���A�~�A�ȴA���A��A�hsA�l�A�1'A��A��A�ȴA�A���A�\)A�  A��A̗�A̓uA�G�A���A�S�Aɉ7A���A�=qA��HA��A�bA��A�l�A���A���A��uA�/A�{A�VA���A�v�A�O�A��PA��^A�z�A�`BA�{A���A�  A���A��#A�$�A�A��!A��A���A�+A�`BA��7A�9XA�M�A�O�A�=qA��A���A��jA��A�O�A��-A���A�ZA�|�A���A��hA�"�A��uA���A��RA��DA�bA�K�A��yA�E�A��A��yA�E�A��`A~Q�A|�RAzQ�AxE�Aw`BAvQ�At$�Aq%Am��Al9XAjn�Ag?}AehsAd1'A^�HA]�FA]+A[�mAZ{AXJAU\)AT1'ARjAP�/AN{AKK�AJ^5AI"�AG;dAF�HAE%A?��A=��A<�RA;33A8�/A7A7�A6��A5�;A5C�A4ZA3`BA2{A0��A/�A.��A-&�A+oA*E�A)��A(��A(�DA'�7A&{A%?}A%�A$��A$��A$bNA#��A"��A!G�A��AC�A��AbA�A��A`BAhsA��A��A�;Al�AdZAA�!A��A�DA�A"�AffA�
AoA�jA�+A$�A��A��A\)A
Q�A	;dA��A�A��A�A �AAjA��A �`A M�@�t�@�n�@�K�@��^@�bN@�S�@��w@�@�t�@�@�X@���@�{@��@��;@�V@�&�@���@���@�E�@�-@���@�V@�I�@睲@��H@��T@�%@��@�@�ȴ@�V@��T@���@ᙚ@�9@޸R@��@��#@��@�b@۝�@ۅ@��H@�M�@ٺ^@���@�bN@�b@�ƨ@�dZ@և+@պ^@�&�@�bN@�(�@��;@�\)@���@҇+@���@�V@мj@�t�@��@Ώ\@���@̛�@��;@�\)@�"�@���@��@���@��@�"�@ƸR@�C�@�S�@�dZ@�t�@���@Ƈ+@�~�@�V@�?}@ļj@�Z@�t�@�@��T@��y@�V@�v�@��@��@��+@��@�hs@�1@�dZ@�S�@���@���@�+@��R@�^5@�G�@�Z@��@�
=@��@�@��h@�&�@�r�@�C�@��!@���@���@��u@�9X@��
@���@�dZ@�
=@��H@��y@�+@�K�@���@�=q@�$�@��h@�O�@�&�@�%@��9@�Q�@�  @��w@���@��F@���@���@�S�@�
=@�J@��h@���@���@�x�@��@�G�@��@��9@�9X@���@��@�"�@�ff@�ff@�V@�V@�^5@��T@�p�@�/@���@�bN@�9X@��@�b@���@��@��;@�ƨ@��@�t�@�o@���@�n�@�M�@�@�X@��@��D@�r�@�(�@��m@���@��@�@���@���@��+@��+@�^5@�J@���@�O�@�%@��j@��j@��j@���@�1'@�b@���@��F@�t�@�\)@�C�@�+@��!@�$�@�@�@�?}@��/@��D@�1'@��@��@�ƨ@��F@�\)@��@���@��+@�~�@�ff@�5?@��#@��^@��-@���@���@��7@�p�@��@��9@�r�@�(�@�1@��m@��m@��F@�\)@��y@���@�E�@�{@���@�p�@�G�@��@��@��/@���@�Ĝ@��j@���@�z�@�I�@��@��
@���@�C�@�
=@��@��H@���@���@��!@�E�@���@�x�@�`B@�O�@�/@�&�@�V@���@��/@��u@�9X@��@���@�l�@�;d@�33@�33@�+@��@���@�n�@�5?@�{@��^@��@�/@�%@�Ĝ@��u@�j@�9X@��m@��@��P@�l�@�K�@�C�@�o@���@��R@���@��\@��+@�ff@��@���@���@�x�@�?}@�V@���@���@�Ĝ@�A�@��@��
@�t�@��H@�ff@�{@��T@��h@�G�@��@��@��@�j@�9X@�b@|�@+@~v�@}�-@}/@|�D@{�m@y�^@y&�@x�9@xA�@w�w@w;d@v�@v��@u�@up�@uV@t�@t(�@s�
@sC�@r�!@rM�@q��@qhs@p��@pQ�@o�;@o+@n��@n5?@n$�@nV@nff@nV@nE�@n5?@n@m��@m`B@l�@l�D@lj@l(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�jA�t�A�t�A�r�A�t�A�z�A�r�A�n�A�r�A�t�A�t�A�r�A�t�A�l�A�l�A�7LA�5?A�/A�;dA���Aߥ�A�M�A�K�A�(�A�1'A�9XA�7LA�5?A޾wA���A޺^A޸RA޺^A޲-A޶FA޴9Aް!A޴9AޮAޮAެAޥ�Aާ�Aޡ�Aޛ�Aޝ�AޑhAޕ�Aޏ\AޑhAޑhAޑhAޑhAޏ\AޓuAލPAލPAޅAމ7AޅAރAޅA�z�A�|�A�t�A�r�A�t�A�jA�p�A�hsA�jA�hsA�hsA�jA�dZA�jA�jA�jA�l�A�hsA�l�A�n�A�jA�n�A�jA�n�A�n�A�dZA�^5A�Q�A�Q�A�Q�A�M�A�G�A�9XA�;dA�9XA�/A�$�A�$�A��A�{A�VA�VA�VA�A���A��A��`A��HA��A��A���A���A�ƨA�ĜA�A�ƨAݼjAݶFAݺ^Aݴ9Aݴ9Aݰ!Aݩ�Aݩ�Aݝ�Aݝ�Aݕ�A݇+A�~�A�r�A�ffA�ZA�G�A�=qA�/A�(�A�(�A�"�A�&�A�"�A�"�A� �A�bA�{A�JA���A���A��A��#A���A�ĜA�ƨA�ƨA�A�A�ƨAܾwA�Aܺ^Aܴ9AܮAܗ�A܋DA܉7A܃A�x�A�x�A�jA�bNA�\)A�M�A�M�A�I�A�A�A�C�A�;dA�7LA�9XA�/A�$�A�"�A��A�{A�%A���A���A��A��A��mA��#A���A���Aۺ^Aۺ^A۶FA۬AۮA۬Aۥ�Aۧ�Aۥ�Aۡ�Aۥ�Aۡ�Aۙ�A۝�Aۙ�Aە�Aۗ�AۅA�t�A�jA�dZA�jA�ffA�dZA�jA�l�A�jA�n�A�\)A�/A� �A�$�A��A��A��A��A��A�&�A�&�A�oA���A��A���A���A��A���A�A�A�%A�oA�{A�oA�%A��A��TA��TA��TA��#A��A�ȴA�ĜAڼjAڧ�AځA�t�A�x�A�n�A�hsA�bNA�"�A���A٧�A�~�A�jA�Q�A� �A�
=A���A���A��A��A���AؾwA؝�A�+A��mA�ĜAװ!A׏\A�t�A�1'A�dZA���A��
A���AլAՙ�A�t�AԍPA��mAӝ�A�S�A�Q�A�=qA�?}A�1'A��A�"�A��A�
=A�  A���A��A��mA���A���A���AҰ!AҋDA��mAыDA�v�A�n�A�G�A��A�1A��A�n�A�A�A�5?A�(�A�{A�JA���A��A��A��mA��;A��/A��#A���A���A���AϬAϝ�Aϙ�AϏ\A�t�A�ZA�7LA�  A·+A�XA�$�A���A��
AͼjAͬA̓A�dZA�-A�ƨA̩�A̛�A̛�A̝�A̕�A̕�A̛�A̗�A̕�A̗�A̙�A̕�Ȁ\Ȁ\A̕�A̓uA̋DA�|�A�jA�O�A�33A�+A�$�A�oA�1A�A���A��A��A��yA��TA��HA�ȴA˺^A�p�A�
=A�ĜA�A�A�A���Aɣ�A�r�A�I�A�%A���A�t�A��A�bNA�O�A�;dA�Aƛ�A�I�A�1'A�1A��A��`A��mA��`A��HA��/A��HA��HA��A���AŴ9A�33A��A��Aò-A�O�A�{A�1A��`A���A´9A¡�A�n�A��/A���A�5?A��A��A��A�bNA�(�A�"�A��A���A��
A���A��9A��!A���A���A��uA���A���A��hA���A���A��uA��uA���A���A��PA�p�A�G�A�5?A��A�1A��HA��9A�t�A� �A��
A��A���A��uA�z�A�S�A�M�A�E�A�/A�{A�1A���A��7A�`BA�VA�1'A�JA��^A�=qA�JA��HA��A�~�A�t�A�ffA�A�A��/A��7A�A�A�VA���A�1'A��uA�hsA��A��A���A���A���A��hA��hA��PA��A��A��A�|�A�~�A�x�A�v�A�x�A�t�A�p�A�t�A�r�A�l�A�n�A�hsA�dZA�ffA�`BA�ZA�XA�I�A��A���A�M�A�33A� �A�oA�%A�A���A��mA��A��jA���A�v�A�XA�-A���A��!A�t�A�ZA�I�A�5?A�/A�"�A��A��A��A�{A�{A�JA�  A��A��#A���A�ƨA��wA���A��^A��jA��wA��^A��^A��^A��!A��uA�z�A�p�A�hsA�G�A�(�A��A���A��A�XA�1'A��A��-A��-A��RA��^A��^A��!A���A��A�ZA�5?A�JA��mA���A�ffA�K�A�C�A�+A�&�A� �A�
=A��A���A�  A���A��yA��/A���A���A�ȴA���A��jA��jA��RA��!A��A���A���A���A���A���A���A���A���A��hA��\A��PA��+A��A�z�A�r�A�n�A�hsA�1'A�+A�(�A��A��7A��DA�|�A�~�A�z�A�x�A��A�XA�M�A�S�A�C�A�?}A�?}A�?}A�;dA�?}A�9XA�/A�(�A��A�  A��/A���A�ȴA�A��^A��FA��\A�l�A�G�A�=qA�5?A�&�A��A�  A�ƨA��A�1'A���A��A���A���A��\A��DA��+A�p�A�M�A��A��A�z�A�bA��A�jA�M�A�I�A�9XA� �A�  A��A�ƨA�|�A�9XA�{A��A���A���A��hA�v�A�G�A�"�A�
=A��A�ȴA��A��hA�l�A�/A�bA��yA�A�p�A�G�A� �A��
A��+A�+A���A���A��-A��hA�~�A�^5A�ZA�K�A��A��A��\A��
A�dZA�-A�{A�VA�JA�VA�VA�VA�
=A�
=A�%A�A�  A���A��A��/A��wA���A��\A�|�A�`BA�1'A� �A�oA�A���A��HA�ĜA��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B�BBBBuB7B�B�B�BBxB�BB
�B
=B�B1BfB	lB1B1B	�B
�B�B�B�B>wBB�BF�BGBD�B?}B>wB@�B?HBB[BCaBD3BK�BS[Ba|Bg�Bg�Bo�BpoBr|Bv�By�B�DB�-B��B҉B�B�[B�2B�vB�B�sB�B�B B�BYBuBB�B)�B,�B$tB5�B?BF�BIRBJ#BIBG�BIRBQBO�BT�BX�BYB^5BT�BP}BN�BP�BK�BF�BA B<jB<�B>wB>B>wB>wB;0B5tB2-B�B�ZBܒB�B��B��B��B��B}�BrGBZ�BA�BGEB8B'BB"B
��B
�B
�^B
�aB
�LB
�B
�~B
��B
�+B
{�B
m]B
I�B
;dB
+�B
xB
�B
4B
�B	�B	�B	�B	��B	��B	�eB	��B	��B	r|B	pB	g8B	_B	U2B	I�B	C-B	@�B	:�B	2�B	#B	�B	B	@B	�B	B��B�B��B�B�2B�pB��B�#B�
B�,B�&BϫB��B͟BϫB�vB��B�gB��B�mBںBںB�dB�EB�B��B��B�B��B��B�+B��B�TB�cB�B��B�WB�B�HB�NB�B�sB�>B�PB	 �B	�B	�B��B��B	�B	{B	~B	B	B	=B	CB	xB	~B	"4B	"�B	#�B	&�B	$tB	(�B	($B	(�B	'�B	#B	!�B	%FB	%�B	'RB	+kB	,qB	C-B	@�B	A�B	C�B	H�B	L�B	C-B	H�B	L0B	P�B	UgB	U�B	T�B	PHB	L�B	J�B	PHB	PB	O�B	P�B	P�B	QB	Q�B	S�B	W�B	Y�B	YKB	^jB	]�B	]/B	\�B	\)B	[�B	_�B	d�B	iB	oiB	sMB	wfB	v�B	x�B	z�B	zDB	|B	z�B	zxB	z�B	zDB	zDB	y>B	y	B	~�B	��B	��B	��B	�7B	�B	�DB	��B	�\B	�\B	� B	�oB	�B	�+B	�7B	�=B	�=B	�CB	��B	��B	�=B	��B	�YB	��B	��B	�FB	��B	��B	��B	�CB	�B	�B	��B	��B	�IB	��B	�B	��B	��B	��B	�-B	�zB	�^B	�wB	��B	�UB	�<B	�dB	�0B	B	��B	�9B	ŢB	ĜB	�3B	�UB	��B	��B	��B	B	�gB	�B	�?B	��B	�B	ȴB	��B	ŢB	�mB	�B	�EB	ȴB	ɆB	�XB	��B	�jB	�}B	��B	ԕB	֡B	�B	��B	�]B	��B	�B	ߤB	�B	�NB	�B	��B	�8B	�B	�B	�B	�B	�B	�"B	�/B	�iB	��B	��B	�+B	�`B	�B	�ZB	��B	�2B	��B	�B	��B	�B	�"B	�(B	��B	��B
 �B
 �B
oB
�B
{B
�B
SB
�B
�B
�B
�B
_B
	B
	7B
	lB
	�B
xB

rB

=B

�B
B
�B
~B
�B
B
�B
"B
VB
�B
bB
�B
B
:B
�B
�B
�B
�B
�B
�B
�B
B
7B
	B
�B
	B
kB
�B
�B
xB
�B
 'B
!-B
!�B
"4B
!�B
"hB
"4B
"hB
#�B
$�B
$�B
$�B
$�B
$�B
%B
&�B
%�B
&B
&B
%�B
%�B
%�B
&�B
'B
'RB
'�B
'�B
&�B
&�B
($B
(�B
*0B
+B
+6B
+�B
,=B
,�B
-B
-CB
-wB
-wB
-�B
-�B
-�B
-�B
.B
.B
.}B
.�B
.�B
/OB
/�B
/�B
/�B
/OB
/OB
/OB
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
33B
3�B
4B
4nB
4nB
4nB
49B
4B
4�B
5B
5B
5tB
5?B
6B
6B
6zB
6FB
6zB
5B
5B
4nB
5�B
6B
6�B
7B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
:^B
:*B
:*B
:�B
:�B
:�B
:�B
;0B
<�B
<�B
<jB
<6B
<jB
=B
=<B
=<B
=<B
=�B
=<B
=qB
=qB
>B
=�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
AUB
B'B
DgB
D�B
EB
EmB
E�B
FB
FB
F?B
GB
GzB
G�B
G�B
H�B
HKB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J#B
JXB
J�B
K�B
L�B
M6B
MjB
MjB
M�B
NB
N<B
NB
N<B
M�B
M�B
NB
M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BSB{BoBB�B{B�BuB�B(B�B�BBB�B�B�BB�B�BbB,qB �BB~B(B	�B1BJB�BJB
rBBB
�BPBDBDB�BB~BB
�BJB
�BJBxB
	B�B	�BB
=B
=B
�B
rBB	�BJBBB	�B	�B
=B	lB
�BfB	lB	7B�B
=B�B	lB1B1B�B�B	B�B�B1B�B	lB_B	B	lB1B
=B�B�BB	B	lB�B�B�B1B	�B�BfB�B	BSB
=B�B	BfB�BB
rB
�B
�B1BDB	7B�B�B
�B
�BPB~B�B\B�B(BPB�B�BB�B\B:B�B�B �B%B*�B2�B8�B>BB@OB@�BA�B?HBA�B@�BA BE�BB'BC-BF�BC�BEBK)BF�BHKBF?BE�BGEBGzBE�BH�BFtBGzBG�BFBE�BEBB[BA�BA�B?HBAUB?HB>B>�B=<B=B>�B=�B?�B@B?BA�BB�B>�B?�B@BA�B@�B=�B?�B>wB>wB@�B@�B@�BCaBA�BB'BD�BB'BB�BD�BC-BCaBDgBB�BC�BE�BC�BD�BE9BC�BIRBJXBK�BM�BL0BN�BOBM6BN�BQBPHBZ�Ba�B_�B^B`�Bb�Bb�Be,Bc�Be`Bh
Bm�Bh
BgmBf�Be�BgmBf�BcTBkQBj�BlWBo5Bp�BsBs�BqvBp�BncBpoBo5Bp;Bp�BtTBt�Bv`Bq�Bn�BqvBpBn/B~]Bx8Bw2Bv�Bv`BwfB{BzBx�BzxBy>Bx�B|Bz�B{�B�B��B��B��B��B��B�4BŢB��B�jB��B��B�B�[B�TB��B�B�sB�9B�2B҉BԕB� BΥB�TB̘B��B��B�KB��BĜB��B��B�B�3B�?B�B��B�0B�gB�0B��B�gB�8B�B�/B�B� B�vB��B�BB�jBޞB�B�5B��BޞB�B�;B�TB�|B��B�B�2B�8B�WB�B;B��B�B iB�.B iB
�B�BoB�B�B�B�B�B.BB�B�B�B�BbB\B B�BhB\B�BoB�BSBkBYBeBBkBFB�BBFB�B�B�B.B@B(B$@BBB"4B"4B �B�B�B�B�BB"hB!-B:�B*eB.IB*�B3hB1�B)_B-�B*�B(�B&�B$@B%�B%B"�B#�B"�B$tBVB;�BJ#B=qB<�BJ�B<jB:�B@�B=�B=qB=BA BMjBFBP}BEBE�BK)BQBK�BFBIBN<BI�BIRBL�BIRBLdBI�BJ#BH�BH�BI�BG�BHBH�BHKBGzBE�BGzBE9BGEBGEBI�BFtBL�BL�BPHBRTBT�BT�BM�BOBBP�BQBN�BM6BP}BU2BN<B]/BXyBRTBP�BV9BT�B`BB^�BS�BYBXyBW
BTaBT,BZQBd&BZ�B[�BXyBZ�Bm]B]�BVmB_BVBVBS[BQNBR�BP}BP}BRTBP}BO�BQ�BOvBP�BQNBOvBO�BP�BN�BN�BPBNpBOBBOBBMBM�BNpBL�BM�BW�BbNBS�BR BNBN�BN<BK�BLdBM�BM6BNpBN<BMjBK)BOvBOBM�BM�BG�BGzBI�BIBG�BH�BGzBEmBG�BF?BF�BF�BG�BGzBDgBD�BD3BB'BB�BA�B@�BB[BAUB?}BB'BDgB?}B<6B;�BB'B>�BB�B>BUgB;�B9�B:�B4B2�B1�B2�B33B8�B6�B8RB=<B>B=�B?BK�B?}B@�B>�BEB=�B?BF�B=�B;�B9$B>BA�BA�B=qB<�B<B>wB>wB=B=<B?B>�B=qB>�B?HB>�B>B=qB=B=�B>�B>wB>�B=<B=B=<B=qB=<B?BG�B>B;�B?HBYKB<6BA�B<�B>�B;0B9XB>BB9�B9�B:^B:�B;dB;0B9�B7�B;�B:�B8�B<6B@�B9XB5�B8�B8�B6�B4�B5?B9�B7LB3hB3hB2�B0!B2�B7�B6B;0B7�B/�B/�B1�B0UB.�B,qB.}B1'B>�B0�B,B.IB+B$@B�B�B$B_B1B@B	BuB�B�B�BoB��B�ZB�B��B�TB�%B�5B�,B�>B�B�B�B�pB��B�#B��B��B�B�]B�dBݘB՛B�aB��B�vB�BB�<BȀB��B�#B�)B��B՛B��B�B�aB�IB�B�B�6B�kB��B�kB��B�=B��B�B��B��B�B�0B��B��B��B�qB�_B��B��B�*B�*B�kB��444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022091505233020220915052330IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022091610011820220916100118QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022091610011820220916100118QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      PRES                            D�� G�O�D��HG�O�@@  G�O�Valu passes DMQC                SI      ARSQ    SIQC    V3.1                                                                                                                                    20230210220014              CF      TEMP                            D�� G�O�D��HG�O�@�  G�O�Valu passes DMQC                SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                