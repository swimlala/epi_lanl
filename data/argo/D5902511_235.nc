CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-11-05T07:04:27Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  Y�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  v   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �d   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  ܸ   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ,�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ,�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   2�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   8�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T >�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ?D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ?L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ?T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ?\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ?d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ?�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   @    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    @   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        @(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        @0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       @8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    @@Argo profile    3.1 1.2 19500101000000  20221105070427  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_235                 6810_008521_235                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���Q���@���Q���11  @������@������@2*��Y��@2*��Y���d���łV�d���łV11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@�G�@��R@��R@�  A   A  A\)A,(�A@��A`  A\)A�Q�A�Q�A��A�\)A�  A߮A�  B (�BQ�B  B(�B (�B(  B0  B8  B?�
BH  BP  BX  B`  Bh(�Bp  Bx  B�  B��B��B��
B��B�  B�{B�{B�  B��
B�  B�  B�  B�{B�  B�{B�(�B�{B�{B�(�B�{B��B�  B�(�B��B�  B�{B�{B�{B�=qB�{B��C   C{C
=C  C  C
  C{C{C  C
=C
=C  C  C
=C
=C  C 
=C"{C$
=C&  C(  C*  C,  C.  C0
=C2
=C4  C5��C7�C9��C<  C>
=C?��CA��CD  CF  CH  CJ
=CK��CN  CP  CR  CS��CV  CX  CZ
=C[��C^  C`  Ca��Cd  Ce��Ch
=Cj  Cl  Cm��Co��Cr
=Ct
=Cv  Cw��Cy��C|  C}��C�  C�C�C�  C�  C�  C�  C�C�C�C�C�C�C�C�C�  C�C���C���C�  C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C�C���C���C���C���C���C���C���C�C�
=C�C���C���C�  C�C�  C�C���C���C���C�  C�  C���C�  C�C�  C�C�C�C�C�  C���C���C�  C�  C���C���C���C�  C�C�  C�  C�  C�C�  C���C�C�C�C�  C���C���C�C�C�C�
=C�
=C�C�  C�  C�  C�  C�  C�C�C�C�C�  C���C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C���C���C�  C���C���C�  C�C�  C�  C���C���C���C�D   D ��D�D� D�qD}qD�D��D�D}qD�D� D�qD� D�D� D�qDz�D��D	z�D
  D
�DD��D�D��D�D��D�D� D  Dz�D��D}qD�qD}qD  D��DD��D  D��DD� D�D� D�qD}qD  D��D  Dz�D  D��D�D��D�D��D  D� D�D� D  D��D �D }qD �qD!� D"  D"��D#  D#}qD$D$� D$�qD%� D&  D&}qD'  D'��D(  D(� D)�D)��D)�qD*}qD*�qD+� D,  D,}qD-  D-� D.  D.��D/  D/� D0�D0� D0�qD1}qD1��D2z�D3  D3��D4�D4� D5  D5� D5�qD6z�D7  D7� D7��D8z�D9�D9��D:�D:��D;  D;� D<�D<� D<�qD=��D>�D>�D?�D?� D?�qD@� DA�DA� DB�DB� DC  DC��DD  DD� DE�DE��DE�qDFz�DF�qDG� DH  DH}qDH��DI� DJ�DJ� DK  DK� DL  DL� DM�DM�DN�DN� DO�DO� DP  DP}qDQ  DQ� DR�DR�DR�qDS}qDT  DT� DU  DU��DVDV� DV��DWz�DX  DX}qDX��DY� DZ  DZ}qD[  D[}qD\�D\��D\��D]}qD]�qD^� D^�qD_}qD`  D`� D`�qDa}qDa��Db}qDc  Dc��Dc�qDd}qDe  De��Df  Df}qDf�qDg� Dh�Dh��Di  Di� Dj�Dj� Dj�qDkz�Dk�qDl� Dm�Dm� Dm�qDn��Do�Do� DpDp� Dp�qDq� Dr  Dr� Ds�Ds� Ds�qDtz�Dt�qDu��Dv  Dv}qDw  Dw�Dx  Dx}qDx�qDy� Dz  Dz}qD{�D{� D|  D|� D|�qD}z�D}�qD~� D  D��D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�~�D�� D�HD�AHD�~�D���D���D�@ D��HD�D�HD�>�D�~�D�� D�  D�AHD�� D�� D�HD�=qD�~�D���D�  D�@ D�� D���D���D�AHD��HD���D�  D�ED�� D��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D���D�  D�@ D�� D�� D�HD�AHD�� D���D���D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD��HD�HD�AHD�y�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?aG�?�=q?�Q�?�
=?�@�@(�@333@:�H@Tz�@\(�@s33@�  @�=q@���@��H@��
@�=q@�z�@���@Ǯ@˅@�
=@޸R@���@�z�@���A33AffA�A  Az�A��A��A ��A$z�A)��A,(�A1�A4z�A:=qA=p�AAG�AE�AH��AN�RAQG�AW�AZ�HA_\)Ae�AhQ�An�RAq�Aw
=A|��A\)A��\A���A�
=A�=qA��
A�ffA���A��\A�{A��A��\A�(�A�
=A��A��A�ffA�Q�A��\A�A�
=A�=qA�(�A��RA�G�A��HA�{A�\)A\A�z�A�
=A�G�A��HA�{A�\)A��AӅA�A׮A�G�A��
A��A�Q�AᙚA�(�A�{A�Q�A�33A���A�
=A�A�A��RA�  A��HA���A��B�B{B�BQ�B{B�RB(�B	G�B
=qB\)B(�B��B=qB�BQ�B��B�RB�B��B�B�B(�BB�RB�
BG�B{B�B ��B"{B#\)B$(�B%�B&�RB((�B)G�B*=qB+�
B,��B-�B/�B0Q�B1�B2�HB4Q�B5��B6ffB8(�B8��B:ffB;�B<z�B=�B?�B@Q�BB{BB�HBDz�BEp�BF�RBH(�BI�BJ�RBK�BL��BNffBO33BP��BQ�BS
=BTz�BUG�BW
=BX  BY�BZ�\B[\)B]�B]�B_�B`��Ba�Bc33BdQ�BeBf�RBhQ�BiG�Bj�RBl  Bl��Bn�\Bo�Bp��Br{Bs�Btz�BuBw33Bx  ByBz�HB{�
B}p�B~=qB�B��\B�
=B��
B�Q�B�
=B��B�(�B��HB�p�B�(�B���B�G�B�(�B��\B�\)B�  B�z�B�G�B��
B�ffB�33B��B�z�B��HB���B�Q�B��RB��B�{B��\B�\)B��
B�Q�B��B��B�(�B��HB�G�B��B��\B���B��B�{B���B�\)B�B�=qB���B�\)B��B���B��B��B�ffB���B�\)B�  B�Q�B��B���B�{B���B�33B��
B�z�B���B�B�ffB��HB���B�=qB��RB�\)B�{B�z�B�33B��
B�Q�B���B��B�{B��HB�p�B��B���B�G�B��
B�Q�B��HB���B�(�B��\B�\)B�  B�ffB���B��B�(�B��RB��B�(�B���B�G�B�  B�ffB�33B�B�(�B���B��B�  B���B�G�B��
Bģ�B�33BŮB�ffB��BǙ�B�{B��HB�p�B��Bʣ�B�G�BˮB�ffB��BͮB�=qB�
=Bϙ�B�(�B��HBљ�B�(�BҸRB�p�B�(�Bԏ\B�\)B�{B֏\B�
=B��
B؏\B��Bٙ�B�ffB��Bۙ�B�=qB�
=BݮB�(�B޸RBߙ�B�=qB�RB�\)B�(�B��B�G�B�{B��B�33B��B��B�33B��
B��B�G�B��
B�z�B�\)B��B�ffB�33B��B�z�B�
=B�B�z�B�
=B�B�Q�B��B�B�Q�B��B��B�=qB�
=B��
B�ffB���B��
B�z�B���B��
B�z�B���B��B�z�B��B���C �C �\C �
C�C�\C�
C�C�C�
C�C�C�
C(�C�\C��C33C�\C��C=qC�\C  CQ�C��C��CffCC	
=C	Q�C	��C
{C
ffC
��C33Cp�C��C33C�\C��C33C�\C�CG�C��C�HC33C��C  CQ�C��C�HC=qC��C  CG�C��C�CQ�C�RC
=CQ�C�RC�CffC�RC�Cz�C�C  CffCC�CffC�RC{Cz�C��C{CffC��C(�Cz�CC{Cz�C��C{CffC�C��CffC��C�
C(�C�C��C   C 33C p�C C ��C!�C!ffC!�C!�
C!��C"(�C"ffC"��C"�RC"��C#
=C#G�C#\)C#z�C#�C#�C${C$(�C$Q�C$�C$�RC$�C%  C%�C%\)C%�\C%�RC%��C%��C&(�C&ffC&�\C&��C&��C'
=C'=qC'p�C'�C'��C'��C(  C(=qC(\)C(p�C(��C(�
C)  C){C)33C)ffC)��C)��C)�HC*
=C*G�C*z�C*��C*�RC*�
C+
=C+G�C+z�C+��C+�RC+��C,(�C,G�C,p�C,�\C,C-  C-(�C-G�C-p�C-��C-�
C.
=C.(�C.G�C.�C.�RC.�
C/  C/33C/z�C/��C/C/�HC0{C0\)C0�\C0�RC0�HC1
=C133C1ffC1��C1�
C1�C2{C2Q�C2�\C2C2�C3
=C333C3p�C3��C3�
C4  C4(�C4Q�C4z�C4��C4�HC5�C5G�C5p�C5��C5�
C6�C6G�C6ffC6�C6�C7{C7=qC7z�C7�RC7��C8�C8G�C8p�C8��C8�C9(�C9ffC9��C9C9�C:�C:\)C:��C:�
C;
=C;33C;\)C;��C;�
C<{C<Q�C<�C<�C<�C={C=G�C=�C=C>  C>33C>p�C>�C>�HC?
=C?=qC?p�C?��C?�HC@�C@\)C@��C@�
CA  CA=qCA\)CA��CA�
CB�CBQ�CB�\CBCB�CC�CCffCC��CC�HCD(�CDffCD�CD�
CE{CEG�CEz�CE�CE�CF(�CFffCF��CF�HCG�CGQ�CGz�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?u@   @B�\@�G�@��R@��R@�  A   A  A\)A,(�A@��A`  A\)A�Q�A�Q�A��A�\)A�  A߮A�  B (�BQ�B  B(�B (�B(  B0  B8  B?�
BH  BP  BX  B`  Bh(�Bp  Bx  B�  B��B��B��
B��B�  B�{B�{B�  B��
B�  B�  B�  B�{B�  B�{B�(�B�{B�{B�(�B�{B��B�  B�(�B��B�  B�{B�{B�{B�=qB�{B��C   C{C
=C  C  C
  C{C{C  C
=C
=C  C  C
=C
=C  C 
=C"{C$
=C&  C(  C*  C,  C.  C0
=C2
=C4  C5��C7�C9��C<  C>
=C?��CA��CD  CF  CH  CJ
=CK��CN  CP  CR  CS��CV  CX  CZ
=C[��C^  C`  Ca��Cd  Ce��Ch
=Cj  Cl  Cm��Co��Cr
=Ct
=Cv  Cw��Cy��C|  C}��C�  C�C�C�  C�  C�  C�  C�C�C�C�C�C�C�C�C�  C�C���C���C�  C�  C�C�  C�C�  C���C�  C�  C�  C�  C�  C�C���C���C���C���C���C���C���C�C�
=C�C���C���C�  C�C�  C�C���C���C���C�  C�  C���C�  C�C�  C�C�C�C�C�  C���C���C�  C�  C���C���C���C�  C�C�  C�  C�  C�C�  C���C�C�C�C�  C���C���C�C�C�C�
=C�
=C�C�  C�  C�  C�  C�  C�C�C�C�C�  C���C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C���C���C�  C���C���C�  C�C�  C�  C���C���C���C�D   D ��D�D� D�qD}qD�D��D�D}qD�D� D�qD� D�D� D�qDz�D��D	z�D
  D
�DD��D�D��D�D��D�D� D  Dz�D��D}qD�qD}qD  D��DD��D  D��DD� D�D� D�qD}qD  D��D  Dz�D  D��D�D��D�D��D  D� D�D� D  D��D �D }qD �qD!� D"  D"��D#  D#}qD$D$� D$�qD%� D&  D&}qD'  D'��D(  D(� D)�D)��D)�qD*}qD*�qD+� D,  D,}qD-  D-� D.  D.��D/  D/� D0�D0� D0�qD1}qD1��D2z�D3  D3��D4�D4� D5  D5� D5�qD6z�D7  D7� D7��D8z�D9�D9��D:�D:��D;  D;� D<�D<� D<�qD=��D>�D>�D?�D?� D?�qD@� DA�DA� DB�DB� DC  DC��DD  DD� DE�DE��DE�qDFz�DF�qDG� DH  DH}qDH��DI� DJ�DJ� DK  DK� DL  DL� DM�DM�DN�DN� DO�DO� DP  DP}qDQ  DQ� DR�DR�DR�qDS}qDT  DT� DU  DU��DVDV� DV��DWz�DX  DX}qDX��DY� DZ  DZ}qD[  D[}qD\�D\��D\��D]}qD]�qD^� D^�qD_}qD`  D`� D`�qDa}qDa��Db}qDc  Dc��Dc�qDd}qDe  De��Df  Df}qDf�qDg� Dh�Dh��Di  Di� Dj�Dj� Dj�qDkz�Dk�qDl� Dm�Dm� Dm�qDn��Do�Do� DpDp� Dp�qDq� Dr  Dr� Ds�Ds� Ds�qDtz�Dt�qDu��Dv  Dv}qDw  Dw�Dx  Dx}qDx�qDy� Dz  Dz}qD{�D{� D|  D|� D|�qD}z�D}�qD~� D  D��D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D���D���D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�~�D�� D�HD�AHD�~�D���D���D�@ D��HD�D�HD�>�D�~�D�� D�  D�AHD�� D�� D�HD�=qD�~�D���D�  D�@ D�� D���D���D�AHD��HD���D�  D�ED�� D��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D���D�  D�@ D�� D�� D�HD�AHD�� D���D���D�@ D�� D�� D�  D�@ D�~�D���D���D�@ D��HD��HD�HD�AHG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?�?aG�?�=q?�Q�?�
=?�@�@(�@333@:�H@Tz�@\(�@s33@�  @�=q@���@��H@��
@�=q@�z�@���@Ǯ@˅@�
=@޸R@���@�z�@���A33AffA�A  Az�A��A��A ��A$z�A)��A,(�A1�A4z�A:=qA=p�AAG�AE�AH��AN�RAQG�AW�AZ�HA_\)Ae�AhQ�An�RAq�Aw
=A|��A\)A��\A���A�
=A�=qA��
A�ffA���A��\A�{A��A��\A�(�A�
=A��A��A�ffA�Q�A��\A�A�
=A�=qA�(�A��RA�G�A��HA�{A�\)A\A�z�A�
=A�G�A��HA�{A�\)A��AӅA�A׮A�G�A��
A��A�Q�AᙚA�(�A�{A�Q�A�33A���A�
=A�A�A��RA�  A��HA���A��B�B{B�BQ�B{B�RB(�B	G�B
=qB\)B(�B��B=qB�BQ�B��B�RB�B��B�B�B(�BB�RB�
BG�B{B�B ��B"{B#\)B$(�B%�B&�RB((�B)G�B*=qB+�
B,��B-�B/�B0Q�B1�B2�HB4Q�B5��B6ffB8(�B8��B:ffB;�B<z�B=�B?�B@Q�BB{BB�HBDz�BEp�BF�RBH(�BI�BJ�RBK�BL��BNffBO33BP��BQ�BS
=BTz�BUG�BW
=BX  BY�BZ�\B[\)B]�B]�B_�B`��Ba�Bc33BdQ�BeBf�RBhQ�BiG�Bj�RBl  Bl��Bn�\Bo�Bp��Br{Bs�Btz�BuBw33Bx  ByBz�HB{�
B}p�B~=qB�B��\B�
=B��
B�Q�B�
=B��B�(�B��HB�p�B�(�B���B�G�B�(�B��\B�\)B�  B�z�B�G�B��
B�ffB�33B��B�z�B��HB���B�Q�B��RB��B�{B��\B�\)B��
B�Q�B��B��B�(�B��HB�G�B��B��\B���B��B�{B���B�\)B�B�=qB���B�\)B��B���B��B��B�ffB���B�\)B�  B�Q�B��B���B�{B���B�33B��
B�z�B���B�B�ffB��HB���B�=qB��RB�\)B�{B�z�B�33B��
B�Q�B���B��B�{B��HB�p�B��B���B�G�B��
B�Q�B��HB���B�(�B��\B�\)B�  B�ffB���B��B�(�B��RB��B�(�B���B�G�B�  B�ffB�33B�B�(�B���B��B�  B���B�G�B��
Bģ�B�33BŮB�ffB��BǙ�B�{B��HB�p�B��Bʣ�B�G�BˮB�ffB��BͮB�=qB�
=Bϙ�B�(�B��HBљ�B�(�BҸRB�p�B�(�Bԏ\B�\)B�{B֏\B�
=B��
B؏\B��Bٙ�B�ffB��Bۙ�B�=qB�
=BݮB�(�B޸RBߙ�B�=qB�RB�\)B�(�B��B�G�B�{B��B�33B��B��B�33B��
B��B�G�B��
B�z�B�\)B��B�ffB�33B��B�z�B�
=B�B�z�B�
=B�B�Q�B��B�B�Q�B��B��B�=qB�
=B��
B�ffB���B��
B�z�B���B��
B�z�B���B��B�z�B��B���C �C �\C �
C�C�\C�
C�C�C�
C�C�C�
C(�C�\C��C33C�\C��C=qC�\C  CQ�C��C��CffCC	
=C	Q�C	��C
{C
ffC
��C33Cp�C��C33C�\C��C33C�\C�CG�C��C�HC33C��C  CQ�C��C�HC=qC��C  CG�C��C�CQ�C�RC
=CQ�C�RC�CffC�RC�Cz�C�C  CffCC�CffC�RC{Cz�C��C{CffC��C(�Cz�CC{Cz�C��C{CffC�C��CffC��C�
C(�C�C��C   C 33C p�C C ��C!�C!ffC!�C!�
C!��C"(�C"ffC"��C"�RC"��C#
=C#G�C#\)C#z�C#�C#�C${C$(�C$Q�C$�C$�RC$�C%  C%�C%\)C%�\C%�RC%��C%��C&(�C&ffC&�\C&��C&��C'
=C'=qC'p�C'�C'��C'��C(  C(=qC(\)C(p�C(��C(�
C)  C){C)33C)ffC)��C)��C)�HC*
=C*G�C*z�C*��C*�RC*�
C+
=C+G�C+z�C+��C+�RC+��C,(�C,G�C,p�C,�\C,C-  C-(�C-G�C-p�C-��C-�
C.
=C.(�C.G�C.�C.�RC.�
C/  C/33C/z�C/��C/C/�HC0{C0\)C0�\C0�RC0�HC1
=C133C1ffC1��C1�
C1�C2{C2Q�C2�\C2C2�C3
=C333C3p�C3��C3�
C4  C4(�C4Q�C4z�C4��C4�HC5�C5G�C5p�C5��C5�
C6�C6G�C6ffC6�C6�C7{C7=qC7z�C7�RC7��C8�C8G�C8p�C8��C8�C9(�C9ffC9��C9C9�C:�C:\)C:��C:�
C;
=C;33C;\)C;��C;�
C<{C<Q�C<�C<�C<�C={C=G�C=�C=C>  C>33C>p�C>�C>�HC?
=C?=qC?p�C?��C?�HC@�C@\)C@��C@�
CA  CA=qCA\)CA��CA�
CB�CBQ�CB�\CBCB�CC�CCffCC��CC�HCD(�CDffCD�CD�
CE{CEG�CEz�CE�CE�CF(�CFffCF��CF�HCG�CGQ�CGz�CG�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aް!A޶FAް!A޺^A�A�A���A���A���A���A���A���A�ĜA�ĜA���A޸RA޸RA޺^A޼jA޾wA���A���A޾wA޺^A޺^Aމ7AށA�~�A�|�A�x�A�t�A�t�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�n�A�jA�dZA�Q�A��A�VA�I�A���A�1'A���A��AыDAήA�^5A�K�A�\)A�\)A���A�E�A��AčPA�(�A���A�x�A�dZA�A�9XA��A�;dA��\A��A�1A���A�/A���A���A�C�A�?}A�A�A�A�-A���A���A�XA��A���A��A�XA�%A�bNA�Q�A�&�A�jA��A���A��+A��+A�ĜA�A��^A�ƨA��A��9A�{A�bA�?}A�O�A��A���A�C�A��A��A��A�ĜA�G�A�K�A~��Ay�Av�At��Ar��Aq�An��AlĜAkp�Ag��Af�Ac�Ab��AaXA]��A\��A\A�AY\)AVM�ASdZAP�yAM�hALbAJ�!AI�-AHĜAEG�AA�A>�9A="�A<�A:��A9
=A7�hA7�A6��A6bA5��A5�A4ZA2��A1�A0jA-��A(��A&�A&r�A%�A#�;A"�yA!�TA��A�FA�jAK�AJAE�Al�A�A�jA{AA�AQ�A��A��AffA9XA�-A��A9XAAx�A
~�A�`A�PA��AjA`BA^5A-A$�A33A�A �RA $�@��y@�j@���@�|�@�\)@�5?@��wA bNA�AdZA �yA ��A ��A {@���A Z@�@��y@��`@�C�@�
=@�V@�V@�{@�{@��u@�j@�\)@�o@�j@�@���@�
=@�33@�
=@��m@�@� �@��@� �@�\)@�^5@�z�@��@�^5@�hs@��#@�7@��@⟾@�r�@�"�@�(�@�@ޏ\@ݙ�@�/@�"�@ى7@׮@��H@֏\@��H@��T@�Z@ӍP@��@ҸR@҇+@���@ѩ�@��@�%@�j@��@Ο�@�ff@�$�@��@́@��`@�V@�Ĝ@�j@˝�@ˍP@�@�ff@��T@�X@ȴ9@��@�Q�@��@Ǿw@�l�@���@��@ř�@���@���@�1@¸R@���@�9X@�+@�Z@���@��D@�bN@���@�K�@�+@��!@�n�@��^@��j@���@���@�Z@��
@�dZ@�ȴ@���@�p�@�?}@���@���@�&�@��@���@���@�l�@��@�^5@��@�V@���@�bN@��@��@�C�@�o@��@���@��@�o@�
=@��@�n�@���@��h@�O�@��@�%@�V@��@���@��j@�bN@��@��@��w@��F@��w@���@��@�^5@�V@�=q@��@���@�p�@��7@�p�@�Ĝ@�bN@�A�@�  @�l�@��\@���@���@�hs@�p�@�x�@�G�@��j@�r�@�9X@��@��F@��H@�{@��7@�X@�&�@��@��@�O�@�G�@�Q�@��;@��;@��w@��@���@�|�@�;d@��@��@�o@�
=@��H@���@���@�=q@���@��7@�G�@�V@��@�(�@��F@��F@���@�t�@�;d@���@�ȴ@���@��\@�ff@�-@��T@��^@���@�p�@�hs@�?}@�Ĝ@�A�@�9X@�Q�@�9X@�b@��;@�K�@���@���@�ff@��@��h@�/@�V@���@���@��@��D@�(�@�ƨ@�S�@���@��H@���@�v�@�$�@���@��-@���@���@��u@�Z@�9X@�9X@�\)@�@��\@�M�@��^@��^@���@�/@��/@���@���@���@���@���@��@���@�=q@�5?@��T@�p�@�7L@���@�I�@�1'@���@��
@��P@�+@���@���@�ff@�{@���@��7@�X@��@�%@��D@�(�@��;@���@�|�@�+@��H@�~�@��@���@��-@�`B@��@���@�%@��@��@�%@���@���@��/@���@�r�@�A�@��
@�|�@�;d@��@��\@�$�@��@���@�x�@�hs@�G�@�/@�%@�Ĝ@�r�@�Q�@�I�@�(�@�@�P@
=@~�y@~��@}�@}�@}�@|z�@{��@{"�@{@z��@z~�@z�\@{��@{��@z�@z��@z=q@y�@y�^@yhs@y&�@xĜ@xbN@xA�@x  @w�@wK�@w;d@vȴ@vV@u��@u`B@t�@t9X@s�m@so@r��@rJ@qx�@q%@p��@pA�@o�@o�@n�R@n��@n��@n��@nff@n{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aޙ�A޶FAޮA޶FA޶FA޴9A޺^Aް!A޴9AޮA޴9AޮA޾wA޶FA޺^A޸RA�ƨA�A�ȴA�A޺^A���A�ĜA���A�ȴA�ȴA���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A�ƨA���A�ƨA���A���A���A���A�ƨA���A�A�A�ƨA޾wA�ĜA���A�ĜA�ĜA�A�ƨA�ĜA޾wA޾wA�ĜA�A�A޾wA�A޼jA޼jA޸RA޴9A޸RA޴9A޺^A޶FA޸RA޼jA޸RA޼jA޶FA޸RA޼jA޸RA޾wA޸RA޾wA޸RA޼jA޼jA޸RA޾wA޺^A޾wA޺^A޼jA޼jA޼jA޾wA޺^A�A޼jA�ƨA�A���A�A޼jA޾wA���A���A�ƨA޼jA�A޼jA���A���A޼jA���A���A�ĜA޼jA޺^A޼jA޼jA޾wA޸RA�A޸RA���A޶FA޾wAޮA޲-A޸RA޶FA޺^A޶FA޾wA޾wA޸RAއ+AރAލPAމ7Aމ7AޅAށAޅA�~�AރA�~�A�|�AރA�|�A�|�AށA�z�AށA�z�A�~�A�|�A�z�A�~�A�x�A�|�A�z�A�v�A�z�A�z�A�x�A�z�A�t�A�z�A�r�A�v�A�v�A�r�A�v�A�r�A�r�A�v�A�p�A�v�A�r�A�r�A�t�A�p�A�t�A�p�A�r�A�t�A�p�A�t�A�n�A�t�A�p�A�r�A�p�A�p�A�r�A�n�A�t�A�p�A�v�A�t�A�r�A�v�A�p�A�v�A�r�A�v�A�t�A�t�A�v�A�p�A�v�A�t�A�p�A�t�A�n�A�p�A�t�A�n�A�v�A�p�A�r�A�t�A�n�A�t�A�p�A�p�A�r�A�l�A�r�A�l�A�p�A�n�A�l�A�p�A�n�A�l�A�p�A�jA�n�A�ffA�hsA�l�A�hsA�n�A�jA�ffA�jA�dZA�bNA�dZA�^5A�^5A�\)A�S�A�S�A�Q�A�E�A�G�A�?}A�/A�-A� �A��A�oA�
=A�%A��TA�~�A���A��;A���A۶FAۥ�AۓuA�~�A�hsA�I�A�"�A��mA�ȴAڗ�A�VA�bNAؗ�A�ĜA�VA�A���A�|�A�ffA�  A�|�A�+A�A�Q�A�ȴAӋDA�l�A�VA�JA�A�A�=qA� �A��A���A�ZA�1'A���A���Aџ�A�p�A�G�A��mA�M�Aϴ9A�JA�G�A��A��TA���Aͺ^AͶFA͟�A�x�A�Q�A��A�`BA˛�A�n�A�I�A�5?A�bA���A�v�A�^5A�VA�Q�A�S�A�Q�A�I�A�?}A�+A���A�=qAȣ�A�7LA���A�x�AƾwA�t�A�dZA�VA�I�A�G�A�I�A�I�A�C�A�C�A�;dA�$�A��A�1A��AżjAŅA�S�A�
=Aħ�A�XA�/A��A�%A��yA�z�A�VA���A�9XA��/A�ffA�  A�K�A��A��A�A���A��hA��A�l�A�`BA�-A�"�A���A��DA�VA��-A��DA�ZA�+A�bA��A���A��FA��A�jA�S�A�?}A��A���A���A�I�A�
=A���A���A��+A�t�A�S�A�7LA�$�A�{A�JA�  A��/A�~�A�^5A�Q�A�G�A�A�A�33A�1A��#A���A��RA���A��A�=qA��-A��PA��PA�-A��mA�ȴA��!A���A��hA�v�A�dZA��A���A��A��mA��HA��/A�ƨA�ffA�-A�JA��TA���A�ĜA��FA���A���A�r�A�;dA�&�A�"�A�oA��wA�ZA�+A�{A���A���A��!A�hsA��A���A��A��\A�v�A�l�A�JA�ĜA�Q�A�=qA�9XA�-A�-A�$�A��A��A��A��A���A� �A��A���A�(�A�oA���A��`A��-A���A���A��\A�v�A�;dA�A��uA�=qA�JA��A���A�\)A�1'A�bA��`A���A��FA���A���A���A��+A�x�A�hsA�Q�A�"�A�
=A�  A���A��yA��;A���A���A�ƨA��A��7A�\)A�=qA�=qA�9XA�&�A�(�A�$�A�"�A�oA��TA��9A��hA�\)A�9XA�1A��RA��A�t�A�ffA�VA�A�A�5?A��A��A��DA�v�A�hsA�G�A�1'A��A��mA���A�C�A��HA�/A��HA�ĜA��9A���A�ZA�/A��A�jA��A��HA��A���A��FA�jA�+A�A��A��
A�ĜA���A��jA��jA��FA���A��7A��A��+A�bNA�M�A�I�A�C�A�9XA�(�A��A���A��TA�v�A�$�A���A��\A�x�A�jA�^5A�I�A�=qA�9XA�$�A��A�  A��A��mA��TA���A���A���A�A��FA��A���A���A���A��\A��A�l�A�E�A�33A��A���A��`A��HA���A��9A�ffA�7LA���A�`BA�5?A�JA���A��A��jA��-A���A�~�A�v�A�G�A�9XA��A�A��A�A��A��uA�ZA�E�A�?}A�-A�bA��TA�ƨA���A��+A�v�A�jA�VA�7LA��A�1A��A���A��!A���A�x�A�Q�A�/A��A��A�JA�1A�  A��TA���A��A�ZA�9XA�$�A�{A���A��;A���A��A��A��RA�jA�oA�ĜA��^A��uA��A�ƨA��A�O�A�;dA�$�A�{A�VA�A���A���A��A��/A���A���A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aް!A޶FAް!A޺^A�A�A���A���A���A���A���A���A�ĜA�ĜA���A޸RA޸RA޺^A޼jA޾wA���A���A޾wA޺^A޺^Aމ7AށA�~�A�|�A�x�A�t�A�t�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�n�A�jA�dZA�Q�A��A�VA�I�A���A�1'A���A��AыDAήA�^5A�K�A�\)A�\)A���A�E�A��AčPA�(�A���A�x�A�dZA�A�9XA��A�;dA��\A��A�1A���A�/A���A���A�C�A�?}A�A�A�A�-A���A���A�XA��A���A��A�XA�%A�bNA�Q�A�&�A�jA��A���A��+A��+A�ĜA�A��^A�ƨA��A��9A�{A�bA�?}A�O�A��A���A�C�A��A��A��A�ĜA�G�A�K�A~��Ay�Av�At��Ar��Aq�An��AlĜAkp�Ag��Af�Ac�Ab��AaXA]��A\��A\A�AY\)AVM�ASdZAP�yAM�hALbAJ�!AI�-AHĜAEG�AA�A>�9A="�A<�A:��A9
=A7�hA7�A6��A6bA5��A5�A4ZA2��A1�A0jA-��A(��A&�A&r�A%�A#�;A"�yA!�TA��A�FA�jAK�AJAE�Al�A�A�jA{AA�AQ�A��A��AffA9XA�-A��A9XAAx�A
~�A�`A�PA��AjA`BA^5A-A$�A33A�A �RA $�@��y@�j@���@�|�@�\)@�5?@��wA bNA�AdZA �yA ��A ��A {@���A Z@�@��y@��`@�C�@�
=@�V@�V@�{@�{@��u@�j@�\)@�o@�j@�@���@�
=@�33@�
=@��m@�@� �@��@� �@�\)@�^5@�z�@��@�^5@�hs@��#@�7@��@⟾@�r�@�"�@�(�@�@ޏ\@ݙ�@�/@�"�@ى7@׮@��H@֏\@��H@��T@�Z@ӍP@��@ҸR@҇+@���@ѩ�@��@�%@�j@��@Ο�@�ff@�$�@��@́@��`@�V@�Ĝ@�j@˝�@ˍP@�@�ff@��T@�X@ȴ9@��@�Q�@��@Ǿw@�l�@���@��@ř�@���@���@�1@¸R@���@�9X@�+@�Z@���@��D@�bN@���@�K�@�+@��!@�n�@��^@��j@���@���@�Z@��
@�dZ@�ȴ@���@�p�@�?}@���@���@�&�@��@���@���@�l�@��@�^5@��@�V@���@�bN@��@��@�C�@�o@��@���@��@�o@�
=@��@�n�@���@��h@�O�@��@�%@�V@��@���@��j@�bN@��@��@��w@��F@��w@���@��@�^5@�V@�=q@��@���@�p�@��7@�p�@�Ĝ@�bN@�A�@�  @�l�@��\@���@���@�hs@�p�@�x�@�G�@��j@�r�@�9X@��@��F@��H@�{@��7@�X@�&�@��@��@�O�@�G�@�Q�@��;@��;@��w@��@���@�|�@�;d@��@��@�o@�
=@��H@���@���@�=q@���@��7@�G�@�V@��@�(�@��F@��F@���@�t�@�;d@���@�ȴ@���@��\@�ff@�-@��T@��^@���@�p�@�hs@�?}@�Ĝ@�A�@�9X@�Q�@�9X@�b@��;@�K�@���@���@�ff@��@��h@�/@�V@���@���@��@��D@�(�@�ƨ@�S�@���@��H@���@�v�@�$�@���@��-@���@���@��u@�Z@�9X@�9X@�\)@�@��\@�M�@��^@��^@���@�/@��/@���@���@���@���@���@��@���@�=q@�5?@��T@�p�@�7L@���@�I�@�1'@���@��
@��P@�+@���@���@�ff@�{@���@��7@�X@��@�%@��D@�(�@��;@���@�|�@�+@��H@�~�@��@���@��-@�`B@��@���@�%@��@��@�%@���@���@��/@���@�r�@�A�@��
@�|�@�;d@��@��\@�$�@��@���@�x�@�hs@�G�@�/@�%@�Ĝ@�r�@�Q�@�I�@�(�@�@�P@
=@~�y@~��@}�@}�@}�@|z�@{��@{"�@{@z��@z~�@z�\@{��@{��@z�@z��@z=q@y�@y�^@yhs@y&�@xĜ@xbN@xA�@x  @w�@wK�@w;d@vȴ@vV@u��@u`B@t�@t9X@s�m@so@r��@rJ@qx�@q%@p��@pA�@o�@o�@n�R@n��@n��@n��@nffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aޙ�A޶FAޮA޶FA޶FA޴9A޺^Aް!A޴9AޮA޴9AޮA޾wA޶FA޺^A޸RA�ƨA�A�ȴA�A޺^A���A�ĜA���A�ȴA�ȴA���A���A���A���A���A�ȴA���A���A���A���A���A���A�ȴA���A�ƨA���A�ƨA���A���A���A���A�ƨA���A�A�A�ƨA޾wA�ĜA���A�ĜA�ĜA�A�ƨA�ĜA޾wA޾wA�ĜA�A�A޾wA�A޼jA޼jA޸RA޴9A޸RA޴9A޺^A޶FA޸RA޼jA޸RA޼jA޶FA޸RA޼jA޸RA޾wA޸RA޾wA޸RA޼jA޼jA޸RA޾wA޺^A޾wA޺^A޼jA޼jA޼jA޾wA޺^A�A޼jA�ƨA�A���A�A޼jA޾wA���A���A�ƨA޼jA�A޼jA���A���A޼jA���A���A�ĜA޼jA޺^A޼jA޼jA޾wA޸RA�A޸RA���A޶FA޾wAޮA޲-A޸RA޶FA޺^A޶FA޾wA޾wA޸RAއ+AރAލPAމ7Aމ7AޅAށAޅA�~�AރA�~�A�|�AރA�|�A�|�AށA�z�AށA�z�A�~�A�|�A�z�A�~�A�x�A�|�A�z�A�v�A�z�A�z�A�x�A�z�A�t�A�z�A�r�A�v�A�v�A�r�A�v�A�r�A�r�A�v�A�p�A�v�A�r�A�r�A�t�A�p�A�t�A�p�A�r�A�t�A�p�A�t�A�n�A�t�A�p�A�r�A�p�A�p�A�r�A�n�A�t�A�p�A�v�A�t�A�r�A�v�A�p�A�v�A�r�A�v�A�t�A�t�A�v�A�p�A�v�A�t�A�p�A�t�A�n�A�p�A�t�A�n�A�v�A�p�A�r�A�t�A�n�A�t�A�p�A�p�A�r�A�l�A�r�A�l�A�p�A�n�A�l�A�p�A�n�A�l�A�p�A�jA�n�A�ffA�hsA�l�A�hsA�n�A�jA�ffA�jA�dZA�bNA�dZA�^5A�^5A�\)A�S�A�S�A�Q�A�E�A�G�A�?}A�/A�-A� �A��A�oA�
=A�%A��TA�~�A���A��;A���A۶FAۥ�AۓuA�~�A�hsA�I�A�"�A��mA�ȴAڗ�A�VA�bNAؗ�A�ĜA�VA�A���A�|�A�ffA�  A�|�A�+A�A�Q�A�ȴAӋDA�l�A�VA�JA�A�A�=qA� �A��A���A�ZA�1'A���A���Aџ�A�p�A�G�A��mA�M�Aϴ9A�JA�G�A��A��TA���Aͺ^AͶFA͟�A�x�A�Q�A��A�`BA˛�A�n�A�I�A�5?A�bA���A�v�A�^5A�VA�Q�A�S�A�Q�A�I�A�?}A�+A���A�=qAȣ�A�7LA���A�x�AƾwA�t�A�dZA�VA�I�A�G�A�I�A�I�A�C�A�C�A�;dA�$�A��A�1A��AżjAŅA�S�A�
=Aħ�A�XA�/A��A�%A��yA�z�A�VA���A�9XA��/A�ffA�  A�K�A��A��A�A���A��hA��A�l�A�`BA�-A�"�A���A��DA�VA��-A��DA�ZA�+A�bA��A���A��FA��A�jA�S�A�?}A��A���A���A�I�A�
=A���A���A��+A�t�A�S�A�7LA�$�A�{A�JA�  A��/A�~�A�^5A�Q�A�G�A�A�A�33A�1A��#A���A��RA���A��A�=qA��-A��PA��PA�-A��mA�ȴA��!A���A��hA�v�A�dZA��A���A��A��mA��HA��/A�ƨA�ffA�-A�JA��TA���A�ĜA��FA���A���A�r�A�;dA�&�A�"�A�oA��wA�ZA�+A�{A���A���A��!A�hsA��A���A��A��\A�v�A�l�A�JA�ĜA�Q�A�=qA�9XA�-A�-A�$�A��A��A��A��A���A� �A��A���A�(�A�oA���A��`A��-A���A���A��\A�v�A�;dA�A��uA�=qA�JA��A���A�\)A�1'A�bA��`A���A��FA���A���A���A��+A�x�A�hsA�Q�A�"�A�
=A�  A���A��yA��;A���A���A�ƨA��A��7A�\)A�=qA�=qA�9XA�&�A�(�A�$�A�"�A�oA��TA��9A��hA�\)A�9XA�1A��RA��A�t�A�ffA�VA�A�A�5?A��A��A��DA�v�A�hsA�G�A�1'A��A��mA���A�C�A��HA�/A��HA�ĜA��9A���A�ZA�/A��A�jA��A��HA��A���A��FA�jA�+A�A��A��
A�ĜA���A��jA��jA��FA���A��7A��A��+A�bNA�M�A�I�A�C�A�9XA�(�A��A���A��TA�v�A�$�A���A��\A�x�A�jA�^5A�I�A�=qA�9XA�$�A��A�  A��A��mA��TA���A���A���A�A��FA��A���A���A���A��\A��A�l�A�E�A�33A��A���A��`A��HA���A��9A�ffA�7LA���A�`BA�5?A�JA���A��A��jA��-A���A�~�A�v�A�G�A�9XA��A�A��A�A��A��uA�ZA�E�A�?}A�-A�bA��TA�ƨA���A��+A�v�A�jA�VA�7LA��A�1A��A���A��!A���A�x�A�Q�A�/A��A��A�JA�1A�  A��TA���A��A�ZA�9XA�$�A�{A���A��;A���A��A��A��RA�jA�oA�ĜA��^A��uA��A�ƨA��A�O�A�;dA�$�A�{A�VA�A���A���A��A��/A���A���A���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��A��B�@B�uB�@B�B�uB�B�uB�@B��B��B�uB�{B��B�B��B�SB�B�B��B��B��B��B�YB��B��B�7B��B�xB�B��B��B�B�B��B��B��B�B��B�B�B�VB��B�-B��BB$�Bd�B�FBq�B��B��B�wB��B��B�B��B�*B�xB��B�+B��B�.B��B��B.B~�B�GB}�B}�B{B|PBx8Bs�Bp�Bk�BgmBe�BbNBXEB;0B:�B3hB#nBCB(B	B�B��B�B��B��B��B��B��B�{B�By>Bl�B^jBPB>BB+�BB
� B
�KB
��B
�kB
�-B
�\B
�~B
��B
��B
|�B
k�B
^B
GzB
+�B
�B
�B	�.B	�B	��B	�yB	�TB	��B	�?B	��B	��B	�	B	� B	v�B	qvB	iDB	X�B	F?B	=<B	.}B	"hB	!B	+B	�B	DB��B��B�AB�;B��B�B�KB�
B՛B��B��B��BԕB�B��B��B�UB��B��B�=B��B��B�:B��B�VB��B��B�{B�B�oB�B��B�7B�!B��B� B��B��B�4B�B��B��B��B�eB�\B�xB�!B�B��B��B�!B��B�4B��B��B��B�B�tB�@B�B�B��B��B�vBߤB	
�B	0�B	/B	*eB	'RB	+kB	)�B	'�B	>BB	@OB	@OB	G�B	CaB	D�B	CaB	GzB	EmB	X�B	P�B	Q�B	L0B	A B	1�B	?}B	B�B	QNB	]�B	^jB	gmB	hsB	e,B	_B	[WB	YB	W?B	R�B	PB	M6B	J�B	S�B	YKB	VmB	R�B	J#B	F�B	M�B	Z�B	ZQB	VB	V�B	VmB	VB	S�B	R B	T�B	_pB	_�B	a�B	aB	cTB	dZB	d�B	e�B	g8B	m]B	p;B	qAB	rGB	r�B	r|B	sMB	yrB	{�B	}�B	�oB	�B	��B	��B	��B	�B	��B	��B	�lB	��B	��B	��B	�B	��B	��B	�'B	�hB	��B	�FB	�*B	�*B	��B	��B	�:B	��B	��B	��B	�!B	��B	��B	�*B	�^B	�dB	�qB	�HB	�B	�B	�}B	��B	��B	�-B	��B	�3B	��B	�B	�mB	�B	�^B	�BB	��B	��B	ɆB	�KB	ȀB	̘B	�B	�B	�}B	� B	՛B	�mB	��B	�yB	��B	��B	��B	ߤB	�pB	��B	��B	��B	��B	�B	�vB	�BB	�HB	�B	�B	�ZB	��B	��B	�2B	�B	�sB	�B	�KB	�
B	�>B	��B	�B	�"B	��B	�/B	��B	��B	��B	�%B	��B	��B	��B	��B	��B	��B	�B	�8B	�DB	�PB	�B	�B	��B	��B	�"B	��B	�DB	�B	�B	��B	�.B
�B
uB
B
�B
�B
%B
�B
_B
1B
�B
�B
1B
	7B
	�B

	B

=B

�B
B
xB
DB
DB

�B
	�B
	lB
	7B
xB
�B
PB
"B
�B
�B
�B
�B
�B
�B
�B
.B
�B
.B
�B
�B
�B
hB
B
B
@B
B
@B
�B
�B
FB
MB
�B
+B
�B
YB
�B
�B
�B
�B
SB
B
�B
eB
1B
1B
qB
qB
�B
7B
CB
�B
=B
	B
�B
=B
�B
=B
B
�B
qB
B
�B
�B
IB
OB
!�B
!bB
!�B
$�B
#�B
"�B
!bB
$@B
$B
"�B
"4B
!�B
"hB
#�B
$�B
%B
$�B
$@B
$@B
$�B
%FB
&B
&�B
'�B
(�B
(�B
)_B
*eB
+B
+kB
+kB
+�B
*�B
)�B
)*B
(�B
(�B
(�B
)�B
*0B
*0B
+6B
,�B
,�B
.�B
/�B
0!B
1'B
1�B
1[B
1'B
0�B
0�B
/�B
/�B
/�B
/B
.�B
/B
/�B
/�B
0UB
0UB
0UB
0�B
1[B
1�B
1�B
2�B
3hB
3�B
4B
4B
4�B
4�B
5tB
5�B
6B
6�B
6FB
6B
6�B
7B
7LB
7LB
7�B
7�B
7LB
7�B
7�B
8RB
8�B
8�B
9$B
9�B
9�B
:�B
:�B
;�B
<6B
=<B
=�B
=�B
=�B
>B
>B
>BB
>�B
>�B
?HB
?}B
>�B
>�B
>�B
?HB
?�B
@OB
@�B
A�B
C-B
D�B
E�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@R��B�oB�B�:B��B�FB� B�B�B��B�:B��B��B��B�B�@B��B��B��B��B�B� B��B��B�oB�uB�:B�{B�oB��B�B�B��B�@B�uB�uB��B��B��B��B��B�oB�{B�B�B�B�oB�MB��B�B��B�@B��B�B��B��B��B��B��B�B��B�{B�FB��B�MB��B�{B��B��B��B�$B�SB��B��B��B��B�B�$B��B��B��B�B�$B��B�$B�B��B��B��B��B�B��B��B�SB��B��B��B��B��B��B��B��B�B�SB��B�YB�B��B��B�{B��B�MB�YB��B��B��B��B��B�MB��B��B��B��B�B�_B��B�_B��B�eB�B�1B�YB��B�+B��B�+B��B��B�+B�OB��B��B�B��B�B��B��B��B�xB�B|�B�qB�B�IB��B��B��B��B�CB�xB�B��B��B�B�xB��B�xB�CB�~B�B�OB�qB�~B��B�CB�B��B�IB�IB�B��B��B��B��B�B��B��B�~B��B�xB��B�B�OB�CB��B��B�B�B�CB��B��B�!B��B��B�VB�~B��B�IB�!B��B��B��B��B��B�~B�OB��B�B�VB��B��B�VB�B�!B��B�~B��B�~B��B�OB�~B�!B��B��B�~B��B�VB��B�OB��B�IB��B�B��B��B�~B��B�\B��B��B�B�B�RB��B�0B��B��B�'B��B��B��B��B�XB�<B�jB��B��B��B��B��BɆBBkB�B�B�B�B \B!�B �B$tB&�B.B-CB2-B:�B`Bw�B�~B�lB�B� B�:B��B�B��B�B�+B�MBe�Bd&BZ�BncBkB��B��B��B�*B��B��B�B��B�6B��B��B��B�*B�dB��B��B�tB��B��B�aB�hB��B�hB�B�9B��B�)B��B�UB��B�XB��B��B�_B��B�:B�tB�bB��B�nB��B��B�*B�3B��B�@B��B��B�XB��B��B��B�OB��B�xB��B��B��B�=B�B�B�=B��B��B��B��B��B�OB�YB�uB�bB�\B��B��B�B��B�_B��B��B��B� B�B�_B�SB��B��B�4B�iB}VB�Bz�B��B�	B�~B�GB{�B�{B�B{B}�B~�B�B�MB|�B|�B}�B}�B}VB��B��B�B�%B�;B|�B}�B��B}�B}�B|�B{JB{B.B�YB}VBz�By	BzDBzxB~�B|Bw�Bz�BzDBw2B� B��B��BwfB}"B}VBtTBu%Bs�Bs�Bt�BrB�uBo�Bp�Bl�Bn�Bm�Bp;B�oBl�Bp;Bm)Bk�Bi�BiBm�Bf�BpoBd�BgmBbNBe�Bu�BjBdZB^�B_;B`�B\)Ba|B]�BS[BaB�~Bu�B^jBI�BOBBK�B=�B;�B;�B:�B6FB;�B7�B5?B?BB[B4�B0!B?}B+�B*eBG�B2�BVB#�B�B"�B#B(�B$tB!�B#�B�B�B�B�BB�B�B�B�B�B�B
�BB�BDBDBBB�B%B%B%BYB{BB�B	�B
	B�B��B�.B�cB��B�(B�B�cB�B�B��B 4B��B  B��B��B�AB�B�|B�B��B�+B�vB�QB�8B�DB�yB�B�,B��B�B�B�B�B�]B�,B�TB�gB�sB��B��B�B�XB��B��B��B��BB��B��B�LB��B��B��B�hB��B�aB�9B�hB�!B�}B�B�}B�wB��B��B��B�=B��B��B��B��B��B��B��B�CB�xB��B��B��B��B�	B��B��B��B��B��B��B��B��B��B�SB��B�@B�@B��B�@B�MB�B� B��B��B�PB�DB�B�B��B��B�oB��B��BcB|PB}�BtBt�Bu%Bx�Bs�Bw2BqBpoBp;Bq�Bj�Bm�BiBo�Bb�BaHBd&Bd&Be`B`vB^jB\]BXyBWsBXyBXyBV�BRTBR�BQBM6BM6BK�BG�BFtB?�B?}B?�B=<B;�B?�B;0B9$B49B1�B/�B-wB,�B)�B.�B/B!bBB#B!�B	B
�B�B7B.�B�B
�B
�8B
�`B
�B
��B
�NB
�B
�jB
�B
��B
یB
�/B
�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022110507042720221105070427IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022111501010620221115010106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022111501010620221115010106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             SI  SI  ARFMARFM                                                                                                                                                2023021013194820230210131948IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                