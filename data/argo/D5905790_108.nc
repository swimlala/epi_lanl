CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-01T19:28:49Z creation; 2021-10-15T19:29:27Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � W|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20210401192849  20211015173718  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               l   lAA  AOAO7824_008764_108                 7824_008764_108                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�hkP��@�hkP��11  @�h��v�@�h��v�@7Rxl"h
@7Rxl"h
�d�^5?|��d�^5?|�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@@  @�  @�  @�G�@�  @��RA\)A   A,(�A@  AaG�A�  A�  A�  A��A��A�  A�Q�A�A��B�
B�B�B�B'�B0  B8Q�B@  BG�BO�
BX  B`(�Bh(�Bp(�Bx(�B�{B�{B�  B��B��B�  B�  B��B�  B�{B�{B�{B�  B��B�  B�  B�  B�  B��B�  B�  B�  B�  B�{B�{B�  B�  B��B��B��B�  B�  C   C
=C  C
=C
=C
{C
=C  C  C  C  C��C��C��C��C��C��C"  C$  C%��C(
=C*{C,
=C.  C0  C2
=C4
=C5��C8  C9��C;��C=��C@  CB  CD  CF  CH  CJ  CK��CN  CP
=CR
=CT
=CV
=CX
=CZ{C\
=C^  C_��Ca��Cd  Cf
=Ch  Cj  Cl  Cm��Cp  Cr  Ct  Cu��Cw��Cz  C|  C}��C��C���C���C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C���C���C���C���C�  C�  C�C���C���C�  C�C�  C�  C�  C���C���C���C�  C�
=C�C�  C���C�  C�  C�  C�  C�C�C�C�C�  C���C�  C�C�C���C���C�  C�  C���C�C�  C�  C�  C���C���C�  C�  C�  C�  C�C�  C�  C���C���C�C�  C�C�  C�C�
=C�  C���C�C�C�C�C�C�C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C���C�  C�C�C�C�  C�  C���C�  C�
=D   D }qD ��Dz�D�qD� D  D� D�D�D�D� D�D�D  D}qD�D��D	  D	��D
  D
� D  D��D  D� D  D� D  D}qD�qD}qD�qD� D  D}qD  D� D  D� D  D}qD  D� D�qD� D  D��D  D� D  D}qD  D� D  D}qD�qD}qD  D��D  D}qD�D� D   D ��D!  D!}qD"  D"� D#  D#��D$�D$��D%�D%��D&  D&}qD&�qD'� D(�D(��D)�D)��D*  D*� D*�qD+� D,  D,}qD,�qD-� D.  D.� D/  D/}qD/�qD0� D1�D1��D2  D2� D3�D3� D4  D4� D4�qD5� D6�D6� D6�qD7� D8  D8� D9�D9� D9��D:}qD:�qD;� D<�D<��D=�D=��D>�D>��D?  D?}qD@  D@}qD@�qDA� DB  DB� DB�qDC}qDC��DD}qDD�qDE� DF  DF� DG�DG� DG�qDH� DI  DI}qDI�qDJz�DJ��DK� DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ� DQ�qDR� DS  DS��DT�DT� DT�qDU� DV�DV� DV�qDW� DX  DX� DY�DY��DZ�DZ� D[  D[� D\  D\��D]�D]� D^  D^}qD^�qD_}qD_��D`}qDa  Da}qDb  Db��Db�qDc� Dd�Dd��De  De� Df  Df}qDg  Dg��Dh�Dh� Di  Di� Dj�Dj��Dk  Dk}qDk�qDl� Dm�Dm��Dn  Dn}qDo�Do��Dp  Dp}qDq  Dq� DrDr��Ds  Ds� Dt  Dt}qDu  Du��Dv  Dv� Dw�Dw� Dw�qDx}qDy  Dy��Dz  Dz� Dz�qD{}qD{�qD|}qD|�qD}� D~  D~� D�D��D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�� D��HD��D�@ D�� D�� D���D�>�D�� D��HD���D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�� D�� D�HD�>�D�}qD�� D�  D�@ D��HD�� D���D�=qD�� D��HD�  D�>�D�� D��HD��D�AHD�� D�� D�  D�@ D�� D��HD���D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D�~�D���D��qD�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD�� D���D���D�=qD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D��HD��D�@ D��HD�D�HD�@ D��HD�� D���D�@ D�� D���D���D�@ D�� D���D�  D�AHD�� D�� D���D�=qD�~�D�� D���D�>�D�~�D��HD�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�>�D�}qD�� D�  D�@ D��HD��HD�  D�B�D���D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D D�� D�  D�AHDÀ Dþ�D�  D�@ DĀ D�� D�  D�@ DŁHD��HD�HD�@ Dƀ D�� D�  D�>�Dǀ D�� D�  D�@ DȀ DȾ�D���D�@ D�~�D�� D���D�@ DʁHD��HD�HD�@ DˁHD��HD�HD�AHD́HD��HD���D�@ D̀ D��HD�  D�>�D΀ D��HD�HD�AHDρHD�� D�HD�@ D�~�D��HD�  D�@ Dр D�� D�HD�AHDҁHD�� D�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�  D�@ DցHD�� D���D�@ D׀ D׾�D�  D�@ D؀ D��HD�  D�>�Dـ D��HD�  D�@ Dڀ Dھ�D���D�@ Dۀ D�� D�  D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ Dހ D��HD�  D�@ D߁HD�� D���D�AHD���D��HD���D�>�D�HD�D�HD�AHD� D�� D���D�>�D�~�D㾸D�HD�@ D� D�� D�  D�>�D�~�D徸D���D�@ D悏D��HD�HD�@ D� D�� D�  D�AHD�HD辸D��qD�>�D� D�D��D�@ D�}qD꾸D���D�>�D�~�D뾸D�  D�@ D�HD�� D��qD�@ D� D���D���D�>�D� D�� D�  D�AHD� DﾸD�  D�AHD�� D�D�  D�@ D�~�D�� D���D�>�D�HD�
>��
>�?B�\?���?�p�?��@��@&ff@:�H@Q�@h��@�G�@��@�
=@��\@�\)@��H@�ff@�33@�p�@�@�z�@��RA�A
�HA��AffA�A!G�A'
=A.{A333A7�A<��AA�AG�AMp�AP��AU�AX��A]p�AaG�Ae�AhQ�Al��Ao\)As33AvffAy��A}p�A�Q�A���A�33A���A�ffA�  A��A��A�p�A�ffA�Q�A��A��
A�p�A�\)A���A��\A�z�A�{A�  A��A��A��A�
=A���A��HA���A��RA���A��\A���A��RA���A��\A�z�A��RA���A��HA���A�
=A�G�A�33A��A�\)A�G�A��
A�A׮A��A��
A�A߮A�=qA��
A�A�A陚A��
A�p�A�A�A��
A�A�  A�=qA�(�A�ffB Q�Bp�B�\B�B��BB�RB�B��B	�B
=B  B�B=qB33B(�BG�BffB�B��B��B�\B�B��BB�HB  B��B{B\)B Q�B!G�B"ffB#�B$��B%B&�HB(  B)G�B*=qB+\)B,��B-B.�HB0  B1�B2=qB3\)B4��B5B6�HB8  B9G�B:=qB;\)B<z�B=�B>�HB@  BA�BBffBC\)BDz�BE��BF�HBH  BI�BJ=qBK�BL��BMBN�HBP(�BQG�BR�\BS�BT��BU�BW
=BXQ�BY��BZ�\B[�B\��B^{B_33B`z�Ba��Bb�HBd  Be�Bf=qBg\)Bh��Bi��Bj�RBk�
Bl��Bn{Bo33Bpz�Bq��Br�RBs�
Bu�Bv{Bw33BxQ�By��Bz�RB{�
B|��B~{B33B�=qB���B�G�B��B��\B��B��B�=qB���B�\)B��B�z�B�
=B���B�(�B��RB�\)B��
B�ffB���B��B�{B��\B�33B�B�Q�B��HB��B�  B���B�G�B��
B�ffB���B��B�(�B��RB�\)B��B��\B��B��B�Q�B��HB�p�B�  B���B�33B�B�Q�B���B���B�(�B��RB�\)B�  B���B�33B�B�ffB�
=B���B�(�B���B�\)B�  B��\B�G�B��
B�ffB�
=B���B�=qB���B�p�B�{B��RB�G�B��
B�z�B��B��B�Q�B���B��B�{B��RB�\)B�  B��\B��B��B�ffB�
=B��B�=qB��RB�\)B��B�z�B�
=B���B�Q�B��HB�p�B�{B���B�33B��B�z�B�
=B��B�(�B���B�p�B��Bď\B��BŮB�=qB��HB�\)B�  B�z�B���Bə�B�(�Bʣ�B�G�B��
B�Q�B��HBͅB�{BΣ�B�33B�B�=qB��HB�\)B��Bҏ\B�
=Bә�B�(�Bԣ�B�G�B��
B�ffB�
=BׅB�(�BظRB�33B��
B�ffB��HBۅB�  B܏\B�33BݮB�=qB޸RB�\)B��B�z�B���B�B�{B�RB�33B�B�=qB���B�\)B��B�z�B���B癚B�{B�RB�G�B�B�ffB��HB�\)B�  B�\B���B홚B�{B��B�33B�B�Q�B��HB�p�B�{B�\B�33B�B�ffB��HB�p�B�{B��\B�33B��B�Q�B���B�\)B�  B�z�B���B��B�(�B��RB�G�B��
B�ffB�
=B��C {C ffC ��C �C=qC�CC{CffC�C  C=qC�\C�HC(�Cp�CC
=C\)C��C��C=qC�\C�
C{CffC�C��C=qC�C�
C	{C	\)C	��C	�HC
(�C
\)C
�\C
��C
��C(�C\)C�\C�RC�HC
=C33CffC�\C�RC�HC{C=qCp�C��CC�C{C=qCp�C��C��C��C(�C\)Cz�C�C�
C
=C(�CffC�\C�RC�C�CQ�Cp�C��C�HC{C=qCffC��C��C  C33C\)C��CC��C(�C\)C�\CC��C�C\)C�C�C�C(�CQ�C�C�RC�C�C\)C�\CC��C(�CffC��C��C  C33C\)C��C��C  C33Cp�C��C�
C
=C=qCp�C��C�
C{C=qCz�C�RC�HC(�CQ�C�CC��C(�C\)C��CC  C33CffC��C�
C {C G�C �C �RC ��C!(�C!\)C!��C!�
C"
=C"G�C"�C"�RC#  C#=qC#z�C#�RC#�C$�C$p�C$��C$�HC%�C%Q�C%�\C%��C&
=C&G�C&�C&�RC&��C'33C'ffC'��C'�HC({C(Q�C(�\C(��C)  C)G�C)p�C)�RC)��C*(�C*ffC*��C*�HC+�C+Q�C+�\C+��C,  C,=qC,�C,C-  C-33C-z�C-�RC-�C.(�C.ffC.�C.�C/(�C/ffC/��C/�C0�C0\)C0��C0��C1
=C1Q�C1�\C1C2
=C2Q�C2�C2C2��C3=qC3p�C3�RC3��C433C4p�C4�C4�C5(�C5ffC5��C5�
C6{C6Q�C6��C6�
C7
=C7G�C7�\C7C7��C8=qC8p�C8��C8�C9(�C9ffC9��C9�
C:{C:Q�C:�\C:��C;  C;G�C;z�C;�RC;��C<33C<p�C<��C<�C=(�C=ffC=��C=�HC>�C>\)C>��C>�
C?�C?\)C?��C?�HC@�C@\)C@��C@�
CA{CA\)CA�\CA�
CB
=CBQ�CB�\CB��CC
=CCG�CC�\CC��CD  CD=qCD�CDCE  CE=qCEp�CE�CE��CF=qCFp�CF�RCF��CG33CGp�CG�RCG��CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ?�=q@�\@@  @�  @�  @�G�@�  @��RA\)A   A,(�A@  AaG�A�  A�  A�  A��A��A�  A�Q�A�A��B�
B�B�B�B'�B0  B8Q�B@  BG�BO�
BX  B`(�Bh(�Bp(�Bx(�B�{B�{B�  B��B��B�  B�  B��B�  B�{B�{B�{B�  B��B�  B�  B�  B�  B��B�  B�  B�  B�  B�{B�{B�  B�  B��B��B��B�  B�  C   C
=C  C
=C
=C
{C
=C  C  C  C  C��C��C��C��C��C��C"  C$  C%��C(
=C*{C,
=C.  C0  C2
=C4
=C5��C8  C9��C;��C=��C@  CB  CD  CF  CH  CJ  CK��CN  CP
=CR
=CT
=CV
=CX
=CZ{C\
=C^  C_��Ca��Cd  Cf
=Ch  Cj  Cl  Cm��Cp  Cr  Ct  Cu��Cw��Cz  C|  C}��C��C���C���C���C�  C�  C���C�  C�C�C�C�C�  C�  C�  C�  C���C���C���C���C�  C�  C�C���C���C�  C�C�  C�  C�  C���C���C���C�  C�
=C�C�  C���C�  C�  C�  C�  C�C�C�C�C�  C���C�  C�C�C���C���C�  C�  C���C�C�  C�  C�  C���C���C�  C�  C�  C�  C�C�  C�  C���C���C�C�  C�C�  C�C�
=C�  C���C�C�C�C�C�C�C�  C�  C�  C�  C�C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C���C���C���C���C���C���C���C���C���C���C�  C�C�C�C���C�  C�C�C�C�  C�  C���C�  C�
=D   D }qD ��Dz�D�qD� D  D� D�D�D�D� D�D�D  D}qD�D��D	  D	��D
  D
� D  D��D  D� D  D� D  D}qD�qD}qD�qD� D  D}qD  D� D  D� D  D}qD  D� D�qD� D  D��D  D� D  D}qD  D� D  D}qD�qD}qD  D��D  D}qD�D� D   D ��D!  D!}qD"  D"� D#  D#��D$�D$��D%�D%��D&  D&}qD&�qD'� D(�D(��D)�D)��D*  D*� D*�qD+� D,  D,}qD,�qD-� D.  D.� D/  D/}qD/�qD0� D1�D1��D2  D2� D3�D3� D4  D4� D4�qD5� D6�D6� D6�qD7� D8  D8� D9�D9� D9��D:}qD:�qD;� D<�D<��D=�D=��D>�D>��D?  D?}qD@  D@}qD@�qDA� DB  DB� DB�qDC}qDC��DD}qDD�qDE� DF  DF� DG�DG� DG�qDH� DI  DI}qDI�qDJz�DJ��DK� DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ� DQ�qDR� DS  DS��DT�DT� DT�qDU� DV�DV� DV�qDW� DX  DX� DY�DY��DZ�DZ� D[  D[� D\  D\��D]�D]� D^  D^}qD^�qD_}qD_��D`}qDa  Da}qDb  Db��Db�qDc� Dd�Dd��De  De� Df  Df}qDg  Dg��Dh�Dh� Di  Di� Dj�Dj��Dk  Dk}qDk�qDl� Dm�Dm��Dn  Dn}qDo�Do��Dp  Dp}qDq  Dq� DrDr��Ds  Ds� Dt  Dt}qDu  Du��Dv  Dv� Dw�Dw� Dw�qDx}qDy  Dy��Dz  Dz� Dz�qD{}qD{�qD|}qD|�qD}� D~  D~� D�D��D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�>�D�� D��HD��D�@ D�� D�� D���D�>�D�� D��HD���D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�D���D���D�>�D��HD��HD�  D�>�D�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�  D�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�~�D���D���D�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D��qD�@ D��HD�� D�  D�@ D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�� D�� D�HD�>�D�}qD�� D�  D�@ D��HD�� D���D�=qD�� D��HD�  D�>�D�� D��HD��D�AHD�� D�� D�  D�@ D�� D��HD���D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D�~�D���D��qD�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD�� D���D���D�=qD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD���D���D�>�D�� D��HD�  D�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D��HD��D�@ D��HD�D�HD�@ D��HD�� D���D�@ D�� D���D���D�@ D�� D���D�  D�AHD�� D�� D���D�=qD�~�D�� D���D�>�D�~�D��HD�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D���D�  D�>�D�}qD�� D�  D�@ D��HD��HD�  D�B�D���D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D D�� D�  D�AHDÀ Dþ�D�  D�@ DĀ D�� D�  D�@ DŁHD��HD�HD�@ Dƀ D�� D�  D�>�Dǀ D�� D�  D�@ DȀ DȾ�D���D�@ D�~�D�� D���D�@ DʁHD��HD�HD�@ DˁHD��HD�HD�AHD́HD��HD���D�@ D̀ D��HD�  D�>�D΀ D��HD�HD�AHDρHD�� D�HD�@ D�~�D��HD�  D�@ Dр D�� D�HD�AHDҁHD�� D�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�>�D�~�D��HD�  D�@ DցHD�� D���D�@ D׀ D׾�D�  D�@ D؀ D��HD�  D�>�Dـ D��HD�  D�@ Dڀ Dھ�D���D�@ Dۀ D�� D�  D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ Dހ D��HD�  D�@ D߁HD�� D���D�AHD���D��HD���D�>�D�HD�D�HD�AHD� D�� D���D�>�D�~�D㾸D�HD�@ D� D�� D�  D�>�D�~�D徸D���D�@ D悏D��HD�HD�@ D� D�� D�  D�AHD�HD辸D��qD�>�D� D�D��D�@ D�}qD꾸D���D�>�D�~�D뾸D�  D�@ D�HD�� D��qD�@ D� D���D���D�>�D� D�� D�  D�AHD� DﾸD�  D�AHD�� D�D�  D�@ D�~�D�� D���D�>�D�HG�O�>��
>�?B�\?���?�p�?��@��@&ff@:�H@Q�@h��@�G�@��@�
=@��\@�\)@��H@�ff@�33@�p�@�@�z�@��RA�A
�HA��AffA�A!G�A'
=A.{A333A7�A<��AA�AG�AMp�AP��AU�AX��A]p�AaG�Ae�AhQ�Al��Ao\)As33AvffAy��A}p�A�Q�A���A�33A���A�ffA�  A��A��A�p�A�ffA�Q�A��A��
A�p�A�\)A���A��\A�z�A�{A�  A��A��A��A�
=A���A��HA���A��RA���A��\A���A��RA���A��\A�z�A��RA���A��HA���A�
=A�G�A�33A��A�\)A�G�A��
A�A׮A��A��
A�A߮A�=qA��
A�A�A陚A��
A�p�A�A�A��
A�A�  A�=qA�(�A�ffB Q�Bp�B�\B�B��BB�RB�B��B	�B
=B  B�B=qB33B(�BG�BffB�B��B��B�\B�B��BB�HB  B��B{B\)B Q�B!G�B"ffB#�B$��B%B&�HB(  B)G�B*=qB+\)B,��B-B.�HB0  B1�B2=qB3\)B4��B5B6�HB8  B9G�B:=qB;\)B<z�B=�B>�HB@  BA�BBffBC\)BDz�BE��BF�HBH  BI�BJ=qBK�BL��BMBN�HBP(�BQG�BR�\BS�BT��BU�BW
=BXQ�BY��BZ�\B[�B\��B^{B_33B`z�Ba��Bb�HBd  Be�Bf=qBg\)Bh��Bi��Bj�RBk�
Bl��Bn{Bo33Bpz�Bq��Br�RBs�
Bu�Bv{Bw33BxQ�By��Bz�RB{�
B|��B~{B33B�=qB���B�G�B��B��\B��B��B�=qB���B�\)B��B�z�B�
=B���B�(�B��RB�\)B��
B�ffB���B��B�{B��\B�33B�B�Q�B��HB��B�  B���B�G�B��
B�ffB���B��B�(�B��RB�\)B��B��\B��B��B�Q�B��HB�p�B�  B���B�33B�B�Q�B���B���B�(�B��RB�\)B�  B���B�33B�B�ffB�
=B���B�(�B���B�\)B�  B��\B�G�B��
B�ffB�
=B���B�=qB���B�p�B�{B��RB�G�B��
B�z�B��B��B�Q�B���B��B�{B��RB�\)B�  B��\B��B��B�ffB�
=B��B�=qB��RB�\)B��B�z�B�
=B���B�Q�B��HB�p�B�{B���B�33B��B�z�B�
=B��B�(�B���B�p�B��Bď\B��BŮB�=qB��HB�\)B�  B�z�B���Bə�B�(�Bʣ�B�G�B��
B�Q�B��HBͅB�{BΣ�B�33B�B�=qB��HB�\)B��Bҏ\B�
=Bә�B�(�Bԣ�B�G�B��
B�ffB�
=BׅB�(�BظRB�33B��
B�ffB��HBۅB�  B܏\B�33BݮB�=qB޸RB�\)B��B�z�B���B�B�{B�RB�33B�B�=qB���B�\)B��B�z�B���B癚B�{B�RB�G�B�B�ffB��HB�\)B�  B�\B���B홚B�{B��B�33B�B�Q�B��HB�p�B�{B�\B�33B�B�ffB��HB�p�B�{B��\B�33B��B�Q�B���B�\)B�  B�z�B���B��B�(�B��RB�G�B��
B�ffB�
=B��C {C ffC ��C �C=qC�CC{CffC�C  C=qC�\C�HC(�Cp�CC
=C\)C��C��C=qC�\C�
C{CffC�C��C=qC�C�
C	{C	\)C	��C	�HC
(�C
\)C
�\C
��C
��C(�C\)C�\C�RC�HC
=C33CffC�\C�RC�HC{C=qCp�C��CC�C{C=qCp�C��C��C��C(�C\)Cz�C�C�
C
=C(�CffC�\C�RC�C�CQ�Cp�C��C�HC{C=qCffC��C��C  C33C\)C��CC��C(�C\)C�\CC��C�C\)C�C�C�C(�CQ�C�C�RC�C�C\)C�\CC��C(�CffC��C��C  C33C\)C��C��C  C33Cp�C��C�
C
=C=qCp�C��C�
C{C=qCz�C�RC�HC(�CQ�C�CC��C(�C\)C��CC  C33CffC��C�
C {C G�C �C �RC ��C!(�C!\)C!��C!�
C"
=C"G�C"�C"�RC#  C#=qC#z�C#�RC#�C$�C$p�C$��C$�HC%�C%Q�C%�\C%��C&
=C&G�C&�C&�RC&��C'33C'ffC'��C'�HC({C(Q�C(�\C(��C)  C)G�C)p�C)�RC)��C*(�C*ffC*��C*�HC+�C+Q�C+�\C+��C,  C,=qC,�C,C-  C-33C-z�C-�RC-�C.(�C.ffC.�C.�C/(�C/ffC/��C/�C0�C0\)C0��C0��C1
=C1Q�C1�\C1C2
=C2Q�C2�C2C2��C3=qC3p�C3�RC3��C433C4p�C4�C4�C5(�C5ffC5��C5�
C6{C6Q�C6��C6�
C7
=C7G�C7�\C7C7��C8=qC8p�C8��C8�C9(�C9ffC9��C9�
C:{C:Q�C:�\C:��C;  C;G�C;z�C;�RC;��C<33C<p�C<��C<�C=(�C=ffC=��C=�HC>�C>\)C>��C>�
C?�C?\)C?��C?�HC@�C@\)C@��C@�
CA{CA\)CA�\CA�
CB
=CBQ�CB�\CB��CC
=CCG�CC�\CC��CD  CD=qCD�CDCE  CE=qCEp�CE�CE��CF=qCFp�CF�RCF��CG33CGp�CG�RCG��CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�O�A�Q�A�S�A�XA�K�A�C�A�G�A�I�A�Q�A�XA�O�A�I�A�K�A�K�A�Q�A�Q�A�`BA�ZA�ZA�ZA�`BA�bNA�hsA�hsA�dZA�`BA�bNA�hsA�XA�bA�?}A���A��`A�ȴA��^A��+A�p�A�bNA�S�A�oA���A�/A���A��DA�(�A��mA��TA��HA�A��PA�XA��A���A�M�A�oA�t�A��/A�A��!A��+A�VA��A�1A���A�33A��9A�;dA��A���A�&�A���A�ffA���A��A���A��`A���A���A�n�A�dZA�dZA�M�A�bA���A���A�7LA���A�1'A�A��DA�A��`A�9XA���A�l�A�33A���A��TA�5?A�{A��\A��;A��PA��A�/A���A�dZA��\A��A�
=A��DA�VA�ȴA�jA�{A|ĜA{�Az��Az��Az��Az��Ay�mAx-Awt�Av��Avn�Au��At�`As"�Aq�PAn�jAl{AihsAh�9Ag��AfVAf �Ae��Ad�\Aa��A`jA_O�A^M�A]O�A\~�A\�AZ �AX��AW�FAWoAV�!AV5?AU�AT�AR-AQ�APZAO\)AN��AM�TAJ��AI7LAH��AH�AF��AFQ�AF-AE��AD��ADJAB�9AA?}A@ĜA?��A>I�A<E�A;A:=qA9�-A9;dA8 �A7&�A5��A4��A3��A3p�A3?}A2jA1;dA/��A-`BA*M�A)%A(ȴA(�DA&�A%��A$��A#C�A!t�A ��A �AAK�AA�A"�AĜA��A�Ax�A�A�A+A��A/AA��A��A7LA�A��AffA�A�A �A�A33A9XA�7A
��A�TAA�A�mA/A�A�A��AG�A �yA �+A 9X@���@��y@�M�@��`@�
=@�G�@���@��;@�O�@�r�@�E�@�;d@�V@��@��@�P@�\)@�dZ@�K�@�v�@��@��@�^5@�$�@�$�@�^@��@�Z@߮@��#@�&�@�r�@��m@�C�@�v�@���@���@�ȴ@���@�Q�@��m@�\)@�{@�V@� �@�\)@��y@́@˥�@�n�@�`B@�bN@�(�@�|�@���@�O�@�I�@î@�C�@��@¸R@�v�@�=q@�@�7L@���@�Ĝ@�bN@�1@��P@�ȴ@�=q@�Z@��F@�+@��@�
=@���@�{@�J@��@���@�x�@�&�@���@�V@�O�@��@�Ĝ@��D@�
=@�$�@��9@���@�o@��+@�v�@�l�@��@��+@�^5@�=q@���@���@�@�ff@�S�@�+@��y@��+@��@��@�7L@��j@�bN@�A�@���@�
=@���@�{@���@�V@��u@��
@�
=@���@��@���@�hs@�j@���@��w@��@�dZ@��@�=q@�%@��j@���@�I�@�1@��m@��@�C�@���@��H@���@���@���@��+@�n�@�ff@�E�@��T@���@��@�7L@��`@�r�@��F@��H@�ff@���@�?}@�hs@��@�x�@�X@��@�t�@��\@�=q@���@�x�@�p�@��@���@���@���@��\@��\@�$�@���@�hs@�V@��`@�V@��@���@��`@���@�\)@�5?@���@��9@�I�@� �@��w@�t�@�l�@���@���@��-@�p�@�7L@�&�@��@��@���@��j@���@��D@�Q�@�1'@�1@��@��F@�dZ@�@���@�$�@�{@���@��@��@��-@�?}@�%@���@�z�@�9X@�b@��@�ƨ@��F@�t�@�C�@�
=@��@�ȴ@�5?@��@���@��-@��@�&�@���@��@�9X@�ƨ@��P@��@�l�@��H@���@�^5@�E�@�E�@�E�@��@��@���@�`B@�7L@��@��@�%@��`@���@��@�r�@�j@�bN@�Q�@�1'@� �@�w@~��@~ȴ@~�@~��@~�y@~�R@~v�@~E�@~@}�@}�-@}?}@|�@|�j@|�@|�D@{ƨ@{"�@z�@yG�@x��@x�9@x�u@xr�@x1'@xb@w�@w�P@w|�@w;d@v�@v��@vV@u�@t�@tZ@tI�@t9X@t�@s��@s��@st�@sS�@sS�@s33@so@r~�@r�\@r�\@r�@q�#@q�7@qX@q&�@q%@pbN@pb@p  @o�@ol�@oK�@nȴ@m`B@mV@l�/@l�j@l�@kƨ@kC�@k33@jM�@i��@iX@hr�@g�w@g|�@f��@fff@fV@fE�@f$�@e�T@e��@e��@eO�@d��@d(�@c�@c33@b�@b��@bn�@aX@`Ĝ@`1'@_��@_�@_|�@_;d@_
=@^�@^�R@^��@^5?@^{@]�T@]�@]/@\(�@Z��@Z=q@Z�@Y�@Y��@YG�@XĜ@W�@W\)@V�@V��@V�+@V�+@V{@U��@UO�@T�@TZ@T�@T1@S�m@Sƨ@S��@St�@SC�@R�@Q��@Q%@P�@O�P@O|�@O��@O�@O�w@O��@O|�@N��@N��@N{@M��@M�-@M��@M�-@M�-@M�-@M�-@M��@M�h@M�@M�@Mp�@M�@K�m@K"�@J��@I��@I&�@H�`@HbN@G|�@Fȴ@Fv�@F{@E�@E��@E@E�h@Ep�@E`B@E?}@EV@D�@D�@D9X@C�m@C�F@C�@Ct�@CdZ@CdZ@C33@B�H@B�H@B^5@A&�@@A�@?�P@?l�@?K�@?
=@>�+@>ff@>$�@>@=�@=��@=`B@<��@<�D@<�@;t�@;o@:�H@:��@:�!@:=q@9�@9�^@9��@9G�@8�9@8r�@8Q�@8Q�@8Q�@81'@7��@7�@7�@7|�@7K�@6�y@6�R@6��@6v�@6E�@65?@6@5�@5��@5�-@5�-@5�-@5�h@5`B@5/@4��@4�/@4�/@4��@4�/@4��@4��@4�j@4I�@3t�@3"�@3o@3o@3o@3@2�H@2��@2��@2�@2J@1�@1�^@1�7@1X@1�@1%@0��@0 �@/�w@/�@/��@/��@/|�@/K�@/+@/
=@/
=@.ȴ@.$�@.@.@-@-`B@-/@,��@,�@,��@,z�@+�
@+��@+dZ@+C�@+S�@+S�@+C�@+C�@+33@+o@*�!@*��@*�\@*�@)�^@)��@)7L@(�`@(�u@(r�@(Q�@( �@'|�@';d@'
=@&�+@&$�@%@%�@%O�@%/@$��@$��@$�j@$j@#��@#�
@#ƨ@#ƨ@#��@#��@#�@#S�@#o@"�!@"-@"J@!��@!�@!�^@!&�@ ��@ Ĝ@ Ĝ@ ��@ ��@ �9@ �9@ ��@ �9@ ��@ �u@ �@ A�@�@�@�P@K�@K�@��@�+@v�@E�@�@@��@p�@��@�D@I�@(�@�@��@�
@�
@�
@�F@��@��@�@t�@C�@"�@o@@@�H@�H@�H@��@�!@�\@~�@^5@�#@��@x�@X@%@Ĝ@A�@b@�;@��@|�@\)@;d@
=@
=@�y@��@�+@ff@V@V@E�@5?@@��@�h@`B@/@/@V@V@�@�@�/@�@��@t�@S�@S�@C�@C�@33@"�@@��@��@M�@-@��@��@hs@7L@&�@��@�9@�@r�@A�@1'@b@�A�Q�A�9XA�K�A�S�A�Q�A�K�A�Q�A�VA�M�A�Q�A�S�A�O�A�\)A�^5A�ZA�Q�A�K�A�I�A�E�A�?}A�C�A�I�A�G�A�E�A�K�A�E�A�K�A�Q�A�Q�A�XA�XA�Q�A�S�A�O�A�K�A�O�A�Q�A�K�A�K�A�M�A�K�A�G�A�I�A�G�A�I�A�K�A�M�A�K�A�G�A�I�A�I�A�K�A�Q�A�K�A�K�A�I�A�I�A�K�A�K�A�I�A�G�A�M�A�O�A�I�A�O�A�Q�A�S�A�S�A�XA�K�A�O�A�Q�A�K�A�M�A�K�A�K�A�I�A�Q�A�XA�VA�VA�XA�ZA�\)A�`BA�`BA�^5A�^5A�bNA�bNA�^5A�^5A�\)A�\)A�`BA�ZA�O�A�Q�A�Q�A�Q�A�ZA�ZA�ZA�`BA�dZA�bNA�S�A�ZA�XA�ZA�^5A�^5A�ZA�bNA�^5A�`BA�bNA�`BA�dZA�dZA�^5A�ZA�ZA�bNA�hsA�ffA�hsA�hsA�dZA�hsA�hsA�ffA�jA�jA�ffA�hsA�jA�hsA�ffA�jA�hsA�hsA�jA�l�A�jA�ffA�`BA�ffA�`BA�^5A�bNA�dZA�bNA�`BA�bNA�dZA�`BA�^5A�^5A�bNA�dZA�bNA�dZA�dZA�ffA�bNA�ffA�hsA�ffA�jA�jA�hsA�bNA�ZA�XA�XA�XA�VA�I�A�33A�+A��A��A��A��A���A�x�A�hsA�5?A�C�A�"�A�{A�bA�bA�A���A���A���A��A��A��A��A��A��mA��/A���A���A���A���A�ȴA�ƨA�A��wA�A���A��wA�A�A��RA��9A���A���A��uA��DA�|�A�~�A�|�A�v�A�x�A�t�A�r�A�r�A�l�A�jA�jA�jA�bNA�dZA�`BA�`BA�bNA�\)A�\)A�ZA�VA�VA�Q�A�G�A�?}A�/A��A�oA�VA�A���A���A��A��!A���A��A�l�A�ffA�S�A�E�A�;dA�5?A�+A�$�A�"�A��A�VA�VA�
=A��A��mA��/A�ƨA��A���A��\A�~�A�n�A�dZA�ZA�K�A�/A�+A� �A�  A���A��A��A��yA��`A��HA��HA��HA��;A��HA��TA��`A��TA��TA��`A��yA��yA��mA��;A��A���A���A���A���A�A��jA��FA��A���A��PA��+A��+A��A�t�A�n�A�`BA�ZA�VA�O�A�I�A�G�A�7LA�5?A�+A�VA�1A�A���A��mA��FA���A�p�A�hsA�`BA�ZA�\)A�S�A�K�A�I�A�C�A�=qA�7LA�5?A�-A�-A�oA��TA���A��!A��hA��A�z�A�jA�S�A�S�A�C�A�(�A��A��TA���A���A��\A��7A��+A�^5A�-A��A���A�ffA�33A��A�1A��mA��-A�n�A�I�A�bA���A��/A��9A���A�n�A�=qA��A��\A�=qA��A�1A�A��mA���A��A��+A�hsA�VA�$�A���A���A�hsA�S�A�VA�\)A�=qA�{A�v�A�oA���A���A��!A���A��\A�~�A�~�A�n�A�`BA�\)A�7LA�1'A�(�A��/A���A�l�A�`BA��A�dZA�oA��!A��PA�jA�S�A�33A�+A��A��A�%A��mA���A���A�^5A���A�  A��A��A�jA�C�A���A��-A�`BA�oA��A�K�A�(�A��A���A���A��RA���A�~�A�M�A�~�A�A���A�A���A�dZA�O�A�E�A�C�A�E�A�A�A�A�A�=qA�7LA���A��FA�|�A�;dA��;A�~�A��A�^5A�r�A��+A�oA��wA�K�A�+A��A��9A���A�z�A�p�A�dZA�ffA�\)A�A�A�?}A�+A� �A��A�
=A���A��A��A��jA���A��7A�Q�A�E�A��A��A��TA�ȴA���A��FA��A���A��PA�5?A��A���A���A���A��hA��DA�~�A�z�A�|�A�v�A�x�A�t�A�r�A�r�A�p�A�n�A�n�A�hsA�jA�n�A�jA�dZA�hsA�dZA�bNA�dZA�ffA�dZA�dZA�dZA�dZA�bNA�dZA�ffA�bNA�dZA�dZA�bNA�dZA�bNA�`BA�ZA�O�A�VA�K�A�K�A�I�A�E�A�C�A�A�A�33A�33A�+A�&�A��A�bA���A��A��yA��/A��
A�ȴA�A��^A��!A���A���A�n�A�G�A�-A�  A��yA��A���A���A���A�A��A�XA� �A��TA�|�A�&�A��A���A�ĜA��FA���A���A�?}A���A���A�v�A�`BA�K�A�E�A�;dA�;dA�33A�5?A�1'A�-A�+A�+A�"�A� �A��A�oA�
=A�  A�  A���A��A��
A���A��A��A��A�M�A��
A���A�~�A�G�A�{A���A��A�ƨA���A��A�dZA�M�A�9XA�(�A�A��yA���A��FA���A�t�A�Q�A�C�A�?}A�A�A�?}A�9XA�-A��A���A��A��mA��
A�ƨA���A��FA��!A���A���A��+A�p�A�jA�hsA�XA�I�A�E�A�=qA�;dA�5?A�5?A�-A�-A�+A�+A�$�A�
=A��A��HA���A���A��RA���A�t�A�Q�A�?}A�&�A�JA��
A���A��A�XA�/A���A���A�+A���A��/A���A��FA��A��A���A�dZA�?}A�oA��-A�t�A��A��`A���A���A��uA��A�v�A�`BA�9XA��A���A��`A��#A���A���A���A���A���A�ȴA�ĜA���A��A�v�A�ffA�^5A�O�A�I�A�A�A�=qA�33A�-A�bA��
A���A���A�z�A�\)A�5?A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               A�M�A�O�A�Q�A�S�A�XA�K�A�C�A�G�A�I�A�Q�A�XA�O�A�I�A�K�A�K�A�Q�A�Q�A�`BA�ZA�ZA�ZA�`BA�bNA�hsA�hsA�dZA�`BA�bNA�hsA�XA�bA�?}A���A��`A�ȴA��^A��+A�p�A�bNA�S�A�oA���A�/A���A��DA�(�A��mA��TA��HA�A��PA�XA��A���A�M�A�oA�t�A��/A�A��!A��+A�VA��A�1A���A�33A��9A�;dA��A���A�&�A���A�ffA���A��A���A��`A���A���A�n�A�dZA�dZA�M�A�bA���A���A�7LA���A�1'A�A��DA�A��`A�9XA���A�l�A�33A���A��TA�5?A�{A��\A��;A��PA��A�/A���A�dZA��\A��A�
=A��DA�VA�ȴA�jA�{A|ĜA{�Az��Az��Az��Az��Ay�mAx-Awt�Av��Avn�Au��At�`As"�Aq�PAn�jAl{AihsAh�9Ag��AfVAf �Ae��Ad�\Aa��A`jA_O�A^M�A]O�A\~�A\�AZ �AX��AW�FAWoAV�!AV5?AU�AT�AR-AQ�APZAO\)AN��AM�TAJ��AI7LAH��AH�AF��AFQ�AF-AE��AD��ADJAB�9AA?}A@ĜA?��A>I�A<E�A;A:=qA9�-A9;dA8 �A7&�A5��A4��A3��A3p�A3?}A2jA1;dA/��A-`BA*M�A)%A(ȴA(�DA&�A%��A$��A#C�A!t�A ��A �AAK�AA�A"�AĜA��A�Ax�A�A�A+A��A/AA��A��A7LA�A��AffA�A�A �A�A33A9XA�7A
��A�TAA�A�mA/A�A�A��AG�A �yA �+A 9X@���@��y@�M�@��`@�
=@�G�@���@��;@�O�@�r�@�E�@�;d@�V@��@��@�P@�\)@�dZ@�K�@�v�@��@��@�^5@�$�@�$�@�^@��@�Z@߮@��#@�&�@�r�@��m@�C�@�v�@���@���@�ȴ@���@�Q�@��m@�\)@�{@�V@� �@�\)@��y@́@˥�@�n�@�`B@�bN@�(�@�|�@���@�O�@�I�@î@�C�@��@¸R@�v�@�=q@�@�7L@���@�Ĝ@�bN@�1@��P@�ȴ@�=q@�Z@��F@�+@��@�
=@���@�{@�J@��@���@�x�@�&�@���@�V@�O�@��@�Ĝ@��D@�
=@�$�@��9@���@�o@��+@�v�@�l�@��@��+@�^5@�=q@���@���@�@�ff@�S�@�+@��y@��+@��@��@�7L@��j@�bN@�A�@���@�
=@���@�{@���@�V@��u@��
@�
=@���@��@���@�hs@�j@���@��w@��@�dZ@��@�=q@�%@��j@���@�I�@�1@��m@��@�C�@���@��H@���@���@���@��+@�n�@�ff@�E�@��T@���@��@�7L@��`@�r�@��F@��H@�ff@���@�?}@�hs@��@�x�@�X@��@�t�@��\@�=q@���@�x�@�p�@��@���@���@���@��\@��\@�$�@���@�hs@�V@��`@�V@��@���@��`@���@�\)@�5?@���@��9@�I�@� �@��w@�t�@�l�@���@���@��-@�p�@�7L@�&�@��@��@���@��j@���@��D@�Q�@�1'@�1@��@��F@�dZ@�@���@�$�@�{@���@��@��@��-@�?}@�%@���@�z�@�9X@�b@��@�ƨ@��F@�t�@�C�@�
=@��@�ȴ@�5?@��@���@��-@��@�&�@���@��@�9X@�ƨ@��P@��@�l�@��H@���@�^5@�E�@�E�@�E�@��@��@���@�`B@�7L@��@��@�%@��`@���@��@�r�@�j@�bN@�Q�@�1'@� �@�w@~��@~ȴ@~�@~��@~�y@~�R@~v�@~E�@~@}�@}�-@}?}@|�@|�j@|�@|�D@{ƨ@{"�@z�@yG�@x��@x�9@x�u@xr�@x1'@xb@w�@w�P@w|�@w;d@v�@v��@vV@u�@t�@tZ@tI�@t9X@t�@s��@s��@st�@sS�@sS�@s33@so@r~�@r�\@r�\@r�@q�#@q�7@qX@q&�@q%@pbN@pb@p  @o�@ol�@oK�@nȴ@m`B@mV@l�/@l�j@l�@kƨ@kC�@k33@jM�@i��@iX@hr�@g�w@g|�@f��@fff@fV@fE�@f$�@e�T@e��@e��@eO�@d��@d(�@c�@c33@b�@b��@bn�@aX@`Ĝ@`1'@_��@_�@_|�@_;d@_
=@^�@^�R@^��@^5?@^{@]�T@]�@]/@\(�@Z��@Z=q@Z�@Y�@Y��@YG�@XĜ@W�@W\)@V�@V��@V�+@V�+@V{@U��@UO�@T�@TZ@T�@T1@S�m@Sƨ@S��@St�@SC�@R�@Q��@Q%@P�@O�P@O|�@O��@O�@O�w@O��@O|�@N��@N��@N{@M��@M�-@M��@M�-@M�-@M�-@M�-@M��@M�h@M�@M�@Mp�@M�@K�m@K"�@J��@I��@I&�@H�`@HbN@G|�@Fȴ@Fv�@F{@E�@E��@E@E�h@Ep�@E`B@E?}@EV@D�@D�@D9X@C�m@C�F@C�@Ct�@CdZ@CdZ@C33@B�H@B�H@B^5@A&�@@A�@?�P@?l�@?K�@?
=@>�+@>ff@>$�@>@=�@=��@=`B@<��@<�D@<�@;t�@;o@:�H@:��@:�!@:=q@9�@9�^@9��@9G�@8�9@8r�@8Q�@8Q�@8Q�@81'@7��@7�@7�@7|�@7K�@6�y@6�R@6��@6v�@6E�@65?@6@5�@5��@5�-@5�-@5�-@5�h@5`B@5/@4��@4�/@4�/@4��@4�/@4��@4��@4�j@4I�@3t�@3"�@3o@3o@3o@3@2�H@2��@2��@2�@2J@1�@1�^@1�7@1X@1�@1%@0��@0 �@/�w@/�@/��@/��@/|�@/K�@/+@/
=@/
=@.ȴ@.$�@.@.@-@-`B@-/@,��@,�@,��@,z�@+�
@+��@+dZ@+C�@+S�@+S�@+C�@+C�@+33@+o@*�!@*��@*�\@*�@)�^@)��@)7L@(�`@(�u@(r�@(Q�@( �@'|�@';d@'
=@&�+@&$�@%@%�@%O�@%/@$��@$��@$�j@$j@#��@#�
@#ƨ@#ƨ@#��@#��@#�@#S�@#o@"�!@"-@"J@!��@!�@!�^@!&�@ ��@ Ĝ@ Ĝ@ ��@ ��@ �9@ �9@ ��@ �9@ ��@ �u@ �@ A�@�@�@�P@K�@K�@��@�+@v�@E�@�@@��@p�@��@�D@I�@(�@�@��@�
@�
@�
@�F@��@��@�@t�@C�@"�@o@@@�H@�H@�H@��@�!@�\@~�@^5@�#@��@x�@X@%@Ĝ@A�@b@�;@��@|�@\)@;d@
=@
=@�y@��@�+@ff@V@V@E�@5?@@��@�h@`B@/@/@V@V@�@�@�/@�@��@t�@S�@S�@C�@C�@33@"�@@��@��@M�@-@��@��@hs@7L@&�@��@�9@�@r�@A�@1'@bG�O�A�Q�A�9XA�K�A�S�A�Q�A�K�A�Q�A�VA�M�A�Q�A�S�A�O�A�\)A�^5A�ZA�Q�A�K�A�I�A�E�A�?}A�C�A�I�A�G�A�E�A�K�A�E�A�K�A�Q�A�Q�A�XA�XA�Q�A�S�A�O�A�K�A�O�A�Q�A�K�A�K�A�M�A�K�A�G�A�I�A�G�A�I�A�K�A�M�A�K�A�G�A�I�A�I�A�K�A�Q�A�K�A�K�A�I�A�I�A�K�A�K�A�I�A�G�A�M�A�O�A�I�A�O�A�Q�A�S�A�S�A�XA�K�A�O�A�Q�A�K�A�M�A�K�A�K�A�I�A�Q�A�XA�VA�VA�XA�ZA�\)A�`BA�`BA�^5A�^5A�bNA�bNA�^5A�^5A�\)A�\)A�`BA�ZA�O�A�Q�A�Q�A�Q�A�ZA�ZA�ZA�`BA�dZA�bNA�S�A�ZA�XA�ZA�^5A�^5A�ZA�bNA�^5A�`BA�bNA�`BA�dZA�dZA�^5A�ZA�ZA�bNA�hsA�ffA�hsA�hsA�dZA�hsA�hsA�ffA�jA�jA�ffA�hsA�jA�hsA�ffA�jA�hsA�hsA�jA�l�A�jA�ffA�`BA�ffA�`BA�^5A�bNA�dZA�bNA�`BA�bNA�dZA�`BA�^5A�^5A�bNA�dZA�bNA�dZA�dZA�ffA�bNA�ffA�hsA�ffA�jA�jA�hsA�bNA�ZA�XA�XA�XA�VA�I�A�33A�+A��A��A��A��A���A�x�A�hsA�5?A�C�A�"�A�{A�bA�bA�A���A���A���A��A��A��A��A��A��mA��/A���A���A���A���A�ȴA�ƨA�A��wA�A���A��wA�A�A��RA��9A���A���A��uA��DA�|�A�~�A�|�A�v�A�x�A�t�A�r�A�r�A�l�A�jA�jA�jA�bNA�dZA�`BA�`BA�bNA�\)A�\)A�ZA�VA�VA�Q�A�G�A�?}A�/A��A�oA�VA�A���A���A��A��!A���A��A�l�A�ffA�S�A�E�A�;dA�5?A�+A�$�A�"�A��A�VA�VA�
=A��A��mA��/A�ƨA��A���A��\A�~�A�n�A�dZA�ZA�K�A�/A�+A� �A�  A���A��A��A��yA��`A��HA��HA��HA��;A��HA��TA��`A��TA��TA��`A��yA��yA��mA��;A��A���A���A���A���A�A��jA��FA��A���A��PA��+A��+A��A�t�A�n�A�`BA�ZA�VA�O�A�I�A�G�A�7LA�5?A�+A�VA�1A�A���A��mA��FA���A�p�A�hsA�`BA�ZA�\)A�S�A�K�A�I�A�C�A�=qA�7LA�5?A�-A�-A�oA��TA���A��!A��hA��A�z�A�jA�S�A�S�A�C�A�(�A��A��TA���A���A��\A��7A��+A�^5A�-A��A���A�ffA�33A��A�1A��mA��-A�n�A�I�A�bA���A��/A��9A���A�n�A�=qA��A��\A�=qA��A�1A�A��mA���A��A��+A�hsA�VA�$�A���A���A�hsA�S�A�VA�\)A�=qA�{A�v�A�oA���A���A��!A���A��\A�~�A�~�A�n�A�`BA�\)A�7LA�1'A�(�A��/A���A�l�A�`BA��A�dZA�oA��!A��PA�jA�S�A�33A�+A��A��A�%A��mA���A���A�^5A���A�  A��A��A�jA�C�A���A��-A�`BA�oA��A�K�A�(�A��A���A���A��RA���A�~�A�M�A�~�A�A���A�A���A�dZA�O�A�E�A�C�A�E�A�A�A�A�A�=qA�7LA���A��FA�|�A�;dA��;A�~�A��A�^5A�r�A��+A�oA��wA�K�A�+A��A��9A���A�z�A�p�A�dZA�ffA�\)A�A�A�?}A�+A� �A��A�
=A���A��A��A��jA���A��7A�Q�A�E�A��A��A��TA�ȴA���A��FA��A���A��PA�5?A��A���A���A���A��hA��DA�~�A�z�A�|�A�v�A�x�A�t�A�r�A�r�A�p�A�n�A�n�A�hsA�jA�n�A�jA�dZA�hsA�dZA�bNA�dZA�ffA�dZA�dZA�dZA�dZA�bNA�dZA�ffA�bNA�dZA�dZA�bNA�dZA�bNA�`BA�ZA�O�A�VA�K�A�K�A�I�A�E�A�C�A�A�A�33A�33A�+A�&�A��A�bA���A��A��yA��/A��
A�ȴA�A��^A��!A���A���A�n�A�G�A�-A�  A��yA��A���A���A���A�A��A�XA� �A��TA�|�A�&�A��A���A�ĜA��FA���A���A�?}A���A���A�v�A�`BA�K�A�E�A�;dA�;dA�33A�5?A�1'A�-A�+A�+A�"�A� �A��A�oA�
=A�  A�  A���A��A��
A���A��A��A��A�M�A��
A���A�~�A�G�A�{A���A��A�ƨA���A��A�dZA�M�A�9XA�(�A�A��yA���A��FA���A�t�A�Q�A�C�A�?}A�A�A�?}A�9XA�-A��A���A��A��mA��
A�ƨA���A��FA��!A���A���A��+A�p�A�jA�hsA�XA�I�A�E�A�=qA�;dA�5?A�5?A�-A�-A�+A�+A�$�A�
=A��A��HA���A���A��RA���A�t�A�Q�A�?}A�&�A�JA��
A���A��A�XA�/A���A���A�+A���A��/A���A��FA��A��A���A�dZA�?}A�oA��-A�t�A��A��`A���A���A��uA��A�v�A�`BA�9XA��A���A��`A��#A���A���A���A���A���A�ȴA�ĜA���A��A�v�A�ffA�^5A�O�A�I�A�A�A�=qA�33A�-A�bA��
A���A���A�z�A�\)A�5?A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�{B��B�SB��B��B��B��B��B�B��B�B�SB�{B��B�uB��B��B��B��B�AB�uB�AB�B�B�B�AB�B��B��B�B�=B�\B�B��B��B��B��B��B�B�B�B�dB�,BخB��B��B�AB�TB�fB��B��B�B�B��B�xB��B�B�	B�B�|B�QB��B�LB��B��B�'B�RB�kB��B{Bv�B�eB��B�B��B<�B'�B1BB.B�BbB B4B.B
�B��B��B��B��B��B��B�}B��B��B�B��B��B�SBt�Bg8BT�BI�BE9B?B4�B*�B&�B~B�B�B�B 4B�B�mB�B҉BŢBB� B��B�}B�jB�!B��B��B�nB�B��B��B��Bk�B_pBH�B>�B8�B1[B-�B+6B'�B	B�B
	B�B
�PB
��B
�GB
�B
�B
��B
�B
ޞB
�B
�
B
ԕB
ȴB
�zB
��B
�B
�tB
��B
�B
��B
��B
�B
��B
�JB
�	B
�7B
��B
�B
�B
v`B
qvB
lWB
ffB
^�B
YB
S&B
OB
L0B
GzB
A�B
;dB
7�B
5?B
2aB
0!B
.�B
)�B
%�B
~B
�B

=B
�B
�B	��B	��B	��B	�B	�>B	��B	�NB	�pB	��B	ٴB	бB	�BB	��B	ÖB	��B	��B	��B	�RB	�LB	��B	�*B	�B	��B	��B	�tB	��B	�aB	��B	�IB	��B	�*B	��B	�zB	�hB	��B	��B	��B	�B	��B	�$B	��B	��B	�hB	�hB	��B	��B	��B	��B	��B	�\B	��B	��B	��B	�PB	��B	�B	��B	�bB	��B	�VB	�oB	��B	��B	�oB	�:B	�$B	�FB	��B	�~B	�xB	�xB	�B	��B	�~B	��B	��B	�tB	�LB	��B	��B	�_B	�CB	�B	�[B	��B	��B	�B	�LB	��B	�B	�6B	�6B	��B	�}B	�}B	�wB	�B	�<B	�BB	��B	��B	��B	�UB	�'B	�-B	�3B	ŢB	ƨB	�B	˒B	͟B	͟B	��B	�}B	� B	�aB	֡B	��B	��B	��B	�B	�B	��B	�|B	��B	�B	��B	�`B	��B	�8B	��B	�B	�iB	��B	��B	�B	�B	�lB	�lB	�8B	��B	��B
uB
�B
kB
�B
kB
~B
B
!B
%zB
-B
7B
=�B
C�B
GEB
I�B
I�B
JXB
L0B
R�B
T�B
U�B
W?B
W?B
Y�B
]�B
aB
c�B
g�B
j�B
k�B
o B
poB
poB
r�B
sB
sB
sB
tTB
wfB
|�B
�4B
��B
��B
��B
�YB
��B
��B
�\B
�oB
�@B
��B
�MB
�B
��B
�YB
��B
�_B
�B
��B
�\B
��B
�zB
��B
��B
��B
��B
��B
�'B
��B
�B
��B
��B
��B
�$B
��B
�^B
��B
�RB
��B
�*B
�B
��B
�OB
�B
��B
ǮB
ǮB
ƨB
�?B
ƨB
ȀB
��B
��B
�<B
�B
�NB
ҽB
�?B
��B
�B
�B
�B
�yB
�B
��B
�pB
�pB
��B
�TB
�B
�ZB
��B
�B
�8B
��B
�B
�KB
�B
�QB
�B
��B
�B
�B
�TB
�8B
��B
�DB
�B
�B
�VB
��B
�.BB�B�B�B�B1BfB~B�B�B B�BBBSB$B+B�B�BB!B!�B#nB$B$tB'�B(XB)�B)�B)�B)�B*eB,qB.}B/�B0UB0�B0�B0�B1'B33B33B3�B3hB3�B3�B49B4nB4nB49B4nB5B5�B6B6FB7�B8�B9XB9XB9$B9�B:*B:�B;�B<B=�B>�B@�BA BA�BB�BB�BB�BC�BD3BD3BEmBEmBE9BE9BD�BE9BE9BG�BG�BGEBGzBG�BHKBI�BIRBH�BH�BH�BIBJ�BI�BI�BJ�BJ�BK�BK�BK�BK�BL�BL�BL�BM�BMBM6BM6BPBPHBQ�BQ�BS&BR�BRTBQ�BR�BR�BS&BW�BYKBZQB\]B\�B\�B\�B\]B]/B]�B]�B^jB_B_�BaBaHBa�Ba|Ba�BdZBd�Bf2BffBf�BgBg�Bh
Bh�BhsBh�Bi�Bi�BiDBiDBjBlWBm)Bl"Bm�Bm�Bn/Bn/BoiBo�Bp;Bp�Bq�Bq�BqABr|Bs�Br�Br�BrGBr�BrGBrGBr�Bs�BtBs�Bs�Bv�Bv`Bx�Bx�Bx�BzxB{B|B|�B|�B}VB|�B|�B|PB}"B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B~(B~]B}�B~]BcBcB~�B.B�BcB�B�iB��B�iB�4B��B�B��B�;B�oB�oB�B�AB��B��B�B��B��B�uB��B��B�AB��B�{B�MB��B�MB�MB��B��B�B�B��B�B��B��B��B�_B��B��B�fB�B��B��B��B��B��B�B��B�DB��B�rB��B��B�rB�JB�xB��B��B�JB��B�PB�VB��B�.B��B�bB��B��B��B�bB�.B�bB� B�4B�hB� B��B�4B�hB�B�hB�hB��B��B��B��B��B�MB��B��B��B�SB�+B��B��B��B��B��B��B�1B��B�7B�7B�kB�kB�kB��B�	B�	B�	B�kB��B�	B��B��B�qB��B��B�qB�CB�qB�xB��B��B�B�~B��B��B�OB�B��B��B�VB�!B��B��B�VB��B��B�-B��B��B�-B��B�4B��B�4B�nB��B��B�tB��B�tB��B�B��B�FB��B�tB�B�B�@B�B�B��B��B��B�@B�@B�@B�@B��B�zB��B��B��B��B��B�B�LB��B��B�B��B��B��B��B��B��B��B��B�eB��B��B��B��B�6B�kB�B�=B�B��B��B��B��B�CB�B��B��B�wB�wB�IB��B�IB�}B��B��B��B�B�B�B��B�B��B��B��B��B�UB��B��B��B�[B��B��B�aB�aB�-B��B�-B�aB��B�hB��B�hB��B�B�9B�B�9B�9B��B�B�?B�tB�?B�FB��B�zB�B�B��B��B��B��B��B�B��B��B�RB��B�RB�XB��B��B�*B�*B��B�0B�0B��B�B��B��B��B�B�6B��B~(B�.B�SB��B��B��B��B��B��B��B�%B��B�oB��B��B��B��B��B��B�%B�YB��B�SB�YB��B��B��B��B�MB�B�B��B��B�SB�YB��B��B��B�YB��B��B��B�B��B�B��B��B�B��B�AB��B��B��B��B��B��B��B��B�B�AB�B�;B��B��B�;B�B�B��B��B��B�AB��B�{B�{B��B�B�GB��B�iB��B�uB�B~�B��B��B��B�GB�uB�oB��B�GB��B��B�oB��B��B�{B�{B�GB��B�iB�uB�AB��B�oB��B�{B�;B�B�uB��B��B�{B�;B�uB��B�B��B�B�uB�oB��B��B��B��B�B��B��B��B�;B�B��B��B�;B�B�AB�B��B��B�oB�B�B�B�oB�B�B�B�B�B�{B��B�;B��B��B�AB�B��B�B��B��B��B�B��B��B��B�B�;B�B��BcB�4B�;B�{B��B�AB�oB�iB��B��B��B��B�lB�_B��B��B�FB��B��B��B��B�FB�zB�@B��B��B�B�RB�RB�XB��B�_B��B��B��B�B�B��B�B��B��B�CB��B��B�}B�B��B�B�}B��B��B��B��B��B�nB��B�nB��B�B��B�tB�FB�tB�B��B�FB��B�LB�FB��B�B�FB��B�zB�LB��B��B��B��B�^B�B��B�<B�OB��B��B��B�aBбB��B�pBϫBΥB�TB҉B� B�&B�mB�2B��BרB֡B�gB�mBیB�QB�QB��B��B�;B�5B�B�B��B�B�B�GB��B�B�B�B��B� B��B�B�B��B��B�TB�B�B�B�ZB��B�ZB��B�ZB�2B�>B�DB�DB�rB�	B�lB��B��B�B��B�PB��B�xB��B�B��B�B�B��B�JB��B��B��B�DB�B��B��B��B�rB�DB�B�B�+B�B�B�>B�JB��B��B�xB�B�DB�DB��B�lB��B�8B�.B�B�xB��B�B�B�rB�DB�xB�lB�B��B�B��B�%B��B��B�GB�iB��B�B�QB� B��B�B�|B�
B�TB�B�8B�vB�fBߤB�|B�BٴB��B�aB��B�NB�B��B��B��B�B�B��B�9B�9B�-B�LB��B�B��B�B��B��B�BɆB�jB�BB��B�3B�!B�-B�B�}B��B�wB�B�_B��B��B�B�-B�B��B��BȴB��B��B�B�B�!B�~B��B�B��B�FB�MB��B�@B��B�{B��B�VB�GB{Bv�ByrBcBxB|�Bx�B|PBzBo5BpBsMBzBx8Bt�B�PB��B��B�B��B�B��B��B��B��B�^B��B��B�B�B�zB��B��B��B�0B��B�B�OB�~B��B� BiyBi�B[�BI�BI�B?}B:�B6�B49B2aB/B2aB,B+�B/OB,�B*�B(�B&�B&�B$�B'�B \B�B#�B�B%B	B�BYBB�B{B�B{B 'B�B@B:B_BhB\B@B�B�B B�B(B�B.BbB�B�B�BbB(B�B�B B�B4B�B.B�B BbBbBhB�B�B B�B.B�B�B BbBB:B.BBbBbB�B B�B�B�B�B�B4B:B�B�B�BhB�B�B"B�B�B�B�B�B�B"B\B�B	�B�B1B�B+B�B	�B	7B1BB�B��B��B�ZB�B�B�vB�B�xB�KB�B��B�B��B��B�TB��B��B�TB�B�|B��B�TB�vB�HB�5B�pBݘB��B��B�WBیB�B�)B�B��B�mBҽB�mB��BĜB��B��B��B�dB��B��B��B��B��B��B��B��B�wB��B�*B��B�$B�@B�B�hB�B�B��B�B�tB��B��B��B��B�IB��B�~B��B�	B��B�qB��B�_B��B�+B��B��B�B��B�FB�B��B��B�oB�FB�uB�FB�(B��B�~B�"B��B��B�B��B��B��B�B�SB� B�B|PB��B��BxlBrBo�BiDBncBgmBe`BiBh
Bd&Bh�BjKBf�B_pB\�BV�BU�BS&BQ�BQBR�BS&BPHBM�BL�BI�BH�BG�BGEBGzBE�BF�BF?BK�BE9BB�BCaBA BA�B?}B?}B>BB>wB<6B@�B@�B=�B6FB8RB7�B6�B2�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202110151929172021101519291720211015192917202110151929172021101519291720211015192917SI  SI  ARFMARFM                                                                                                                                                2021040119284920210401192849IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040414012320210404140123QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040414012320210404140123QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074620211014130746IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                