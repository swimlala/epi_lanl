CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  F   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-02-14T08:08:43Z creation; 2023-02-10T23:09:43Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  W8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  ]�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  ~�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ӝ   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0  �X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � /D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 5�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` P    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   P`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   V`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   \`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T b`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   b�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   b�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   b�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   b�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � b�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   cT   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   cp   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    cx   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        c�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        c�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       c�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    c�Argo profile    3.1 1.2 19500101000000  20220214080843  20230210230943  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_200                 6810_008521_200                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٹ���u�@ٹ���u�11  @ٹ����@ٹ����@0?X:S��@0?X:S���d��ŗNf�d��ŗNf11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?aG�@   @B�\@�G�@�  @��R@�  A   A��A ��A,(�AAG�Aa�A���A�  A�  A�  A��AϮA߮A�A��BQ�B(�B�
B�
B'�B/�B7�
B@  BH  BO�
BW�B_�
Bh(�BpQ�Bx  B�
B�  B��B��B�  B��B��B�  B�  B�  B�(�B�=qB�{B��B�{B�  B��B�{B�{B�  B�  B�{B�  B��B�  B�{B�{B�(�B�(�B�  B��
B�  C   C  C
=C  C  C
  C  C��C��C
=C  C  C  C��C��C  C   C"  C$  C&  C(
=C*
=C,  C-�C/�C1�C4  C6  C8
=C:  C<  C>
=C@  CB  CD  CE��CG��CI��CL
=CN
=CP
=CR  CS��CU��CW�HCY��C\
=C^{C`  Ca��Cd  Cf  Ch  Ci�Cl  Cn
=Cp
=Cr  Cs��Cu��Cw��Cy��C{��C}��C��C���C�  C���C�  C�  C���C�  C�C�C�  C���C�  C���C���C���C���C�  C�  C�C�C�C���C���C�  C���C�  C�  C�
=C�
=C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�  C�  C�  C�
=C�C�  C���C���C���C�  C�  C���C�  C�C�  C���C���C���C���C���C���C�C�  C���C�C�  C���C�  C�  C���C���C���C���C�  C�C�C���C�  C�C�C�C�C�C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C���C��C���C���C���C�  C�  C�  C�C�  C��C�  C�\C�C�
=C�
=C���C�  C�
=C���C���C�  C�
=C�
=C�
=C�C���C�  C�C�
=C�C�  C�  D �D ��D�D��D�D�D�D}qD�qD}qD�qD� D��D��DD��D  D� D	  D	� D
  D
��D�D��D�D��D  Dz�D�qD}qD�qD}qD  D}qD�qD}qD  Dz�D��Dz�D  D��D  D��D�qD}qD  D}qD�qD�D  Dz�D��DxRD��D}qD�RDz�D��Dz�D��D}qD  Dz�D�qD ��D!  D!}qD"�D"��D#�D#� D$  D$}qD$�qD%}qD&�D&��D&�qD'��D(D(� D(�qD)}qD*  D*� D+�D+� D,  D,}qD,�qD-��D.�D.}qD.�qD/��D0  D0��D1D1��D2  D2}qD2��D3� D4�D4��D4�qD5��D6�D6z�D6�qD7z�D8  D8��D9  D9� D:D:� D:�qD;� D<�D<� D=�D=��D>�D>�D?�D?}qD@  D@�DADA��DB�DBz�DB�qDC� DD  DD� DEDE�DF  DF� DF�qDG}qDHDH�DH�qDIz�DI��DJz�DJ�qDK}qDK�qDL� DMDM��DN  DN��DO�DO��DP  DP� DP��DQ}qDR  DR}qDS  DS��DS�qDT}qDT�qDU� DV  DV� DW�DW� DX  DX��DY�DY��DZ�DZ��D[  D[}qD[�qD\z�D]  D]� D]�qD^}qD_  D_� D_�qD`}qDa  Da��Db�Db}qDc  Dc� Dd  Dd��Dd�qDe}qDf�Df��Dg  Dg� Dh�Dh�DiDi� Dj  Dj� Dj�qDk��Dl  Dl��DmDm��Dn  Dn� Dn�qDo}qDp�Dp�Dq�Dq� Dr  Dr}qDs�Ds�Ds�qDt}qDu  Du}qDv  Dv��Dw�Dw�Dx�Dx}qDy  Dy� Dz  Dz� D{�D{�D|�D|}qD}  D}� D}�qD~� D~�qD}qD�qD�>�D�� D��HD���D�>�D��HD��HD�  D�@ D�~�D��HD�  D�>�D�� D�� D�  D�@ D�~�D���D���D�=qD�� D��HD�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�HD�>�D�~�D�� D�HD�@ D�~�D���D�HD�AHD��HD��RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?L��?�=q?\?�@�@�R@8Q�@G�@\(�@xQ�@��\@�{@��H@��
@�33@��H@��@�33@ٙ�@�=q@�33@�(�A�A��A�RAA��A\)A%�A(��A/\)A5A:=qA?\)AFffAJ�HAP  AVffAZ�HA_\)AfffAk�Ao\)AuA{�A�Q�A��
A�A���A�(�A�{A���A�(�A�ffA�=qA�z�A��RA�=qA�z�A��A��HA��A��A�33A��A�Q�A��A�p�A���A�33A��A�Q�A˅A�p�A�Q�A��HA��Aأ�A��HA�p�A��A��HA�p�A��A�A�p�A��A�A�p�A�Q�A��A�p�B Q�B��B�RB(�BB
=B  B	B
=B  B��B
=BQ�B�B�RB(�BG�BffB  BG�B=qB�
BG�B=qB (�B!�B"ffB$  B%�B&ffB((�B)G�B*ffB,(�B-G�B.ffB0  B1G�B2ffB4(�B5G�B6�\B8(�B9�B:=qB;�
B=G�B>{B?�
BA�BA�BC�BD��BE�BG�BHz�BIBK\)BL(�BMp�BO
=BPQ�BQG�BR�RBT(�BUG�BV�HBX(�BY�BZ�HB\  B]�B^�HB_�
Ba�Bb�RBc�Be�Bf�RBg�BiG�Bj�\Bk�Bm�Bn�\Bo�Bp��BrffBs\)Btz�Bv{Bw
=BxQ�ByBz�RB|Q�B}��B~ffB�B���B��B��B�z�B��B���B�Q�B��B���B�(�B�
=B���B�(�B���B��B�Q�B���B��B�Q�B���B��B�Q�B���B�p�B�Q�B��RB�\)B�(�B��RB�\)B�(�B��RB�\)B�(�B��HB�G�B�(�B��HB�\)B�{B��HB��B�{B���B��B�=qB��RB�p�B�=qB��HB�\)B�(�B��HB�\)B��B��RB�\)B��
B��\B�G�B�B�Q�B�
=B��B�  B��RB�G�B��B�(�B��HB�33B���B�(�B��RB���B�\)B�  B�=qB�z�B�
=B�\)B���B�  B�z�B���B���B��B��
B�  B�ffB��HB��B�\)B��B�(�B�z�B���B�G�B��B��B�ffB��RB���B�p�B��B��B�ffB���B���B�\)B��
B�{B�Q�B��HB�33B�p�B�B�=qB�ffB���B�G�B�\)B��
B�=qB���B��RB�33B��B��B�=qB¸RB��B�\)BÙ�B�(�B�ffBĸRB�G�Bř�B�B�=qBƸRB���B�G�B�B�{B�Q�BȸRB�33B�\)BɮB�(�B�z�BʸRB�
=B˅BˮB�{B̏\B���B�
=B�p�B��B�  B�ffB��HB�
=B�\)B��
B�  B�Q�B���B��B�\)BѮB�=qB�ffB���B�33B�p�B�B�Q�Bԏ\B���B�33Bՙ�B�B�  B֏\B���B���B�\)B��
B��B�=qBظRB���B��BمB�  B�(�B�ffB��HB�33B�p�B�  B�ffB܏\B���B݅B��
B�(�B�z�B���B�p�B߮B�{B��B���B�G�B�B�{B�z�B���B�33B�B�(�B�Q�B�RB�G�B�B��B�ffB���B�33B�B�{B�z�B�RB�33B�B�  B�ffB���B�33B�B�{B�z�B��B�33B홚B��
B�(�B�RB���B�33B�B�{B�Q�B���B�33B�B�B�=qB��B�RB��B�B��B�(�B�\B�
=B�33B��B�{B�=qB�z�B��HB�\)B��B��B�ffB���B�
=B�p�B��B�(�B�z�B�
=B�G�B���B�{B�ffB���B��B���B�B�  B��\B��HB��B��B��C (�C G�C ffC ��C �HC  C(�Cp�C��C�RC  C�CG�C�C�RC��C
=CQ�CffC��C�
C��C�CffC�\C�C�
C�C=qCffC�C�C  C=qCz�C��C��C{C33CffC�C�
C  CQ�Cp�C��C�HC	
=C	33C	z�C	�C	C
  C
G�C
ffC
�\C
�
C
�C(�CffC�C�C��C{C=qC�C��C��C{C(�C\)C��C��C�HC(�CffCz�C��C�C{C=qCz�C�RC�HC
=CQ�C�\C��C�
C�C\)C��C�RC�C33CffC�\CC  C=qCz�C��C��C{CQ�Cp�C�C��C�CG�C�\C��C�C�CffC��CC��C=qC\)C�\C�HC
=C=qC�\C�RC�HC33Cp�C��C��C{C\)C�C�RC
=C=qCffC�C��C�C\)C��C�HC  C=qC�CC�C�CffC�C��C
=CQ�C��CC��C G�C �C �C �C!33C!ffC!��C!�HC"(�C"Q�C"�C"��C#{C#G�C#z�C#�RC$
=C$=qC$p�C$�C$�HC%33C%z�C%��C%�
C&�C&ffC&�\C&��C'�C'\)C'�C'��C(  C(Q�C(�\C(C(��C)=qC)�C)C)�C*(�C*p�C*�RC*��C+�C+ffC+��C+�C,�C,G�C,�C,��C-{C-=qC-ffC-C.  C.(�C.Q�C.��C.�C/�C/G�C/z�C/C0
=C0G�C0p�C0��C0�HC1(�C1Q�C1z�C1�C1�C2(�C2ffC2�\C2�RC3  C3=qC3z�C3��C3��C3��C4�C4\)C4�\C4C5  C533C5ffC5�\C5�RC5�C6(�C6ffC6��C6C6�HC7�C7\)C7�\C7�RC7�HC8{C8Q�C8�\C8C8�C9
=C9=qC9z�C9�C9�C:(�C:Q�C:p�C:��C:��C;
=C;G�C;ffC;�C;�RC<  C<33C<\)C<�C<�C<�HC=�C=\)C=��C=�
C>  C>(�C>\)C>�\C>��C?
=C?Q�C?�\C?C?�C@{C@Q�C@�\C@��CA
=CAG�CAz�CA��CA��CB
=CBQ�CB�\CBCB��CC�CCQ�CC�CCCD
=CDG�CDz�CD�CD�
CE
=CE=qCEp�CE�CF  CF33CF\)CF�CFCG  CG=qCGz�CG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?aG�@   @B�\@�G�@�  @��R@�  A   A��A ��A,(�AAG�Aa�A���A�  A�  A�  A��AϮA߮A�A��BQ�B(�B�
B�
B'�B/�B7�
B@  BH  BO�
BW�B_�
Bh(�BpQ�Bx  B�
B�  B��B��B�  B��B��B�  B�  B�  B�(�B�=qB�{B��B�{B�  B��B�{B�{B�  B�  B�{B�  B��B�  B�{B�{B�(�B�(�B�  B��
B�  C   C  C
=C  C  C
  C  C��C��C
=C  C  C  C��C��C  C   C"  C$  C&  C(
=C*
=C,  C-�C/�C1�C4  C6  C8
=C:  C<  C>
=C@  CB  CD  CE��CG��CI��CL
=CN
=CP
=CR  CS��CU��CW�HCY��C\
=C^{C`  Ca��Cd  Cf  Ch  Ci�Cl  Cn
=Cp
=Cr  Cs��Cu��Cw��Cy��C{��C}��C��C���C�  C���C�  C�  C���C�  C�C�C�  C���C�  C���C���C���C���C�  C�  C�C�C�C���C���C�  C���C�  C�  C�
=C�
=C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�  C�  C�  C�
=C�C�  C���C���C���C�  C�  C���C�  C�C�  C���C���C���C���C���C���C�C�  C���C�C�  C���C�  C�  C���C���C���C���C�  C�C�C���C�  C�C�C�C�C�C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C���C��C���C���C���C�  C�  C�  C�C�  C��C�  C�\C�C�
=C�
=C���C�  C�
=C���C���C�  C�
=C�
=C�
=C�C���C�  C�C�
=C�C�  C�  D �D ��D�D��D�D�D�D}qD�qD}qD�qD� D��D��DD��D  D� D	  D	� D
  D
��D�D��D�D��D  Dz�D�qD}qD�qD}qD  D}qD�qD}qD  Dz�D��Dz�D  D��D  D��D�qD}qD  D}qD�qD�D  Dz�D��DxRD��D}qD�RDz�D��Dz�D��D}qD  Dz�D�qD ��D!  D!}qD"�D"��D#�D#� D$  D$}qD$�qD%}qD&�D&��D&�qD'��D(D(� D(�qD)}qD*  D*� D+�D+� D,  D,}qD,�qD-��D.�D.}qD.�qD/��D0  D0��D1D1��D2  D2}qD2��D3� D4�D4��D4�qD5��D6�D6z�D6�qD7z�D8  D8��D9  D9� D:D:� D:�qD;� D<�D<� D=�D=��D>�D>�D?�D?}qD@  D@�DADA��DB�DBz�DB�qDC� DD  DD� DEDE�DF  DF� DF�qDG}qDHDH�DH�qDIz�DI��DJz�DJ�qDK}qDK�qDL� DMDM��DN  DN��DO�DO��DP  DP� DP��DQ}qDR  DR}qDS  DS��DS�qDT}qDT�qDU� DV  DV� DW�DW� DX  DX��DY�DY��DZ�DZ��D[  D[}qD[�qD\z�D]  D]� D]�qD^}qD_  D_� D_�qD`}qDa  Da��Db�Db}qDc  Dc� Dd  Dd��Dd�qDe}qDf�Df��Dg  Dg� Dh�Dh�DiDi� Dj  Dj� Dj�qDk��Dl  Dl��DmDm��Dn  Dn� Dn�qDo}qDp�Dp�Dq�Dq� Dr  Dr}qDs�Ds�Ds�qDt}qDu  Du}qDv  Dv��Dw�Dw�Dx�Dx}qDy  Dy� Dz  Dz� D{�D{�D|�D|}qD}  D}� D}�qD~� D~�qD}qD�qD�>�D�� D��HD���D�>�D��HD��HD�  D�@ D�~�D��HD�  D�>�D�� D�� D�  D�@ D�~�D���D���D�=qD�� D��HD�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�@ D��HD��HD�HD�>�D�~�D�� D�HD�@ D�~�D���D�HD�AHD��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?�?L��?�=q?\?�@�@�R@8Q�@G�@\(�@xQ�@��\@�{@��H@��
@�33@��H@��@�33@ٙ�@�=q@�33@�(�A�A��A�RAA��A\)A%�A(��A/\)A5A:=qA?\)AFffAJ�HAP  AVffAZ�HA_\)AfffAk�Ao\)AuA{�A�Q�A��
A�A���A�(�A�{A���A�(�A�ffA�=qA�z�A��RA�=qA�z�A��A��HA��A��A�33A��A�Q�A��A�p�A���A�33A��A�Q�A˅A�p�A�Q�A��HA��Aأ�A��HA�p�A��A��HA�p�A��A�A�p�A��A�A�p�A�Q�A��A�p�B Q�B��B�RB(�BB
=B  B	B
=B  B��B
=BQ�B�B�RB(�BG�BffB  BG�B=qB�
BG�B=qB (�B!�B"ffB$  B%�B&ffB((�B)G�B*ffB,(�B-G�B.ffB0  B1G�B2ffB4(�B5G�B6�\B8(�B9�B:=qB;�
B=G�B>{B?�
BA�BA�BC�BD��BE�BG�BHz�BIBK\)BL(�BMp�BO
=BPQ�BQG�BR�RBT(�BUG�BV�HBX(�BY�BZ�HB\  B]�B^�HB_�
Ba�Bb�RBc�Be�Bf�RBg�BiG�Bj�\Bk�Bm�Bn�\Bo�Bp��BrffBs\)Btz�Bv{Bw
=BxQ�ByBz�RB|Q�B}��B~ffB�B���B��B��B�z�B��B���B�Q�B��B���B�(�B�
=B���B�(�B���B��B�Q�B���B��B�Q�B���B��B�Q�B���B�p�B�Q�B��RB�\)B�(�B��RB�\)B�(�B��RB�\)B�(�B��HB�G�B�(�B��HB�\)B�{B��HB��B�{B���B��B�=qB��RB�p�B�=qB��HB�\)B�(�B��HB�\)B��B��RB�\)B��
B��\B�G�B�B�Q�B�
=B��B�  B��RB�G�B��B�(�B��HB�33B���B�(�B��RB���B�\)B�  B�=qB�z�B�
=B�\)B���B�  B�z�B���B���B��B��
B�  B�ffB��HB��B�\)B��B�(�B�z�B���B�G�B��B��B�ffB��RB���B�p�B��B��B�ffB���B���B�\)B��
B�{B�Q�B��HB�33B�p�B�B�=qB�ffB���B�G�B�\)B��
B�=qB���B��RB�33B��B��B�=qB¸RB��B�\)BÙ�B�(�B�ffBĸRB�G�Bř�B�B�=qBƸRB���B�G�B�B�{B�Q�BȸRB�33B�\)BɮB�(�B�z�BʸRB�
=B˅BˮB�{B̏\B���B�
=B�p�B��B�  B�ffB��HB�
=B�\)B��
B�  B�Q�B���B��B�\)BѮB�=qB�ffB���B�33B�p�B�B�Q�Bԏ\B���B�33Bՙ�B�B�  B֏\B���B���B�\)B��
B��B�=qBظRB���B��BمB�  B�(�B�ffB��HB�33B�p�B�  B�ffB܏\B���B݅B��
B�(�B�z�B���B�p�B߮B�{B��B���B�G�B�B�{B�z�B���B�33B�B�(�B�Q�B�RB�G�B�B��B�ffB���B�33B�B�{B�z�B�RB�33B�B�  B�ffB���B�33B�B�{B�z�B��B�33B홚B��
B�(�B�RB���B�33B�B�{B�Q�B���B�33B�B�B�=qB��B�RB��B�B��B�(�B�\B�
=B�33B��B�{B�=qB�z�B��HB�\)B��B��B�ffB���B�
=B�p�B��B�(�B�z�B�
=B�G�B���B�{B�ffB���B��B���B�B�  B��\B��HB��B��B��C (�C G�C ffC ��C �HC  C(�Cp�C��C�RC  C�CG�C�C�RC��C
=CQ�CffC��C�
C��C�CffC�\C�C�
C�C=qCffC�C�C  C=qCz�C��C��C{C33CffC�C�
C  CQ�Cp�C��C�HC	
=C	33C	z�C	�C	C
  C
G�C
ffC
�\C
�
C
�C(�CffC�C�C��C{C=qC�C��C��C{C(�C\)C��C��C�HC(�CffCz�C��C�C{C=qCz�C�RC�HC
=CQ�C�\C��C�
C�C\)C��C�RC�C33CffC�\CC  C=qCz�C��C��C{CQ�Cp�C�C��C�CG�C�\C��C�C�CffC��CC��C=qC\)C�\C�HC
=C=qC�\C�RC�HC33Cp�C��C��C{C\)C�C�RC
=C=qCffC�C��C�C\)C��C�HC  C=qC�CC�C�CffC�C��C
=CQ�C��CC��C G�C �C �C �C!33C!ffC!��C!�HC"(�C"Q�C"�C"��C#{C#G�C#z�C#�RC$
=C$=qC$p�C$�C$�HC%33C%z�C%��C%�
C&�C&ffC&�\C&��C'�C'\)C'�C'��C(  C(Q�C(�\C(C(��C)=qC)�C)C)�C*(�C*p�C*�RC*��C+�C+ffC+��C+�C,�C,G�C,�C,��C-{C-=qC-ffC-C.  C.(�C.Q�C.��C.�C/�C/G�C/z�C/C0
=C0G�C0p�C0��C0�HC1(�C1Q�C1z�C1�C1�C2(�C2ffC2�\C2�RC3  C3=qC3z�C3��C3��C3��C4�C4\)C4�\C4C5  C533C5ffC5�\C5�RC5�C6(�C6ffC6��C6C6�HC7�C7\)C7�\C7�RC7�HC8{C8Q�C8�\C8C8�C9
=C9=qC9z�C9�C9�C:(�C:Q�C:p�C:��C:��C;
=C;G�C;ffC;�C;�RC<  C<33C<\)C<�C<�C<�HC=�C=\)C=��C=�
C>  C>(�C>\)C>�\C>��C?
=C?Q�C?�\C?C?�C@{C@Q�C@�\C@��CA
=CAG�CAz�CA��CA��CB
=CBQ�CB�\CBCB��CC�CCQ�CC�CCCD
=CDG�CDz�CD�CD�
CE
=CE=qCEp�CE�CF  CF33CF\)CF�CFCG  CG=qCGz�CG��CG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aʏ\AʓuAʓuAʣ�Aʣ�Aʣ�Aʩ�AʬAʩ�AʬAʧ�AʮAʴ9Aʰ!AʓuAʓuAʋDAʅAʇ+A�x�A�p�A�v�A�~�Aʇ+Aʝ�Aʟ�Aʧ�Aʡ�AʬAʮAʰ!AʮAʶFAʶFAʶFAʸRA��
A�$�A�G�A��A��/A���A�Aʩ�A�v�A�O�A�7LA�1A�n�A�"�A�  A�ȴAț�A�K�AǛ�Aƺ^A�bNAĬA�|�A���A��A��A�VA���A��TA�ƨA�A��\A���A�9XA�C�A��jA�dZA�bA��/A�VA��wA�VA��A�VA�A�dZA��FA�(�A�1A��A�|�A���A��A�33A�Q�A�bA��`A��hA��A��A��yA�ZA��!A���A���A��A�ĜA�A�  A� �A�hsA��TA���A���A��A��9A~I�A}K�A|�`A|�A{33Ay�mAwAu�
Ar�ApbNAoAh1Ae�Ab��AbI�AahsA]��A[�7AV�!AR�AN$�AJ��AH��AD1ABz�AA�PA=
=A:ĜA9�FA9"�A7��A3��A2�+A1;dA0r�A01'A/G�A.A,��A,=qA+��A+"�A(�/A($�A'�A&~�A%��A%O�A$ĜA$�A#t�A#VA"ĜA"A�A!;dA ��A �uA�PA�A��AZAv�A�AĜA{A�AbNA1'AJA��A�AK�A��A�jAA�A|�A�yA�PA��A$�A��A�PA\)AK�A�A��A��Az�AVA��AZA�A��At�A
�jA
^5A	�A9XA�A��A�A�TA��A`BA/A�/A�\A1'A�;A��A|�AS�A��A�#A&�@���@�33@���@�$�@��7@�`B@��@��F@��@�ff@�&�@�n�@�Ĝ@���@�dZ@�E�@��@�ȴ@�=q@��-@�x�@�p�@�Ĝ@��@��`@�9X@�  @�P@�ff@�@�^@�h@�x�@�+@�!@�J@�p�@�D@�(�@��@�v�@���@���@�bN@��@�l�@�
=@�n�@ݑh@��@ܓu@�1@ۥ�@��@��y@ڗ�@�n�@��#@ٙ�@�X@׮@�33@�@��@��@���@�E�@պ^@��/@�1'@��@Ӿw@�
=@�~�@ѡ�@�bN@���@϶F@�"�@θR@�$�@�%@�1@˥�@�9X@ˍP@�S�@�o@�~�@��@Ɂ@ț�@�b@�l�@�V@�-@őh@ř�@Ł@�`B@�V@�bN@Õ�@��@�E�@��#@��@�J@��@���@�G�@�/@�1'@�|�@�hs@�?}@�?}@��@��D@�I�@�bN@��@��@�p�@���@�Z@��
@��@�=q@���@���@��D@� �@��P@�^5@��@�`B@�/@��@���@���@�Z@�1'@��@���@��F@�{@�@���@���@��@�?}@�Ĝ@�Z@�1'@�1'@��@��@��w@�|�@��@�~�@���@���@��9@�j@��;@���@�S�@�33@��@��H@���@��#@���@�x�@�7L@���@��/@��9@���@�j@��@��@�C�@��H@�@���@���@�p�@���@�j@� �@��@�33@���@�v�@�@�@���@�7L@��9@�b@��
@�|�@�C�@��@��!@�^5@��T@�`B@��@���@��u@�I�@�b@���@�S�@��H@�=q@���@�`B@��@��j@���@���@��w@��P@�\)@��@�5?@��-@��@�O�@�%@�Ĝ@��@���@��@�\)@�K�@�C�@�33@�
=@�
=@�
=@�@���@�v�@��-@�x�@���@�I�@�(�@��;@��@�t�@�K�@�+@�"�@��@��@�"�@�
=@��@��@���@��R@��R@���@�ff@�=q@�5?@�-@�J@��T@��7@�p�@�O�@��@��@���@�Ĝ@��u@�A�@�1@��;@��F@���@��P@�|�@�C�@�+@��@��@��H@��@���@���@�~�@�^5@�E�@�5?@�$�@�@���@�x�@�`B@�?}@�V@��`@��j@���@��@�Q�@�I�@�9X@�\)@�C�@�;d@�+@��!@��\@�v�@�ff@�M�@�-@��-@�p�@��@��F@�o@��+@�^5@��^@�G�@��@�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʏ\AʑhAʍPAʍPAʕ�AʓuAʓuAʑhAʏ\AʑhAʗ�Aʥ�Aʟ�Aʣ�Aʥ�Aʝ�Aʥ�Aʣ�Aʣ�AʬAʥ�AʬAʮAʩ�AʮAʧ�Aʩ�AʮAʧ�AʬAʬAʧ�Aʩ�AʮAʮAʰ!Aʲ-AʮAʮAʴ9Aʴ9Aʲ-AʸRAʸRAʴ9AʸRAʸRAʬAʣ�AʬAʗ�Aʕ�AʑhAʕ�AʓuAʏ\AʓuAʓuAʑhAʗ�AʑhAʓuAʓuAʋDAʉ7AʍPAʅAʉ7Aʉ7AʃAʇ+AʃAʁAʅAʉ7AʅAʋDAʉ7AʃAʅAʁA�z�A�t�A�v�A�n�A�p�A�p�A�l�A�p�A�t�A�n�A�p�A�r�A�n�A�v�A�z�A�|�AʁAʅA�~�A�x�A�|�A�z�A�x�A�|�Aʇ+Aʇ+Aʇ+AʓuAʓuAʕ�Aʕ�Aʛ�Aʡ�Aʣ�Aʧ�Aʣ�Aʛ�Aʡ�Aʝ�Aʟ�Aʥ�Aʧ�Aʩ�AʮAʩ�Aʥ�Aʥ�Aʝ�Aʛ�Aʝ�Aʝ�Aʧ�Aʴ9AʮAʩ�AʮAʩ�Aʧ�AʮAʮAʩ�Aʰ!Aʰ!AʬAʰ!AʮAʬAʰ!AʮAʮAʲ-AʬAʬAʰ!Aʰ!AʬAʰ!Aʴ9Aʲ-AʸRAʶFAʴ9Aʺ^AʸRAʴ9Aʺ^Aʴ9Aʴ9Aʺ^Aʴ9Aʴ9AʶFAʲ-Aʺ^Aʺ^AʶFAʺ^AʼjAʸRAʶFAʺ^AʸRAʺ^A�ƨA�ĜA���A��/A���A�oA��A��A��A�&�A�7LA�9XA�C�A�S�A�Q�A�O�A�A�A�9XA�5?A�33A� �A�1A�A���A��A��HA��A��A���A��
A��
A���A���A���A�ȴA�ȴA���A�ƨA�A�ĜAʼjAʸRAʺ^Aʺ^Aʴ9AʬAʟ�AʓuAʏ\AʁA�|�A�r�A�jA�ffA�`BA�VA�O�A�M�A�K�A�C�A�C�A�A�A�9XA�7LA�9XA�1'A�&�A� �A��A�
=A�A�A���A��TAɼjAɏ\A�jA�ZA�S�A�K�A�E�A�A�A�=qA�5?A�+A�"�A��A�{A��A�oA�bA�VA�JA�%A�%A�1A�1A�A�  A���A���A��A��A��`A��HA��HA��
A�ȴA�ƨA�ĜAȼjAȴ9Aȴ9AȮAȩ�AȮAȬAȥ�Aȡ�Aȟ�Aș�Aȕ�Aȗ�Aȕ�Aȉ7A�|�A�hsA�XA�S�A�O�A�I�A�G�A�I�A�I�A�C�A�E�A�A�A�1'A��A�%A��mA�ĜAǡ�AǕ�AǍPA�p�A�^5A�=qA�bA���A��A�ƨAƺ^AƸRAƲ-AƮAƮAư!AƧ�Aƣ�Aƣ�AƟ�AƓuAƋDAƁA�r�A�`BA�XA�A�A�-A�+A��A�1A��#AžwAōPA��A��yAģ�A�Q�A��A��`AÁA�(�A�
=A��yA��A�A�x�A�p�A�ffA�VA�K�A�C�A�?}A�5?A��A���A��;A���A��!A���A���A��hA�|�A�r�A�^5A�M�A�9XA�bA��A��A�S�A���A��RA�I�A��-A��^A�S�A�VA�A���A��RA���A���A��+A�r�A�M�A�33A�&�A�"�A��A��A�{A�oA�VA�A�%A�1A�%A�A�  A�  A���A���A���A���A���A��A���A��A��A��A��;A��;A��`A��HA��HA��;A��HA��;A��#A��;A��/A���A���A���A�ƨA�ȴA��jA���A���A�r�A�E�A�-A�/A�+A��A��yA��A���A�ȴA��^A��A�`BA�I�A��A���A��jA�t�A�VA��`A�A��-A���A�x�A�bNA�E�A�33A�JA��FA�dZA�JA��A��A�~�A�v�A�t�A�p�A�^5A�Q�A�K�A�E�A�7LA��A��A���A��A���A��DA�x�A�bNA�Q�A��A���A�A���A���A��;A��A���A���A���A��jA��RA��A���A���A���A���A���A��A�~�A�r�A�bNA�O�A�I�A�5?A�+A�"�A��A�{A��A�{A�1A�JA�%A�A�%A�  A�  A�A�A���A�  A�  A���A���A��hA�r�A�r�A�r�A�p�A�ffA�K�A��/A���A��RA���A���A��uA��A�ffA�\)A�/A�A�jA�"�A�JA��A���A�dZA�K�A�C�A�&�A��A�JA���A��`A�ƨA��A���A�VA�5?A�JA�  A��/A��-A��uA��A�t�A�ffA�\)A�Q�A�5?A�A��/A��A�ƨA�~�A�VA�C�A�1'A��A���A��`A���A���A���A�ȴA���A��RA��A���A���A�^5A�VA�ȴA���A��A�M�A��A��A���A�VA�+A�"�A��A���A��wA�^5A��A��#A��RA���A�v�A�^5A�Q�A�G�A�;dA�1A��A��;A��wA��A���A�|�A�bNA�C�A�(�A��A��yA���A�~�A�p�A�I�A�&�A���A��\A�ĜA�|�A�O�A�$�A���A��#A�ȴA�ĜA��^A��9A��!A��A��!A��!A��DA��A��hA�jA�/A��A���A��A�`BA�7LA��A��A�1A���A��mA��mA��9A�K�A�=qA�-A� �A�%A���A�ffA�9XA��A�A�  A���A��A���A���A�XA��A��;A�A�1'A��uA�M�A�oA��A���A�ȴA�A��DA�XA�I�A�C�A�9XA�7LA�33A�(�A�bA���A��A��7A�A�Q�A�O�A�Q�A�K�A�VA���A��/A�l�A�ZA�(�A���A��FA��A��FA�^5A��A���A���A�p�A�XA��A��A���A���A���A��uA�I�A��A��A��A��A��A�
=A���A��wA�ĜA��wA��FA���A���A�~�A�t�A�^5A�ZA�;dA�/A�5?A���A�v�A�v�A��A�G�A���A���A��/A�x�A��yA���A��^A���A�|�A�VA�9XA� �A���A��TA���A��FA�jA��FA�5?A��A�A��A��mA��#A�ƨA���A��7A�r�A�l�A�\)A�M�A�;dA�(�A�oA���A�ĜA��9A���A��A�ffA�5?A��A�A��HA�ȴA��9A���A��7A�|�A�\)A�;dA��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʏ\AʓuAʓuAʣ�Aʣ�Aʣ�Aʩ�AʬAʩ�AʬAʧ�AʮAʴ9Aʰ!AʓuAʓuAʋDAʅAʇ+A�x�A�p�A�v�A�~�Aʇ+Aʝ�Aʟ�Aʧ�Aʡ�AʬAʮAʰ!AʮAʶFAʶFAʶFAʸRA��
A�$�A�G�A��A��/A���A�Aʩ�A�v�A�O�A�7LA�1A�n�A�"�A�  A�ȴAț�A�K�AǛ�Aƺ^A�bNAĬA�|�A���A��A��A�VA���A��TA�ƨA�A��\A���A�9XA�C�A��jA�dZA�bA��/A�VA��wA�VA��A�VA�A�dZA��FA�(�A�1A��A�|�A���A��A�33A�Q�A�bA��`A��hA��A��A��yA�ZA��!A���A���A��A�ĜA�A�  A� �A�hsA��TA���A���A��A��9A~I�A}K�A|�`A|�A{33Ay�mAwAu�
Ar�ApbNAoAh1Ae�Ab��AbI�AahsA]��A[�7AV�!AR�AN$�AJ��AH��AD1ABz�AA�PA=
=A:ĜA9�FA9"�A7��A3��A2�+A1;dA0r�A01'A/G�A.A,��A,=qA+��A+"�A(�/A($�A'�A&~�A%��A%O�A$ĜA$�A#t�A#VA"ĜA"A�A!;dA ��A �uA�PA�A��AZAv�A�AĜA{A�AbNA1'AJA��A�AK�A��A�jAA�A|�A�yA�PA��A$�A��A�PA\)AK�A�A��A��Az�AVA��AZA�A��At�A
�jA
^5A	�A9XA�A��A�A�TA��A`BA/A�/A�\A1'A�;A��A|�AS�A��A�#A&�@���@�33@���@�$�@��7@�`B@��@��F@��@�ff@�&�@�n�@�Ĝ@���@�dZ@�E�@��@�ȴ@�=q@��-@�x�@�p�@�Ĝ@��@��`@�9X@�  @�P@�ff@�@�^@�h@�x�@�+@�!@�J@�p�@�D@�(�@��@�v�@���@���@�bN@��@�l�@�
=@�n�@ݑh@��@ܓu@�1@ۥ�@��@��y@ڗ�@�n�@��#@ٙ�@�X@׮@�33@�@��@��@���@�E�@պ^@��/@�1'@��@Ӿw@�
=@�~�@ѡ�@�bN@���@϶F@�"�@θR@�$�@�%@�1@˥�@�9X@ˍP@�S�@�o@�~�@��@Ɂ@ț�@�b@�l�@�V@�-@őh@ř�@Ł@�`B@�V@�bN@Õ�@��@�E�@��#@��@�J@��@���@�G�@�/@�1'@�|�@�hs@�?}@�?}@��@��D@�I�@�bN@��@��@�p�@���@�Z@��
@��@�=q@���@���@��D@� �@��P@�^5@��@�`B@�/@��@���@���@�Z@�1'@��@���@��F@�{@�@���@���@��@�?}@�Ĝ@�Z@�1'@�1'@��@��@��w@�|�@��@�~�@���@���@��9@�j@��;@���@�S�@�33@��@��H@���@��#@���@�x�@�7L@���@��/@��9@���@�j@��@��@�C�@��H@�@���@���@�p�@���@�j@� �@��@�33@���@�v�@�@�@���@�7L@��9@�b@��
@�|�@�C�@��@��!@�^5@��T@�`B@��@���@��u@�I�@�b@���@�S�@��H@�=q@���@�`B@��@��j@���@���@��w@��P@�\)@��@�5?@��-@��@�O�@�%@�Ĝ@��@���@��@�\)@�K�@�C�@�33@�
=@�
=@�
=@�@���@�v�@��-@�x�@���@�I�@�(�@��;@��@�t�@�K�@�+@�"�@��@��@�"�@�
=@��@��@���@��R@��R@���@�ff@�=q@�5?@�-@�J@��T@��7@�p�@�O�@��@��@���@�Ĝ@��u@�A�@�1@��;@��F@���@��P@�|�@�C�@�+@��@��@��H@��@���@���@�~�@�^5@�E�@�5?@�$�@�@���@�x�@�`B@�?}@�V@��`@��j@���@��@�Q�@�I�@�9X@�\)@�C�@�;d@�+@��!@��\@�v�@�ff@�M�@�-@��-@�p�@��@��F@�o@��+@�^5@��^@�G�@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aʏ\AʑhAʍPAʍPAʕ�AʓuAʓuAʑhAʏ\AʑhAʗ�Aʥ�Aʟ�Aʣ�Aʥ�Aʝ�Aʥ�Aʣ�Aʣ�AʬAʥ�AʬAʮAʩ�AʮAʧ�Aʩ�AʮAʧ�AʬAʬAʧ�Aʩ�AʮAʮAʰ!Aʲ-AʮAʮAʴ9Aʴ9Aʲ-AʸRAʸRAʴ9AʸRAʸRAʬAʣ�AʬAʗ�Aʕ�AʑhAʕ�AʓuAʏ\AʓuAʓuAʑhAʗ�AʑhAʓuAʓuAʋDAʉ7AʍPAʅAʉ7Aʉ7AʃAʇ+AʃAʁAʅAʉ7AʅAʋDAʉ7AʃAʅAʁA�z�A�t�A�v�A�n�A�p�A�p�A�l�A�p�A�t�A�n�A�p�A�r�A�n�A�v�A�z�A�|�AʁAʅA�~�A�x�A�|�A�z�A�x�A�|�Aʇ+Aʇ+Aʇ+AʓuAʓuAʕ�Aʕ�Aʛ�Aʡ�Aʣ�Aʧ�Aʣ�Aʛ�Aʡ�Aʝ�Aʟ�Aʥ�Aʧ�Aʩ�AʮAʩ�Aʥ�Aʥ�Aʝ�Aʛ�Aʝ�Aʝ�Aʧ�Aʴ9AʮAʩ�AʮAʩ�Aʧ�AʮAʮAʩ�Aʰ!Aʰ!AʬAʰ!AʮAʬAʰ!AʮAʮAʲ-AʬAʬAʰ!Aʰ!AʬAʰ!Aʴ9Aʲ-AʸRAʶFAʴ9Aʺ^AʸRAʴ9Aʺ^Aʴ9Aʴ9Aʺ^Aʴ9Aʴ9AʶFAʲ-Aʺ^Aʺ^AʶFAʺ^AʼjAʸRAʶFAʺ^AʸRAʺ^A�ƨA�ĜA���A��/A���A�oA��A��A��A�&�A�7LA�9XA�C�A�S�A�Q�A�O�A�A�A�9XA�5?A�33A� �A�1A�A���A��A��HA��A��A���A��
A��
A���A���A���A�ȴA�ȴA���A�ƨA�A�ĜAʼjAʸRAʺ^Aʺ^Aʴ9AʬAʟ�AʓuAʏ\AʁA�|�A�r�A�jA�ffA�`BA�VA�O�A�M�A�K�A�C�A�C�A�A�A�9XA�7LA�9XA�1'A�&�A� �A��A�
=A�A�A���A��TAɼjAɏ\A�jA�ZA�S�A�K�A�E�A�A�A�=qA�5?A�+A�"�A��A�{A��A�oA�bA�VA�JA�%A�%A�1A�1A�A�  A���A���A��A��A��`A��HA��HA��
A�ȴA�ƨA�ĜAȼjAȴ9Aȴ9AȮAȩ�AȮAȬAȥ�Aȡ�Aȟ�Aș�Aȕ�Aȗ�Aȕ�Aȉ7A�|�A�hsA�XA�S�A�O�A�I�A�G�A�I�A�I�A�C�A�E�A�A�A�1'A��A�%A��mA�ĜAǡ�AǕ�AǍPA�p�A�^5A�=qA�bA���A��A�ƨAƺ^AƸRAƲ-AƮAƮAư!AƧ�Aƣ�Aƣ�AƟ�AƓuAƋDAƁA�r�A�`BA�XA�A�A�-A�+A��A�1A��#AžwAōPA��A��yAģ�A�Q�A��A��`AÁA�(�A�
=A��yA��A�A�x�A�p�A�ffA�VA�K�A�C�A�?}A�5?A��A���A��;A���A��!A���A���A��hA�|�A�r�A�^5A�M�A�9XA�bA��A��A�S�A���A��RA�I�A��-A��^A�S�A�VA�A���A��RA���A���A��+A�r�A�M�A�33A�&�A�"�A��A��A�{A�oA�VA�A�%A�1A�%A�A�  A�  A���A���A���A���A���A��A���A��A��A��A��;A��;A��`A��HA��HA��;A��HA��;A��#A��;A��/A���A���A���A�ƨA�ȴA��jA���A���A�r�A�E�A�-A�/A�+A��A��yA��A���A�ȴA��^A��A�`BA�I�A��A���A��jA�t�A�VA��`A�A��-A���A�x�A�bNA�E�A�33A�JA��FA�dZA�JA��A��A�~�A�v�A�t�A�p�A�^5A�Q�A�K�A�E�A�7LA��A��A���A��A���A��DA�x�A�bNA�Q�A��A���A�A���A���A��;A��A���A���A���A��jA��RA��A���A���A���A���A���A��A�~�A�r�A�bNA�O�A�I�A�5?A�+A�"�A��A�{A��A�{A�1A�JA�%A�A�%A�  A�  A�A�A���A�  A�  A���A���A��hA�r�A�r�A�r�A�p�A�ffA�K�A��/A���A��RA���A���A��uA��A�ffA�\)A�/A�A�jA�"�A�JA��A���A�dZA�K�A�C�A�&�A��A�JA���A��`A�ƨA��A���A�VA�5?A�JA�  A��/A��-A��uA��A�t�A�ffA�\)A�Q�A�5?A�A��/A��A�ƨA�~�A�VA�C�A�1'A��A���A��`A���A���A���A�ȴA���A��RA��A���A���A�^5A�VA�ȴA���A��A�M�A��A��A���A�VA�+A�"�A��A���A��wA�^5A��A��#A��RA���A�v�A�^5A�Q�A�G�A�;dA�1A��A��;A��wA��A���A�|�A�bNA�C�A�(�A��A��yA���A�~�A�p�A�I�A�&�A���A��\A�ĜA�|�A�O�A�$�A���A��#A�ȴA�ĜA��^A��9A��!A��A��!A��!A��DA��A��hA�jA�/A��A���A��A�`BA�7LA��A��A�1A���A��mA��mA��9A�K�A�=qA�-A� �A�%A���A�ffA�9XA��A�A�  A���A��A���A���A�XA��A��;A�A�1'A��uA�M�A�oA��A���A�ȴA�A��DA�XA�I�A�C�A�9XA�7LA�33A�(�A�bA���A��A��7A�A�Q�A�O�A�Q�A�K�A�VA���A��/A�l�A�ZA�(�A���A��FA��A��FA�^5A��A���A���A�p�A�XA��A��A���A���A���A��uA�I�A��A��A��A��A��A�
=A���A��wA�ĜA��wA��FA���A���A�~�A�t�A�^5A�ZA�;dA�/A�5?A���A�v�A�v�A��A�G�A���A���A��/A�x�A��yA���A��^A���A�|�A�VA�9XA� �A���A��TA���A��FA�jA��FA�5?A��A�A��A��mA��#A�ƨA���A��7A�r�A�l�A�\)A�M�A�;dA�(�A�oA���A�ĜA��9A���A��A�ffA�5?A��A�A��HA�ȴA��9A���A��7A�|�A�\)A�;dA��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                                 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��A홚B
@�B
@�B
>�B
@�B
@B
@B
?HB
@OB
@�B
@�B
E�B
N�B
e�B
�'B
��B
��B
��B
��B
��B
��B
��B
�wB
�'B
ʌB
��B
��B
�HB
՛B
�?B
�B
�WB
ߤB
�B
�B
�B
�%B)�B��B�0B�zB�wB�aB�B�dB�vB�B�2B��B�B�sB�cB�B�BoB1BfB�BOBVB<6BDgB@OB>�B<�B:�B4�B8B?�BCaBJ�BM6BP�BR BS�BZBa|B_�Bd�B[#BQ�BO�BK�BCaB%�BVB�B%BSB�B�B��B�B��B��B�DBr�BV9BLdB>BB,qBeB
��B
��B
�B
�pB
�mB
ȴB
��B
��B
�"B
~�B
g8B
\�B
Y�B
U2B
NB
IB
9�B
%�B
~B

rB	��B	��B	�[B	��B	�eB	�LB	�uB	�oB	u�B	d&B	VB	G�B	DgB	9�B	/OB	(�B	*�B	IB	�B	�B	~B	 �B	�B	�B	�B	B	!B	$@B	'�B	(�B	(�B	)*B	3hB	1'B	3�B	:*B	>BB	B�B	J�B	P}B	Q�B	P�B	N�B	R�B	\�B	^�B	bNB	b�B	j�B	oiB	t�B	s�B	j�B	m�B	r�B	t�B	s�B	s�B	s�B	t�B	v+B	w2B	z�B	~(B	��B	��B	��B	��B	�0B	��B	��B	��B	�6B	��B	�0B	��B	�eB	��B	��B	��B	��B	��B	�'B	��B	�XB	�B	�B	�FB	��B	��B	��B	��B	�UB	��B	��B	�gB	ŢB	�EB	�B	��B	�B	�EB	ǮB	�zB	�XB	��B	̘B	�6B	��B	�BB	�pB	��B	�}B	�<B	�6B	�BB	�&B	��B	��B	ӏB	֡B	՛B	�yB	�B	�B	�#B	�B	��B	�|B	ߤB	�jB	�B	�NB	�NB	ޞB	�jB	��B	��B	�QB	�B	��B	��B	��B	�8B	�8B	��B	�B	��B	��B	�8B	�B	�fB	�B	�`B	�ZB	��B	�B	�B	�+B	��B	�2B	�B	�lB	��B	��B	��B	�JB	��B	�B	�B	��B	�VB	��B	�(B	��B	�PB	�VB	��B	��B	�.B	��B	��B	��B	��B	��B
  B	��B	�VB	��B
B
�B
�B
YB
%B
YB
DB
JB

�B
�B
	�B
	B
�B
�B
�B
�B
B
B
�B
B
oB
�B
�B
�B
@B
�B
�B
B
�B
"B
	�B
+B
_B
_B
B
SB
_B

�B
"B
�B
SB
B
B
�B
�B
1B
�B
MB
�B
B
B
B
B
B
:B
:B
:B
4B
bB
�B
�B
�B
�B
�B
 B
hB
 B
4B
B
B
FB
{B
{B
�B
FB
�B
FB
B
�B
+B
�B
B
B
�B
xB
IB
 'B
 �B
"�B
#�B
#B
#B
#nB
$tB
$�B
$�B
$�B
%B
&B
&�B
'B
'�B
)�B
(�B
(�B
(�B
)*B
($B
'�B
'�B
)_B
)_B
+kB
.IB
/�B
0UB
0�B
0�B
0�B
0UB
0!B
/�B
0�B
1[B
0�B
1'B
1�B
2-B
4�B
4nB
5B
5�B
6zB
6B
6zB
6zB
6�B
6�B
6�B
6�B
7�B
7�B
7�B
7�B
7�B
8�B
9XB
:^B
:*B
:^B
:�B
:�B
:�B
;�B
;�B
=qB
=�B
=�B
=�B
>wB
>wB
?�B
?�B
?}B
?}B
@�B
@OB
A�B
A�B
A�B
C-B
CaB
C�B
D3B
DgB
D�B
DgB
D�B
D�B
E9B
FB
F�B
G�B
HB
H�B
H�B
IB
IB
H�B
IB
IB
I�B
J#B
I�B
J�B
J�B
K)B
K)B
J�B
K�B
K�B
K�B
K�B
L0B
K�B
L0B
L0B
L�B
L�B
L�B
M6B
M6B
M6B
MB
MB
MB
MjB
M�B
M�B
M�B
N<B
OB
OBB
OvB
OvB
OBB
N�B
OB
OB
OBB
OB
OB
P�B
P�B
PHB
P}B
P}B
QB
P�B
P�B
PHB
O�B
O�B
N�B
N�B
N<B
K�B
LdB
LdB
LdB
NB
NB
NB
NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�7�@�9XB
A�B
A�B
?}B
@�B
A B
A B
CaB
AUB
A�B
?B
A B
>�B
?B
B�B
=�B
@�B
A B
>�B
B[B
>wB
>�B
@�B
=�B
A B
@B
@B
A�B
@OB
@B
@�B
@�B
?�B
AUB
HKB
G�B
G�B
H�B
L0B
K�B
OBB
NB
R�B
WsB
V�B
Z�B
c�B
zxB
ncB
�GB
��B
�tB
�B
��B
��B
��B
�IB
�!B
�wB
��B
��B
�B
��B
�UB
��B
��B
�UB
�OB
�[B
��B
��B
�-B
��B
�UB
�3B
��B
�hB
�tB
�-B
�3B
�?B
�aB
�B
��B
��B
�aB
��B
��B
��B
��B
�3B
�hB
��B
�zB
�$B
��B
��B
�qB
�wB
�OB
�BB
�B
��B
�HB
�B
� B
�aB
�tB
�B
ƨB
��B
ȀB
�0B
�pB
�<B
ϫB
�vB
��B
�pB
��B
͟B
�B
�B
�HB
� B
�TB
�HB
�}B
�B
�B
��B
�HB
�2B
�mB
֡B
�,B
��B
�9B
��B
՛B
��B
�mB
��B
�KB
�B
��B
ٴB
�EB
�B
�QB
�KB
�]B
��B
ٴB
چB
�]B
ںB
��B
ޞB
ݘB
ޞB
�TB
�HB
�B
�B
�B
�ZB
�B
�B
�,B
�B
��B
�B
��B
�8B
�B
��B
�mB
�B
�yB
�
B
�B
�QB
��B
�B
�B
��BB�B �B%�B'�B%�B=<BI�BQBo�B��B��B��B�-B��B�-B�RB��B�'B�hB��B�LB��B�?B��B��B��B�6B��B��B�'B�'B��BBÖB�[BĜB�9B�3B��B��B�B�B�#BʌB�)B�)B�B�pB�6B͟BϫB�}BΥB�vB��B�}B��B�BѷB�HBуB�[B��B�,B՛B՛B��B�gBٴB��B� B�`B�B��B�&B�,B��B�&B��B��B��B�fB�B�fB�B�>B��B�mB�B�B�B�8B�B�>B�mB�yB�B�B�B�B��B�)B��B�cB�B�B�vB�B�B�B�iB�B�vB��B�;B�B�B�;B�oB�B�B��B�B��B�MB�ZB��B�B�B��B�MB�B��B�xB�B��B  BGB iB �B�BuB�B�BfB	7B	7B	B�B�B1B�BYB1B1BYB�B1B�B�B	�B	�B�B
=B	7B_B�B
�BPB	lB�B�B(B�B�B�B�B#�B~BB�BIB�BBIBxB�B!BBxBOB!�B�B�B �B �BB�B!B!-B�B!bB~B=B!bB/OB0!B:�B1'B:�B@�BPHB\]BU�B@BB[BI�BC�BAUBEBC�BFtBGBGEBA B@�BA�B?HBA BA�B>wB?�B@B?�B>BB@B@OB?HB?B?}B>wB>B@�B<�B<�B>BB>�B<B=�B=�B:�B;�B<�B<jB:�B<B<B:�B:�B<�B<�B9�B9�B8�B:�B7�B:^B8�B9�B5tB4B33B5�B4nB2aB0�B2-B3�B<6B3hB7LB6B49B;0B1�B>�B<6B:�B6FB;�B6B=B5B8�B7LBC�BG�BCaB@BH�BC�BA�B?�B?�BC�BA�BA�B>�BB�BC�BEmBN�BF?BE9BEBGBI�BJ�BS[BOBF?BJ�BLdBNpBM�BN<BL0BK�BOvBL0BOBBK�BN�BM�BLdBK�BS�BNBQ�BS�BV�BNBR BR�BQNBS�BR�BQBQ�BS�BP�BP�BT�BR BRTBQ�BPBOvBP�BO�BN�BbBVmBZQBYBV�BT�BU�BWsBYKBjBZ�B[�B\]B[�BYB\�B]�B[�BaHBbNB�oB]dBZ�B`�B_Bd�B]�B`B^5B_;B_�B_B`�BU�BdZBe�Bk�BgmBe�Bc�Be�Bc�B`B`�B]/B`BBX�B[WBaHB`BBS�BT�BW�B\�BT,BUgBQNBS[BRTBR�BO�BM�BOBN�BNBP�BN�BK)BN�BQ�BW
BR BJ#BI�BIRBL0BGzBK)BR�BJ�BJ�BLdBO�BO�BT�BIBD3B4�B2�B,=B+�B'�B'B$�B&�B$B$�B"�B!�B"4B!bB!bB�BOB�B"4B"�B�BBSB�B+B!�B%zB�B:B�B1B+B1B�BABGB�B�B��B�"B�B{B�B�B�B�B��B�MB�B��B�B�AB��B�B�B�"B��B�|B�8B�B�`B�
B��B�B�vB��B�B��B��BخB�/B�]B��B�mBݘB�MB�dBŢB�EB��B�XB��B��B��B��B��B�0B�_B�_B�zB��B��B��B��B�B�hB��B�YB��B�B�PB��B��B��B�=B�SB��B��By	By	B��B|�BzB`BX�Br|Bc�BW�B`Be�BZ�BUgBV�BncBOvBR�BPHBM�BS�BR BVBMBL�BI�BK^BH�BI�BK^BOBBCaB@BG�B>wB>wBI�BZQB'�B8RBL�B-B.�B1�B@B �B%FB5tB$@B)_B�B#BxB�BB1B�BB6�B�B�BBoB iB;BB �B
��B
�>B
�8B
�8B
�PB
��B
�2B
��B
�2B
�B
�GB
�B
�B
�vB
�%B
�B
�B
�B
�fB
�
B
�B
�TB
��B
��B
��B
�|4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                 4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022021408084320220214080843IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021513005220220215130052QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021513005220220215130052QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194320230210131943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                