CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-03-01T20:16:06Z creation; 2022-07-12T13:01:51Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         iPRIMARY | https://orcid.org/0000-0001-5113-1068 | Deborah West-Mack, Woods Hole Oceanographic Institution         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7d   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7t   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7x   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7|   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8<   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  8�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9l   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9t   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9x   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  9�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :8   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :P   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :T   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :d   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :t   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        <�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    <�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    <�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  <�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  d    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʴ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ҙ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � A   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210301201606  20220712090151  1902223 1902223 US ARGO PROJECT                                                 US ARGO PROJECT                                                 BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         PRES            TEMP            PSAL            PRES            TEMP            PSAL               -   -AA  AOAO7993                            7993                            2C  2C  DD  S2A                             S2A                             7529                            7529                            SBE602 15Aug17 ARM V2.4         SBE602 15Aug17 ARM V2.4         854 854 @�x���@�x���11  @�x-��@@�x-��@�DgP��{��DgP��{�@DwX�e,@DwX�e,11  GPS     GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                        Near-surface sampling: discrete, pumped [data sampled at 1.0Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�\@@  @�  @��\@�G�@�p�A ��A�RA#�
AAG�A^�RA~�RA���A�\)A�  A��A�\)A�\)A�  B (�B�
B  B�
B�B'�B0(�B8Q�B@  BH  BP  BX(�B`(�Bh  Bo�
Bw�
B�  B�{B��B�  B�  B�  B�  B��B�  B�{B�  B��B�  B�{B�{B�{B�  B�{B�  B�{B�{B�  B�{B�{B�  B�  B�  B�  B��B�  B�  B�  C   C  C  C��C  C

=C  C  C  C  C��C  C  C  C  C  C��C!��C$  C&  C(  C)��C,  C.  C/��C1��C3��C6  C8  C:  C<  C>  C@  CB
=CD  CE��CH  CJ  CL  CN  CP  CR
=CS��CV  CX
=CY��C\  C^  C`  Cb  Cd  Cf  Ch  Ci��Ck��Cn  Cp  Cr
=Ct
=Cv  Cw��Cy��C|  C~  C�  C�C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���C���C�  C�  C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C���C���C�C�C�C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�C�C�C�  C���C�  C�C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�C�C�C�C�  C���C���C���C�  C�  C���C�  C�C�C�C�C�C�C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C���C�  C�C�C�  C���C���C���C�C�  C�  C�  C�  C���C���C�  C�  C�C�  C���C�C�D   D }qD �qD� D�D� D�D� D  D� D  D� D  D� D  D}qD�qD� D�qD	}qD
  D
� D  D��D�qD}qD  D� D  D}qD�qD}qD  D� D�qD��D  D� D�D��D  D� D�D��D  D� D�D� D  D� D  D� D  D� D�D� D  D� D�qD� D  D� D  D}qD�qD ��D!�D!� D!�qD"}qD#  D#��D$  D$}qD%�D%� D%�qD&}qD&�qD'��D(  D(� D)  D)}qD*  D*� D+�D+� D+�qD,}qD-  D-}qD.  D.� D.�qD/}qD/�qD0}qD1  D1��D2�D2� D2��D3}qD4  D4��D5  D5}qD5��D6}qD6�qD7}qD7�qD8� D8�qD9z�D9�qD:� D;  D;}qD<  D<��D=�D=��D>  D>}qD>�qD?� D@�D@� D@��DA� DB  DBz�DB�qDC� DD�DD��DE  DEz�DF  DF��DG  DG� DH�DH��DI�DI� DJ  DJ}qDK  DK��DK�qDL}qDM  DM}qDN  DN� DO  DO��DP  DP� DQ�DQ� DQ�qDR}qDS  DS� DS�qDT}qDU  DU}qDU�qDV� DW�DW� DW�qDX� DY�DY� DY�qDZ��D[�D[� D[�qD\}qD]  D]��D^�D^��D^�qD_z�D`  D`��Da  Da}qDb  Db��Dc�Dc}qDc�qDd}qDd�qDe� Df�Df��Dg�Dg��Dh  Dhz�Dh��Di� Dj  Dj}qDj�qDk}qDl  Dl� Dm�Dm� Dn  Dn� Do  Do� Do�qDp� Dq  Dq� Dr  Dr}qDr�qDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw}qDw�qDx� Dy  Dy��Dz  Dz� Dz�qD{}qD{�qD|� D}�D}� D~  D~}qD~�qD� D�HD�@ D�~�D���D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�  D�AHD�� D��qD���D�AHD�� D���D�  D�@ D�� D��HD�HD�@ D�~�D���D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�~�D�� D�  D�@ D��HD���D���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�� D�� D���D�>�D�� D��HD�  D�AHD�� D���D���D�>�D�� D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD���D���D�@ D�� D���D�  D�AHD�� D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD�� D���D���D�>�D�~�D���D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D�� D���D���D�@ D�� D���D���D�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�~�D���D���D�@ D�~�D¾�D���D�@ DÀ Dþ�D�  D�@ DĀ D�� D���D�@ Dŀ Dž�D���D�>�D�~�D�� D�  D�@ DǁHD��HD�  D�>�DȁHD�� D�  D�AHDɀ Dɾ�D���D�@ Dʀ D�� D�HD�@ Dˀ D��HD�  D�=qD̀ D�� D�  D�@ D́HD��HD�  D�>�D΀ Dξ�D���D�>�D�}qDϾ�D�  D�AHDЁHD��HD�  D�>�DсHD��HD�  D�@ DҁHD�� D���D�@ DӀ D�� D�  D�@ DԁHDԾ�D���D�@ DՁHD��HD�HD�AHDցHD�� D�  D�@ D׀ D�� D�  D�AHD؀ D�� D�  D�>�D�}qD�� D�HD�AHDځHD��HD�  D�AHDہHD��HD�HD�@ D܀ D�� D���D�>�D�~�Dݾ�D���D�>�D�~�D�� D�  D�>�D߀ D�� D���D�@ D��HD�� D���D�@ D�HD�� D�  D�AHD�HD⾸D�  D�AHD� D�� D�  D�AHD� D�� D�HD�>�D�~�D�� D���D�@ D� D�� D�  D�@ D�HD�� D�HD�AHD�HD�� D�HD�@ D� D�� D�  D�@ D� D�� D�HD�@ D�~�D�� D�HD�@ D� D쾸D���D�>�D� D���D���D�@ D�HDD�  D�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�>�D� D�D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D�  D�AHD�� D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��
D�/\?��?.{?L��?aG�?��?�z�?��
?�Q�?Ǯ?�
=?�@   @�@�@��@�R@(��@333@=p�@E�@O\)@W
=@c�
@n{@xQ�@}p�@��
@�=q@�{@��@�@��H@�  @��\@��@�ff@���@���@�\)@�33@�
=@���@�(�@��R@\@��
@��@���@˅@�{@У�@�33@�@�Q�@��H@޸R@�\@��@�=q@�\)@�z�@�
=@��H@��RAG�A33A�A�A	��A
=qA�A�Ap�A�RA��A�A�
AAQ�A��A�HA(�Ap�A�RA   A!�A#�
A%A&ffA'�A)��A*=qA,��A.{A0  A1G�A2�\A333A4z�A5A7
=A8Q�A:=qA;�A=p�A>�RA@  A@��AA�AC33ADz�AE�AG
=AG�AI��AJ�HAL(�AN{AO\)AP  AP  AP��AQ�AS33AU�AVffAW
=AW�AXQ�AX��AY��AZ�HA\(�A]p�A^{A`  AaG�Ab�\Ac�
Ae�AfffAhQ�Ai��Aj�HAl��An{Ao\)AqG�As33Atz�AuAw
=Ax��Az=qA{�A}p�A~{A\)A�Q�A�G�A��A��\A��A�(�A��A�A�
=A���A��A��A�(�A��
A�(�A�(�A�z�A���A��A�A�{A��RA�
=A��A�  A���A�G�A��A��\A��\A��HA��A��
A�z�A���A�p�A�A�ffA��RA�\)A��A�Q�A���A���A��A��HA�33A��A�(�A�z�A���A��A�p�A�{A��RA�
=A��A�  A�Q�A���A�G�A��A��\A�33A��
A�(�A��A�A�ffA�\)A�Q�A�G�A��\A�33A�(�A�p�A��RA�\)A�  A���A��A��HA��
A���A�A��RA��A���A���A��\A��A�z�A�A��RA��A���A���A\A��
A���A�p�A�
=A�  A�G�A�=qA�33A�z�A�AθRA�  A�G�A�=qAӅA���A�{A�\)Aأ�Aٙ�A��HA�(�A�p�A޸RA�  A�G�A��HA�(�A�p�A�RA�Q�A陚A��HA�(�A�A�
=A��A��A�A��A��RA��A�G�A��\A��A��A�ffA��B Q�B�BB=qB�HB�B(�B��B��BffB33B�
B��B	G�B	�B
�RB\)B(�B��B��BffB�HB\)B  B��BG�B�B�\B33B�
B��B�BBffB33B�
B��BG�B{B�RB\)B  B��BG�B�B�RB\)B   B ��B!p�B"{B"�RB#�B$  B$��B%G�B%�B&�RB'\)B(  B(��B)p�B)�B*�HB+�B,Q�B,��B-��B.ffB/
=B/�B0z�B1G�B1�B2�\B3\)B4  B4��B5p�B6=qB7
=B7�
B8z�B9G�B9�B:ffB;33B;�
B<z�B=�B=B>�\B?33B@  B@��BAp�BB{BB�HBC�BDQ�BD��BE��BF=qBG
=BG�BHQ�BI�BIBJ�\BK\)BL  BLz�BM�BMBNffBO
=BO�BPz�BP��BQ��BRffBS
=BS�BTQ�BT��BU��BV=qBV�RBW\)BX(�BX��BYp�BY�BZ�RB[\)B\  B\��B]G�B]B^�\B_33B_�B`z�BaG�BaBb�\Bc33Bc�
BdQ�Be�BeBfffBg
=Bg�BhQ�Bh��Bi��Bj=qBj�HBk�Bl(�Bl��Bmp�Bn{Bn�HBo�Bp(�Bp��Bqp�Br=qBr�RBs\)Bt  Bt��Bup�Bv{Bv�\Bw33Bx  Bxz�By�ByBz=qBz�HB{�B|(�B|��B}p�B}�B~�\B
=B�B�{B�Q�B���B���B��B�\)B���B��B�(�B�ffB��\B��HB�
=B�G�B��B��B��
B�{B�=qB�z�B���B��HB�
=B�33B�\)B���B�B��B�{B�=qB�ffB��\B���B���B��B�G�B��B��B��
B�  B�=qB�ffB���B���B���B��B�\)B�p�B��B��
B�  B�(�B�ffB��\B��RB���B�33B�G�B��B��B��
B�{B�=qB�ffB���B��HB���B�33B�\)B���B�B�  B�(�B�Q�B��\B��RB���B��B�G�B�p�B��B��B�{B�Q�B��\B��RB��HB��B�\)B��B�B�  B�(�B�ffB��\B���B���B�33B�p�B��B��
B�{B�Q�B��\B���B�
=B�G�B��B��B��B�(�B�Q�B��\B���B�
=B�33B��B�B��B�(�B�Q�B��\B��RB���B�33B�\)B���B��
B�{B�=qB�z�B���B���B��B�\)B��B�B��B�(�B�ffB��\B���B�
=B�G�B�p�B��B��
B�{B�Q�B��\B���B���B�33B�p�B��B��B�{B�Q�B��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B���B�
=B�33B�p�B��B��
B�{B�Q�B�z�B��RB��HB��B�G�B��B�B��B�(�B�Q�B��\B��RB���B�33B�\)B��B�B��B�(�B�Q�B�z�B���B��HB�
=B�G�B�p�B��B��
B�  B�=qB�ffB���B���B���B�33B�\)B��B�B��B�{B�Q�B�z�B��RB���B��B�\)B��B�B��B�(�B�ffB��\B���B���B�33B�p�B���B��B�{B�ffB��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B��RB���B�33B�\)B���B�B�  B�=qB�ffB���B��HB�
=B�G�B��B�B�  B�=qB�z�B���B��HB��B�G�B��B�B�  B�=qB�z�B���B��HB��B�\)B��B�B�  B�=qB�z�B���B��HB��B�\)B��B�B��B�(�B�ffB��\B���B�
=B�G�B�p�B��B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�=qB�z�B¸RB���B�33B�\)BÙ�B��B�(�B�Q�Bď\B���B���B�33BŅBŮB��B�(�B�ffBƣ�B��HB��B�\)BǙ�B��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ?�  @�\@@  @�  @��\@�G�@�p�A ��A�RA#�
AAG�A^�RA~�RA���A�\)A�  A��A�\)A�\)A�  B (�B�
B  B�
B�B'�B0(�B8Q�B@  BH  BP  BX(�B`(�Bh  Bo�
Bw�
B�  B�{B��B�  B�  B�  B�  B��B�  B�{B�  B��B�  B�{B�{B�{B�  B�{B�  B�{B�{B�  B�{B�{B�  B�  B�  B�  B��B�  B�  B�  C   C  C  C��C  C

=C  C  C  C  C��C  C  C  C  C  C��C!��C$  C&  C(  C)��C,  C.  C/��C1��C3��C6  C8  C:  C<  C>  C@  CB
=CD  CE��CH  CJ  CL  CN  CP  CR
=CS��CV  CX
=CY��C\  C^  C`  Cb  Cd  Cf  Ch  Ci��Ck��Cn  Cp  Cr
=Ct
=Cv  Cw��Cy��C|  C~  C�  C�C�  C���C���C���C�  C�  C���C�  C�  C�  C�  C���C���C�  C�  C���C�  C�  C���C�  C�C�  C�  C�  C�  C���C���C���C���C�C�C�C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�C�C�C�  C���C�  C�C�C�C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�C�C�C�C�  C���C���C���C�  C�  C���C�  C�C�C�C�C�C�C�  C�  C�  C�  C���C�  C���C�  C�  C�  C�C�  C�  C�  C�  C�  C���C�  C�C�C�  C���C���C���C�C�  C�  C�  C�  C���C���C�  C�  C�C�  C���C�C�D   D }qD �qD� D�D� D�D� D  D� D  D� D  D� D  D}qD�qD� D�qD	}qD
  D
� D  D��D�qD}qD  D� D  D}qD�qD}qD  D� D�qD��D  D� D�D��D  D� D�D��D  D� D�D� D  D� D  D� D  D� D�D� D  D� D�qD� D  D� D  D}qD�qD ��D!�D!� D!�qD"}qD#  D#��D$  D$}qD%�D%� D%�qD&}qD&�qD'��D(  D(� D)  D)}qD*  D*� D+�D+� D+�qD,}qD-  D-}qD.  D.� D.�qD/}qD/�qD0}qD1  D1��D2�D2� D2��D3}qD4  D4��D5  D5}qD5��D6}qD6�qD7}qD7�qD8� D8�qD9z�D9�qD:� D;  D;}qD<  D<��D=�D=��D>  D>}qD>�qD?� D@�D@� D@��DA� DB  DBz�DB�qDC� DD�DD��DE  DEz�DF  DF��DG  DG� DH�DH��DI�DI� DJ  DJ}qDK  DK��DK�qDL}qDM  DM}qDN  DN� DO  DO��DP  DP� DQ�DQ� DQ�qDR}qDS  DS� DS�qDT}qDU  DU}qDU�qDV� DW�DW� DW�qDX� DY�DY� DY�qDZ��D[�D[� D[�qD\}qD]  D]��D^�D^��D^�qD_z�D`  D`��Da  Da}qDb  Db��Dc�Dc}qDc�qDd}qDd�qDe� Df�Df��Dg�Dg��Dh  Dhz�Dh��Di� Dj  Dj}qDj�qDk}qDl  Dl� Dm�Dm� Dn  Dn� Do  Do� Do�qDp� Dq  Dq� Dr  Dr}qDr�qDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw}qDw�qDx� Dy  Dy��Dz  Dz� Dz�qD{}qD{�qD|� D}�D}� D~  D~}qD~�qD� D�HD�@ D�~�D���D�  D�@ D�~�D�� D���D�>�D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D���D�@ D�� D�� D�  D�AHD�� D��qD���D�AHD�� D���D�  D�@ D�� D��HD�HD�@ D�~�D���D�  D�@ D��HD�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�@ D�~�D���D���D�@ D�� D�� D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D���D���D�>�D�~�D�� D�  D�@ D��HD���D���D�>�D�~�D���D�  D�@ D�� D���D���D�@ D�� D�� D���D�>�D�� D��HD�  D�AHD�� D���D���D�>�D�� D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD��HD���D���D�@ D�� D���D�  D�AHD�� D���D���D�>�D�~�D�� D�  D�@ D��HD��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD�� D���D���D�>�D�~�D���D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�@ D�� D��HD�HD�@ D�~�D���D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD��HD�HD�@ D�~�D���D�  D�AHD�� D�� D���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D�� D���D���D�@ D�� D���D���D�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD���D�>�D�~�D���D���D�@ D�~�D¾�D���D�@ DÀ Dþ�D�  D�@ DĀ D�� D���D�@ Dŀ Dž�D���D�>�D�~�D�� D�  D�@ DǁHD��HD�  D�>�DȁHD�� D�  D�AHDɀ Dɾ�D���D�@ Dʀ D�� D�HD�@ Dˀ D��HD�  D�=qD̀ D�� D�  D�@ D́HD��HD�  D�>�D΀ Dξ�D���D�>�D�}qDϾ�D�  D�AHDЁHD��HD�  D�>�DсHD��HD�  D�@ DҁHD�� D���D�@ DӀ D�� D�  D�@ DԁHDԾ�D���D�@ DՁHD��HD�HD�AHDցHD�� D�  D�@ D׀ D�� D�  D�AHD؀ D�� D�  D�>�D�}qD�� D�HD�AHDځHD��HD�  D�AHDہHD��HD�HD�@ D܀ D�� D���D�>�D�~�Dݾ�D���D�>�D�~�D�� D�  D�>�D߀ D�� D���D�@ D��HD�� D���D�@ D�HD�� D�  D�AHD�HD⾸D�  D�AHD� D�� D�  D�AHD� D�� D�HD�>�D�~�D�� D���D�@ D� D�� D�  D�@ D�HD�� D�HD�AHD�HD�� D�HD�@ D� D�� D�  D�@ D� D�� D�HD�@ D�~�D�� D�HD�@ D� D쾸D���D�>�D� D���D���D�@ D�HDD�  D�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�>�D� D�D���D�>�D�~�D�� D�  D�>�D�~�D�� D���D�>�D�~�D�� D�  D�AHD�� D���D���D�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�~�D�� D��
D�/\?��?.{?L��?aG�?��?�z�?��
?�Q�?Ǯ?�
=?�@   @�@�@��@�R@(��@333@=p�@E�@O\)@W
=@c�
@n{@xQ�@}p�@��
@�=q@�{@��@�@��H@�  @��\@��@�ff@���@���@�\)@�33@�
=@���@�(�@��R@\@��
@��@���@˅@�{@У�@�33@�@�Q�@��H@޸R@�\@��@�=q@�\)@�z�@�
=@��H@��RAG�A33A�A�A	��A
=qA�A�Ap�A�RA��A�A�
AAQ�A��A�HA(�Ap�A�RA   A!�A#�
A%A&ffA'�A)��A*=qA,��A.{A0  A1G�A2�\A333A4z�A5A7
=A8Q�A:=qA;�A=p�A>�RA@  A@��AA�AC33ADz�AE�AG
=AG�AI��AJ�HAL(�AN{AO\)AP  AP  AP��AQ�AS33AU�AVffAW
=AW�AXQ�AX��AY��AZ�HA\(�A]p�A^{A`  AaG�Ab�\Ac�
Ae�AfffAhQ�Ai��Aj�HAl��An{Ao\)AqG�As33Atz�AuAw
=Ax��Az=qA{�A}p�A~{A\)A�Q�A�G�A��A��\A��A�(�A��A�A�
=A���A��A��A�(�A��
A�(�A�(�A�z�A���A��A�A�{A��RA�
=A��A�  A���A�G�A��A��\A��\A��HA��A��
A�z�A���A�p�A�A�ffA��RA�\)A��A�Q�A���A���A��A��HA�33A��A�(�A�z�A���A��A�p�A�{A��RA�
=A��A�  A�Q�A���A�G�A��A��\A�33A��
A�(�A��A�A�ffA�\)A�Q�A�G�A��\A�33A�(�A�p�A��RA�\)A�  A���A��A��HA��
A���A�A��RA��A���A���A��\A��A�z�A�A��RA��A���A���A\A��
A���A�p�A�
=A�  A�G�A�=qA�33A�z�A�AθRA�  A�G�A�=qAӅA���A�{A�\)Aأ�Aٙ�A��HA�(�A�p�A޸RA�  A�G�A��HA�(�A�p�A�RA�Q�A陚A��HA�(�A�A�
=A��A��A�A��A��RA��A�G�A��\A��A��A�ffA��B Q�B�BB=qB�HB�B(�B��B��BffB33B�
B��B	G�B	�B
�RB\)B(�B��B��BffB�HB\)B  B��BG�B�B�\B33B�
B��B�BBffB33B�
B��BG�B{B�RB\)B  B��BG�B�B�RB\)B   B ��B!p�B"{B"�RB#�B$  B$��B%G�B%�B&�RB'\)B(  B(��B)p�B)�B*�HB+�B,Q�B,��B-��B.ffB/
=B/�B0z�B1G�B1�B2�\B3\)B4  B4��B5p�B6=qB7
=B7�
B8z�B9G�B9�B:ffB;33B;�
B<z�B=�B=B>�\B?33B@  B@��BAp�BB{BB�HBC�BDQ�BD��BE��BF=qBG
=BG�BHQ�BI�BIBJ�\BK\)BL  BLz�BM�BMBNffBO
=BO�BPz�BP��BQ��BRffBS
=BS�BTQ�BT��BU��BV=qBV�RBW\)BX(�BX��BYp�BY�BZ�RB[\)B\  B\��B]G�B]B^�\B_33B_�B`z�BaG�BaBb�\Bc33Bc�
BdQ�Be�BeBfffBg
=Bg�BhQ�Bh��Bi��Bj=qBj�HBk�Bl(�Bl��Bmp�Bn{Bn�HBo�Bp(�Bp��Bqp�Br=qBr�RBs\)Bt  Bt��Bup�Bv{Bv�\Bw33Bx  Bxz�By�ByBz=qBz�HB{�B|(�B|��B}p�B}�B~�\B
=B�B�{B�Q�B���B���B��B�\)B���B��B�(�B�ffB��\B��HB�
=B�G�B��B��B��
B�{B�=qB�z�B���B��HB�
=B�33B�\)B���B�B��B�{B�=qB�ffB��\B���B���B��B�G�B��B��B��
B�  B�=qB�ffB���B���B���B��B�\)B�p�B��B��
B�  B�(�B�ffB��\B��RB���B�33B�G�B��B��B��
B�{B�=qB�ffB���B��HB���B�33B�\)B���B�B�  B�(�B�Q�B��\B��RB���B��B�G�B�p�B��B��B�{B�Q�B��\B��RB��HB��B�\)B��B�B�  B�(�B�ffB��\B���B���B�33B�p�B��B��
B�{B�Q�B��\B���B�
=B�G�B��B��B��B�(�B�Q�B��\B���B�
=B�33B��B�B��B�(�B�Q�B��\B��RB���B�33B�\)B���B��
B�{B�=qB�z�B���B���B��B�\)B��B�B��B�(�B�ffB��\B���B�
=B�G�B�p�B��B��
B�{B�Q�B��\B���B���B�33B�p�B��B��B�{B�Q�B��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B���B�
=B�33B�p�B��B��
B�{B�Q�B�z�B��RB��HB��B�G�B��B�B��B�(�B�Q�B��\B��RB���B�33B�\)B��B�B��B�(�B�Q�B�z�B���B��HB�
=B�G�B�p�B��B��
B�  B�=qB�ffB���B���B���B�33B�\)B��B�B��B�{B�Q�B�z�B��RB���B��B�\)B��B�B��B�(�B�ffB��\B���B���B�33B�p�B���B��B�{B�ffB��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B���B�
=B�33B�p�B��B��B�(�B�Q�B��\B��RB���B�33B�\)B���B�B�  B�=qB�ffB���B��HB�
=B�G�B��B�B�  B�=qB�z�B���B��HB��B�G�B��B�B�  B�=qB�z�B���B��HB��B�\)B��B�B�  B�=qB�z�B���B��HB��B�\)B��B�B��B�(�B�ffB��\B���B�
=B�G�B�p�B��B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�=qB�z�B¸RB���B�33B�\)BÙ�B��B�(�B�Q�Bď\B���B���B�33BŅBŮB��B�(�B�ffBƣ�B��HB��B�\)BǙ�B��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A1�mA1�mA1�mA1�mA1�mA1�;A1�A1��A1��A1��A1��A1��A2  A2A2  A2  A1�A1��A2JA2�A2$�A2�A2�A2bA2�A2E�A2E�A2A�A29XA25?A21'A2E�A2bNA2bNA2ZA2ZA2ffA2^5A2E�A2=qA2bNA2bNA2�9A2ȴA4VA5t�A4��A69XA9+A=dZA?��AA?}ABVAD�AE"�AD��AC�PAB-A@�jA@JA?�A>�uA>VA=�#A=�;A>1A>VA>bA<�yA<n�A<bNA;��A:��A:(�A:-A:1A:  A9�A9��A9��A9t�A9;dA8�RA8n�A8(�A7��A6��A6{A5�A4�A4M�A2��A2-A2  A1��A1��A1�A1S�A0�9A0VA0�A/VA.M�A,�jA,z�A,M�A+�wA+7LA*�`A*M�A*�A)��A)�7A)�PA)�hA)�A)�A)t�A)hsA)\)A)�A'dZA&��A&��A&r�A&ZA& �A%ƨA%��A%t�A%33A%%A$�9A$z�A#�wA#�A"��A!�A!G�A �A �\A VA�-A��A��Ar�A{A�AS�A��AA�AS�A�#A|�A$�A�A��AJA��AXA��A�A�FAAn�AI�AAt�AoA�;A"�AȴA�A5?AA�-A�7Al�A�HAE�A��A`BA+A��A9XA�AbA1A1A�#Ax�A7LAĜA��A�-AS�AĜA��AG�A�!A��A�PAp�A\)A&�A�A��AbNA-AbAl�A��A �A��AhsAG�A�A
�A
��A
��A
Q�A	��A	;dA�+AƨA�A�!A�AffAbA�AC�A��A��AC�A�A �/A �A @�~�@�r�@��!@���@���@�1@�S�@�n�@�hs@� �@�R@��@��@�V@�p�@�&�@�z�@�;d@�9X@��@�7L@��@�Ĝ@�D@���@�@�33@�Q�@�|�@ٺ^@�J@�o@�$�@�Q�@�@̴9@ˍP@ʰ!@�-@�hs@ȃ@ǥ�@�S�@�@ư!@��@��@�+@���@�t�@���@��@� �@��;@���@��@�\)@��@���@�@�I�@�K�@�5?@��j@�
=@�?}@�;d@�^5@���@���@�9X@���@��@���@�%@�%@��9@��u@�9X@�+@�/@��u@�Q�@�|�@��#@�`B@�7L@��j@���@�J@���@��@���@�A�@�K�@���@��@�^5@�z�@�ȴ@�J@��@�@�p�@�7L@�%@��`@���@��9@���@�A�@��
@�C�@�$�@�/@��@�V@��@��@��@�  @���@�l�@�K�@�K�@�;d@�;d@�;d@�33@��@��y@��@��@��F@��@�Q�@���@���@�&�@�/@��@�X@���@��#@���@��^@��T@�{@�n�@��y@�;d@�dZ@���@��
@�  @�Q�@�r�@�b@��;@���@��@�n�@�5?@��T@��@���@��@��@�Z@��m@��w@��@�@���@��@��@���@�V@�{@�J@�@���@�x�@��7@���@���@���@���@���@�G�@���@��j@�z�@�Q�@��u@��j@�Ĝ@���@�Ĝ@��j@��9@�Q�@��F@��@�@���@�V@�{@�@���@�j@�9X@�(�@;d@~ff@}�T@}�h@}O�@}@~V@~ȴ@~ȴ@~�+@}��@}/@|��@|�@|�D@|j@|9X@{ƨ@{C�@{@y��@y%@x��@xĜ@x�9@x�@xA�@w�w@w�w@u@t�@t9X@s��@s"�@r�\@r=q@r�!@r��@s33@t��@u�@v�@x  @y7L@{S�@|(�@{��@{S�@z��@z��@zJ@y&�@x1'@w�@w�@w�@w�@v��@v�y@v�y@vȴ@v�R@v��@vff@vE�@u�@u/@t�/@t��@t��@tz�@tI�@s�
@st�@sS�@s"�@r�@r��@rM�@q��@q��@qX@q&�@p��@p�9@p�@pbN@pbN@pb@o�@nȴ@n�+@n{@m��@m/@lj@k33@h�`@f��@e�h@e�@dZ@d�@cS�@c"�@aG�@aX@ax�@a��@a�#@a�@a��@a��@a��@`�u@`r�@`��@a�@b~�@b�H@c@b�!@a��@\I�@XbN@S�
@R~�@R-@R�@Q��@O�@M�T@KC�@I��@G�@G�w@G�w@G�w@G��@GK�@F�y@E�h@E�@D��@D�j@D��@Dj@C�m@Cƨ@C�
@EO�@E�@D(�@Dj@C�
@B�\@B-@A�#@A�#@A��@A��@A��@A�^@A�7@AG�@@�`@@��@@��@@�9@@r�@@1'@?�@?�;@?��@?��@?��@?��@?�@?\)@?+@@  @@�@A&�@AX@AX@A7L@AG�@A�^@A�#@A�#@BJ@A�@A��@B�@B�@BJ@A��@A�7@Ahs@A7L@A%@@Ĝ@@ �@?�@?�@?�;@?�P@>��@>{@=�@=@=�-@=��@=�@=/@=V@<�@<�D@<z�@<Z@<(�@<1@;��@;�@;S�@;33@:�@:�@:�@:��@:�!@:~�@:n�@:=q@9��@9��@9�@8�`@8�`@8��@8��@8�9@8bN@81'@8 �@8 �@8 �@8 �@8b@8  @7�;@7��@7�@7|�@7\)@7+@7+@7
=@6�@6�R@6�R@6��@6��@6ȴ@6��@6ȴ@6�@6ȴ@6ȴ@6ȴ@6ȴ@6ȴ@6�@6ȴ@6ȴ@6ȴ@6ȴ@6ȴ@6�R@6�R@6��@6��@6v�@6v�@6v�@6V@6$�@6$�@5�@5�T@5��@5@5@5@5@5��@5`B@5?}@5O�@5O�@5O�@5O�@5V@4�/@4�j@4�j@4�j@4�@4j@4j@4Z@49X@4�@41@3�m@3�m@3ƨ@3ƨ@3ƨ@3��@333@3@3@3@2��@2��@2~�@2=q@1��@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�#@1�#@1��@1�^@1��@1G�@1�@1%@0��@0�`@0��@0�9@0��@0�u@0�u@0�u@0�@0�@0�@0r�@0r�@0r�@0bN@0Q�@01'@0  @/�@/��@/�w@/��@/��@/��@/�P@/�P@/|�@/l�@/l�@/�P@/�P@/|�@/�P@/l�@/+@/
=@/
=@/
=@/
=@/
=@/
=@/�@/�@/�@/
=@.��@.�@.�R@.�+@.v�@.ff@.E�@.E�@.V@.ff@.ff@.ff@.ff@.ff@.v�@.�+@.��@.��@.��@.�R@.ȴ@.��@/+@/K�@/K�@/K�@/K�@/;d@/�@/
=@/
=@.ȴ@.�@.�@.�@.ȴ@.�R@.��@.v�@.ff@.E�@.5?@.@.@.@-�T@-�T@-�@.{@.$�@.@-@-��@-�h@-�@-�@-p�@-`B@-`B@-?}@-�@-V@-V@-V@-V@,��@,��@,��@,��@,��@,�/@,��@,��@,�j@,�j@,�@,�D@,�D@,z�@,j@,z�@,z�@,j@,Z@,Z@,Z@,j@,j@,j@,j@,j@,j@,j@,9X@,1@+��@+��@+��@+��@+��@+��@+��@+��@+��@+�
@+�F@+��@+��@+��@+��@+��@+��@+��@+t�@+dZ@+S�@+C�@+C�@+33@+o@*�H@*��@*�!@*�!@*�!@*��@*��@*�\@*�\@*n�@*M�@*-@*J@)��@)�@)�@)��@)�^@)��@)�7@)x�@)G�@)&�@)�@)%@)%@(�`@(Ĝ@(�@(bN@(bN@( �@(  @(  @'�@'�@'�@'�@'�@'�@'�@'�;@'�;@'��@'��A1�TA1�TA1�TA1�mA1�mA1�mA1�A1�mA1�A1�A1�A1�TA1�A1�A1�mA1�TA1�mA1�mA1�TA1�mA1�A1�A1�A1�A1�A1�mA1�mA1�mA1�TA1�TA1�mA1�mA1�mA1�mA1�mA1�TA1�TA1�A1�TA1�;A1�TA1�TA1�;A1�TA1�TA1�TA1�;A1�#A1�#A1�;A1�TA1�;A1�TA1�TA1�mA1�mA1�A2  A2  A1��A1��A1��A1��A2  A1��A1��A2  A2  A1��A1��A1��A1��A1��A1��A1��A1��A2  A2  A1��A1��A2  A1��A1��A1��A2  A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1�A1�A1�A1�A1�A1�A1�A1�A1��A1��A1��A2  A1��A1��A1��A1��A1�A1��A1��A1��A1��A1��A1��A1�A1��A1�A1�A1�A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1�A1��A1��A1��A2  A2A2  A1��A1��A1��A1��A1��A1��A2A2A2  A2A2A2  A2A2A2  A2A2A2  A2  A2A2A2A2A21A21A2JA2bA2JA2JA2JA2A2  A2A2  A1��A1��A2A2  A2  A21A2  A1��A2  A2  A2  A2  A2  A2  A2  A2A21A21A2JA2A2A21A2A2  A2A2  A2  A2A2  A1��A1��A1��A1��A2A2A2  A2A2A2  A2  A2  A1��A2A2A2  A2A21A21A2A2A2  A2  A2A21A2A1��A1�A1�A1�A1�A1�A1�A1�A1�A1�A1�mA1�TA1�mA1�A1�A1�A1�A1�A1�A1��A2  A2  A2  A1��A1��A1��A1�A1�A1�A1�A1�A1�A1��A2  A2  A2{A21A1��A2A21A2JA21A2JA2�A2�A2�A2�A2�A2�A2�A2�A2{A2{A2�A2�A2�A2�A2�A2{A2�A2�A2�A21'A21'A2-A21'A21'A2-A2$�A2(�A2�A2�A2�A2�A2bA2�A2�A2{A2 �A2(�A2$�A2$�A2(�A2 �A2�A2�A2�A2{A2{A2�A2bA21A21A21A21A21A21A2A21A2JA2JA2JA2{A2-A2$�A2$�A2{A2{A2�A2{A2{A2�A2{A2bA2�A2�A2 �A2$�A21'A2E�A2I�A2M�A2M�A2I�A2I�A2Q�A2E�A2E�A2E�A2E�A2I�A2M�A2E�A2A�A2I�A2E�A2E�A2I�A2E�A2E�A2E�A2A�A2E�A2E�A2A�A2=qA2M�A2M�A2A�A2A�A2A�A29XA2=qA29XA25?A25?A25?A25?A2(�A2-A2=qA2=qA2A�A2A�A2E�A2A�A2I�A29XA21'A29XA21'A2 �A2(�A2-A2(�A2(�A2(�A2-A29XA2=qA29XA25?A21'A21'A25?A25?A21'A25?A29XA2=qA25?A2=qA2=qA21'A25?A2A�A2VA2ffA2jA2bNA2ffA2jA2ZA2M�A2ZA2Q�A2bNA2v�A2r�A2jA2ffA2ffA2^5A2^5A2bNA2^5A2^5A2bNA2bNA2bNA2jA2ffA2ffA2jA2ffA2ZA2ZA2I�A2VA2Q�A2VA2^5A2ffA2ffA2^5A2^5A2ZA2ZA2VA2Q�A2ZA2M�A2^5A2ZA2VA2bNA2jA2bNA2bNA2ffA2ffA2n�A2n�A2n�A2n�A2r�A2bNA2ZA2Q�A2VA2^5A2n�A2jA2jA2jA2n�A2n�A2v�A2z�A2r�A2r�A2n�A2bNA2^5A2bNA2VA2ZA2M�A2VA2VA2I�A2M�A2E�A2E�A2M�A2M�A2I�A2Q�A2I�A2I�A2M�A2Q�A2VA2M�A2M�A2M�A2Q�A2A�A2=qA2E�A2I�A29XA2=qA21'A21'A2-A21'A2-A2(�A2-A2-A2(�A21'A2E�A2VA2A�A2-A2(�A2-A2-A29XA2A�A2I�A2=qA2Q�A2ZA2ZA2ZA2ZA2VA2ZA2^5A2^5A2^5A2ffA2n�A2n�A2n�A2bNA2n�A2jA2bNA2ffA2ffA2^5A2bNA2ZA2M�A2ZA2n�A2n�A2jA2jA2Q�A2I�A2ZA2Q�A2I�A2VA2bNA2^5A2bNA2ffA2ffA2z�A2��A2��A2��A2��A2�jA2��A2ĜA2��A2ȴA2ĜA2ĜA2ȴA2��A2�RA2�DA2�DA2��A2��A2��A2�uA2��A2ĜA2��A2ĜA2ĜA2ȴA2ȴA2�jA2ȴA2ĜA2�9A2�9A2�jA2��A2��A2�A3/A3/A3�wA41A3�TA4JA3�hA3�A3�^A3\)A3l�A3��A4�A4��A5K�A5?}A5oA5/A5C�A5��A5��A6{A5�
A5�PA5�hA5+A5�7A5�PA5�
A5��A5t�A5�hA5��A5O�A57LA5�A5A4ȴA4�jA5`BA5\)A5oA4^5A4��A4 �A4$�A41A4VA4(�A45?A4VA45?A4��A4��A4�RA4��A533A5l�A5%A4v�A3��A4{A45?A4^5A4��A5VA5��A6bNA6z�A6��A6�jA6��A6�\A7C�A7;dA6�`A7"�A8^5A9%A8��A8�jA8�A8�/A8��A8�!A8�+A8ȴA8��A9C�A9O�A9�hA9�7A9C�A9t�A9hsA9�A9�FA9A;\)A=&�A=�hA=��A=hsA>  A>E�A> �A=�^A=��A=S�A=+A=S�A=;dA=O�A=K�A=K�A=XA=\)A=dZA=|�A=�FA?oA?K�A?C�A?K�A?+A?7LA?K�A?`BA@�AAVAA�AA+A@�`AA?}AA7LAA7LAA%AA+AAG�AAO�AAS�AAO�AAO�AAG�AA;dAA33AA33AA/AA7LAAG�AAG�AAK�AAK�AAS�AA�AA��AA�wAA��AA�TAA�AA��AB�AB�ABv�AB�uAB�9AB��AB��AC?}AC��AC��AC�-AC��AD �AD9XAD�AD�AD�`AD��AD��AD�AE%AE?}AEt�AEt�AEx�AE|�AE�7AE��AE��AE�AE`BAEK�AE�AEoAEoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             A1�mA1�mA1�mA1�mA1�mA1�;A1�A1��A1��A1��A1��A1��A2  A2A2  A2  A1�A1��A2JA2�A2$�A2�A2�A2bA2�A2E�A2E�A2A�A29XA25?A21'A2E�A2bNA2bNA2ZA2ZA2ffA2^5A2E�A2=qA2bNA2bNA2�9A2ȴA4VA5t�A4��A69XA9+A=dZA?��AA?}ABVAD�AE"�AD��AC�PAB-A@�jA@JA?�A>�uA>VA=�#A=�;A>1A>VA>bA<�yA<n�A<bNA;��A:��A:(�A:-A:1A:  A9�A9��A9��A9t�A9;dA8�RA8n�A8(�A7��A6��A6{A5�A4�A4M�A2��A2-A2  A1��A1��A1�A1S�A0�9A0VA0�A/VA.M�A,�jA,z�A,M�A+�wA+7LA*�`A*M�A*�A)��A)�7A)�PA)�hA)�A)�A)t�A)hsA)\)A)�A'dZA&��A&��A&r�A&ZA& �A%ƨA%��A%t�A%33A%%A$�9A$z�A#�wA#�A"��A!�A!G�A �A �\A VA�-A��A��Ar�A{A�AS�A��AA�AS�A�#A|�A$�A�A��AJA��AXA��A�A�FAAn�AI�AAt�AoA�;A"�AȴA�A5?AA�-A�7Al�A�HAE�A��A`BA+A��A9XA�AbA1A1A�#Ax�A7LAĜA��A�-AS�AĜA��AG�A�!A��A�PAp�A\)A&�A�A��AbNA-AbAl�A��A �A��AhsAG�A�A
�A
��A
��A
Q�A	��A	;dA�+AƨA�A�!A�AffAbA�AC�A��A��AC�A�A �/A �A @�~�@�r�@��!@���@���@�1@�S�@�n�@�hs@� �@�R@��@��@�V@�p�@�&�@�z�@�;d@�9X@��@�7L@��@�Ĝ@�D@���@�@�33@�Q�@�|�@ٺ^@�J@�o@�$�@�Q�@�@̴9@ˍP@ʰ!@�-@�hs@ȃ@ǥ�@�S�@�@ư!@��@��@�+@���@�t�@���@��@� �@��;@���@��@�\)@��@���@�@�I�@�K�@�5?@��j@�
=@�?}@�;d@�^5@���@���@�9X@���@��@���@�%@�%@��9@��u@�9X@�+@�/@��u@�Q�@�|�@��#@�`B@�7L@��j@���@�J@���@��@���@�A�@�K�@���@��@�^5@�z�@�ȴ@�J@��@�@�p�@�7L@�%@��`@���@��9@���@�A�@��
@�C�@�$�@�/@��@�V@��@��@��@�  @���@�l�@�K�@�K�@�;d@�;d@�;d@�33@��@��y@��@��@��F@��@�Q�@���@���@�&�@�/@��@�X@���@��#@���@��^@��T@�{@�n�@��y@�;d@�dZ@���@��
@�  @�Q�@�r�@�b@��;@���@��@�n�@�5?@��T@��@���@��@��@�Z@��m@��w@��@�@���@��@��@���@�V@�{@�J@�@���@�x�@��7@���@���@���@���@���@�G�@���@��j@�z�@�Q�@��u@��j@�Ĝ@���@�Ĝ@��j@��9@�Q�@��F@��@�@���@�V@�{@�@���@�j@�9X@�(�@;d@~ff@}�T@}�h@}O�@}@~V@~ȴ@~ȴ@~�+@}��@}/@|��@|�@|�D@|j@|9X@{ƨ@{C�@{@y��@y%@x��@xĜ@x�9@x�@xA�@w�w@w�w@u@t�@t9X@s��@s"�@r�\@r=q@r�!@r��@s33@t��@u�@v�@x  @y7L@{S�@|(�@{��@{S�@z��@z��@zJ@y&�@x1'@w�@w�@w�@w�@v��@v�y@v�y@vȴ@v�R@v��@vff@vE�@u�@u/@t�/@t��@t��@tz�@tI�@s�
@st�@sS�@s"�@r�@r��@rM�@q��@q��@qX@q&�@p��@p�9@p�@pbN@pbN@pb@o�@nȴ@n�+@n{@m��@m/@lj@k33@h�`@f��@e�h@e�@dZ@d�@cS�@c"�@aG�@aX@ax�@a��@a�#@a�@a��@a��@a��@`�u@`r�@`��@a�@b~�@b�H@c@b�!@a��@\I�@XbN@S�
@R~�@R-@R�@Q��@O�@M�T@KC�@I��@G�@G�w@G�w@G�w@G��@GK�@F�y@E�h@E�@D��@D�j@D��@Dj@C�m@Cƨ@C�
@EO�@E�@D(�@Dj@C�
@B�\@B-@A�#@A�#@A��@A��@A��@A�^@A�7@AG�@@�`@@��@@��@@�9@@r�@@1'@?�@?�;@?��@?��@?��@?��@?�@?\)@?+@@  @@�@A&�@AX@AX@A7L@AG�@A�^@A�#@A�#@BJ@A�@A��@B�@B�@BJ@A��@A�7@Ahs@A7L@A%@@Ĝ@@ �@?�@?�@?�;@?�P@>��@>{@=�@=@=�-@=��@=�@=/@=V@<�@<�D@<z�@<Z@<(�@<1@;��@;�@;S�@;33@:�@:�@:�@:��@:�!@:~�@:n�@:=q@9��@9��@9�@8�`@8�`@8��@8��@8�9@8bN@81'@8 �@8 �@8 �@8 �@8b@8  @7�;@7��@7�@7|�@7\)@7+@7+@7
=@6�@6�R@6�R@6��@6��@6ȴ@6��@6ȴ@6�@6ȴ@6ȴ@6ȴ@6ȴ@6ȴ@6�@6ȴ@6ȴ@6ȴ@6ȴ@6ȴ@6�R@6�R@6��@6��@6v�@6v�@6v�@6V@6$�@6$�@5�@5�T@5��@5@5@5@5@5��@5`B@5?}@5O�@5O�@5O�@5O�@5V@4�/@4�j@4�j@4�j@4�@4j@4j@4Z@49X@4�@41@3�m@3�m@3ƨ@3ƨ@3ƨ@3��@333@3@3@3@2��@2��@2~�@2=q@1��@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�@1�#@1�#@1��@1�^@1��@1G�@1�@1%@0��@0�`@0��@0�9@0��@0�u@0�u@0�u@0�@0�@0�@0r�@0r�@0r�@0bN@0Q�@01'@0  @/�@/��@/�w@/��@/��@/��@/�P@/�P@/|�@/l�@/l�@/�P@/�P@/|�@/�P@/l�@/+@/
=@/
=@/
=@/
=@/
=@/
=@/�@/�@/�@/
=@.��@.�@.�R@.�+@.v�@.ff@.E�@.E�@.V@.ff@.ff@.ff@.ff@.ff@.v�@.�+@.��@.��@.��@.�R@.ȴ@.��@/+@/K�@/K�@/K�@/K�@/;d@/�@/
=@/
=@.ȴ@.�@.�@.�@.ȴ@.�R@.��@.v�@.ff@.E�@.5?@.@.@.@-�T@-�T@-�@.{@.$�@.@-@-��@-�h@-�@-�@-p�@-`B@-`B@-?}@-�@-V@-V@-V@-V@,��@,��@,��@,��@,��@,�/@,��@,��@,�j@,�j@,�@,�D@,�D@,z�@,j@,z�@,z�@,j@,Z@,Z@,Z@,j@,j@,j@,j@,j@,j@,j@,9X@,1@+��@+��@+��@+��@+��@+��@+��@+��@+��@+�
@+�F@+��@+��@+��@+��@+��@+��@+��@+t�@+dZ@+S�@+C�@+C�@+33@+o@*�H@*��@*�!@*�!@*�!@*��@*��@*�\@*�\@*n�@*M�@*-@*J@)��@)�@)�@)��@)�^@)��@)�7@)x�@)G�@)&�@)�@)%@)%@(�`@(Ĝ@(�@(bN@(bN@( �@(  @(  @'�@'�@'�@'�@'�@'�@'�@'�;@'�;@'��@'��A1�TA1�TA1�TA1�mA1�mA1�mA1�A1�mA1�A1�A1�A1�TA1�A1�A1�mA1�TA1�mA1�mA1�TA1�mA1�A1�A1�A1�A1�A1�mA1�mA1�mA1�TA1�TA1�mA1�mA1�mA1�mA1�mA1�TA1�TA1�A1�TA1�;A1�TA1�TA1�;A1�TA1�TA1�TA1�;A1�#A1�#A1�;A1�TA1�;A1�TA1�TA1�mA1�mA1�A2  A2  A1��A1��A1��A1��A2  A1��A1��A2  A2  A1��A1��A1��A1��A1��A1��A1��A1��A2  A2  A1��A1��A2  A1��A1��A1��A2  A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1�A1�A1�A1�A1�A1�A1�A1�A1��A1��A1��A2  A1��A1��A1��A1��A1�A1��A1��A1��A1��A1��A1��A1�A1��A1�A1�A1�A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1��A1�A1��A1��A1��A2  A2A2  A1��A1��A1��A1��A1��A1��A2A2A2  A2A2A2  A2A2A2  A2A2A2  A2  A2A2A2A2A21A21A2JA2bA2JA2JA2JA2A2  A2A2  A1��A1��A2A2  A2  A21A2  A1��A2  A2  A2  A2  A2  A2  A2  A2A21A21A2JA2A2A21A2A2  A2A2  A2  A2A2  A1��A1��A1��A1��A2A2A2  A2A2A2  A2  A2  A1��A2A2A2  A2A21A21A2A2A2  A2  A2A21A2A1��A1�A1�A1�A1�A1�A1�A1�A1�A1�A1�mA1�TA1�mA1�A1�A1�A1�A1�A1�A1��A2  A2  A2  A1��A1��A1��A1�A1�A1�A1�A1�A1�A1��A2  A2  A2{A21A1��A2A21A2JA21A2JA2�A2�A2�A2�A2�A2�A2�A2�A2{A2{A2�A2�A2�A2�A2�A2{A2�A2�A2�A21'A21'A2-A21'A21'A2-A2$�A2(�A2�A2�A2�A2�A2bA2�A2�A2{A2 �A2(�A2$�A2$�A2(�A2 �A2�A2�A2�A2{A2{A2�A2bA21A21A21A21A21A21A2A21A2JA2JA2JA2{A2-A2$�A2$�A2{A2{A2�A2{A2{A2�A2{A2bA2�A2�A2 �A2$�A21'A2E�A2I�A2M�A2M�A2I�A2I�A2Q�A2E�A2E�A2E�A2E�A2I�A2M�A2E�A2A�A2I�A2E�A2E�A2I�A2E�A2E�A2E�A2A�A2E�A2E�A2A�A2=qA2M�A2M�A2A�A2A�A2A�A29XA2=qA29XA25?A25?A25?A25?A2(�A2-A2=qA2=qA2A�A2A�A2E�A2A�A2I�A29XA21'A29XA21'A2 �A2(�A2-A2(�A2(�A2(�A2-A29XA2=qA29XA25?A21'A21'A25?A25?A21'A25?A29XA2=qA25?A2=qA2=qA21'A25?A2A�A2VA2ffA2jA2bNA2ffA2jA2ZA2M�A2ZA2Q�A2bNA2v�A2r�A2jA2ffA2ffA2^5A2^5A2bNA2^5A2^5A2bNA2bNA2bNA2jA2ffA2ffA2jA2ffA2ZA2ZA2I�A2VA2Q�A2VA2^5A2ffA2ffA2^5A2^5A2ZA2ZA2VA2Q�A2ZA2M�A2^5A2ZA2VA2bNA2jA2bNA2bNA2ffA2ffA2n�A2n�A2n�A2n�A2r�A2bNA2ZA2Q�A2VA2^5A2n�A2jA2jA2jA2n�A2n�A2v�A2z�A2r�A2r�A2n�A2bNA2^5A2bNA2VA2ZA2M�A2VA2VA2I�A2M�A2E�A2E�A2M�A2M�A2I�A2Q�A2I�A2I�A2M�A2Q�A2VA2M�A2M�A2M�A2Q�A2A�A2=qA2E�A2I�A29XA2=qA21'A21'A2-A21'A2-A2(�A2-A2-A2(�A21'A2E�A2VA2A�A2-A2(�A2-A2-A29XA2A�A2I�A2=qA2Q�A2ZA2ZA2ZA2ZA2VA2ZA2^5A2^5A2^5A2ffA2n�A2n�A2n�A2bNA2n�A2jA2bNA2ffA2ffA2^5A2bNA2ZA2M�A2ZA2n�A2n�A2jA2jA2Q�A2I�A2ZA2Q�A2I�A2VA2bNA2^5A2bNA2ffA2ffA2z�A2��A2��A2��A2��A2�jA2��A2ĜA2��A2ȴA2ĜA2ĜA2ȴA2��A2�RA2�DA2�DA2��A2��A2��A2�uA2��A2ĜA2��A2ĜA2ĜA2ȴA2ȴA2�jA2ȴA2ĜA2�9A2�9A2�jA2��A2��A2�A3/A3/A3�wA41A3�TA4JA3�hA3�A3�^A3\)A3l�A3��A4�A4��A5K�A5?}A5oA5/A5C�A5��A5��A6{A5�
A5�PA5�hA5+A5�7A5�PA5�
A5��A5t�A5�hA5��A5O�A57LA5�A5A4ȴA4�jA5`BA5\)A5oA4^5A4��A4 �A4$�A41A4VA4(�A45?A4VA45?A4��A4��A4�RA4��A533A5l�A5%A4v�A3��A4{A45?A4^5A4��A5VA5��A6bNA6z�A6��A6�jA6��A6�\A7C�A7;dA6�`A7"�A8^5A9%A8��A8�jA8�A8�/A8��A8�!A8�+A8ȴA8��A9C�A9O�A9�hA9�7A9C�A9t�A9hsA9�A9�FA9A;\)A=&�A=�hA=��A=hsA>  A>E�A> �A=�^A=��A=S�A=+A=S�A=;dA=O�A=K�A=K�A=XA=\)A=dZA=|�A=�FA?oA?K�A?C�A?K�A?+A?7LA?K�A?`BA@�AAVAA�AA+A@�`AA?}AA7LAA7LAA%AA+AAG�AAO�AAS�AAO�AAO�AAG�AA;dAA33AA33AA/AA7LAAG�AAG�AAK�AAK�AAS�AA�AA��AA�wAA��AA�TAA�AA��AB�AB�ABv�AB�uAB�9AB��AB��AC?}AC��AC��AC�-AC��AD �AD9XAD�AD�AD�`AD��AD��AD�AE%AE?}AEt�AEt�AEx�AE|�AE�7AE��AE��AE�AE`BAEK�AE�AEoAEoG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	N�B	N�B	M�B	N�B	N�B	O�B	O�B	O�B	O�B	O�B	O�B	N�B	O�B	O�B	O�B	N�B	M�B	M�B	M�B	N�B	O�B	O�B	O�B	M�B	O�B	Q�B	R�B	Q�B	Q�B	Q�B	P�B	Q�B	S�B	S�B	S�B	S�B	S�B	T�B	Q�B	P�B	T�B	T�B	\)B	]/B	x�B	��B	�7B	��B	�B
33B
e`B
�PB
��B
�BBBJB&�B@�BR�BYBcTBiyBl�Bv�Bz�B�B�DB�{B�\B�JB�DB�DB�B~�B�B�%B�+B�+B�+B�B�B�B�B~�B|�Bv�Bn�BgmBbNB[#BR�BI�BA�B?}B=qB;dB:^B8RB2-B,B)�B!�B�BPB1BBB
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�sB
�/B
��B
��B
��B
��B
��B
ɺB
ǮB
ƨB
ĜB
B
�}B
�qB
�dB
�9B
�-B
�B
��B
��B
��B
��B
��B
��B
�{B
�oB
�hB
�DB
�\B
��B
��B
�FB
�!B
�JB
{�B
o�B
jB
|�B
�B
�XB
ŢB
ƨB
ȴB
��B
�B
�B
�B
��B
��B
ȴB
��B
��B
�}B
�qB
�dB
�XB
�LB
�FB
�9B
�B
�B
�B
�B
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
��B
��B
��B
��B
�{B
�\B
�\B
�DB
�=B
�=B
�7B
�+B
�%B
�B
�B
�B
� B
y�B
v�B
t�B
q�B
p�B
o�B
n�B
n�B
l�B
k�B
hsB
dZB
aHB
\)B
W
B
VB
Q�B
K�B
H�B
H�B
F�B
F�B
A�B
>wB
<jB
;dB
8RB
5?B
2-B
,B
%�B
"�B
�B
�B
�B
�B
�B
oB
VB
JB
	7B
B
  B	��B	��B	��B	�B	�`B	�ZB	�NB	�HB	�BB	�;B	�
B	��B	B	�wB	�XB	�'B	��B	��B	��B	�DB	�%B	�B	~�B	}�B	z�B	y�B	u�B	t�B	s�B	r�B	p�B	m�B	hsB	aHB	\)B	XB	Q�B	J�B	F�B	F�B	F�B	E�B	D�B	B�B	@�B	<jB	;dB	5?B	33B	/B	+B	#�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	uB	hB	\B	VB		7B	+B	1B	%B	B��B��B��B��B��B��B��B��B�B�NB�/B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�#B�#B�#B�#B�#B�)B�/B�/B�;B�HB�NB�`B�sB�B�B�B��B��B	  B	B	B	+B	+B	
=B	JB	\B	hB	oB	�B	�B	�B	�B	$�B	%�B	(�B	,B	-B	/B	2-B	33B	7LB	9XB	9XB	9XB	8RB	8RB	8RB	7LB	7LB	7LB	7LB	8RB	8RB	8RB	8RB	:^B	;dB	=qB	@�B	B�B	C�B	C�B	E�B	F�B	G�B	I�B	N�B	O�B	P�B	P�B	P�B	R�B	T�B	T�B	R�B	T�B	XB	ZB	]/B	^5B	^5B	_;B	_;B	cTB	e`B	e`B	gmB	e`B	gmB	ffB	ffB	ffB	ffB	ffB	ffB	hsB	hsB	iyB	iyB	m�B	p�B	t�B	w�B	y�B	|�B	� B	� B	�B	�B	�%B	�%B	�1B	�=B	�=B	�=B	�PB	�PB	�VB	�\B	�bB	�hB	�hB	�oB	�uB	��B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	�B	�'B	�FB	�jB	ÖB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�TB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�yB	�TB	�NB	�HB	�BB	�BB	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�B	�/B	�/B	�5B	�NB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B
	7B
JB
\B
\B
bB
hB
{B
�B
�B
�B
�B
!�B
!�B
"�B
#�B
(�B
.B
/B
0!B
1'B
2-B
6FB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
;dB
;dB
<jB
<jB
?}B
A�B
B�B
B�B
B�B
C�B
D�B
E�B
F�B
H�B
H�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
P�B
P�B
P�B
P�B
P�B
Q�B
S�B
VB
W
B
XB
XB
XB
ZB
[#B
]/B
^5B
`BB
bNB
dZB
e`B
e`B
ffB
ffB
gmB
iyB
iyB
jB
m�B
o�B
p�B
q�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
y�B
z�B
{�B
|�B
|�B
|�B
}�B
~�B
� B
�B
�B
�B
�B
�B
�%B
�%B
�1B
�7B
�=B
�DB
�JB
�\B
�hB
�oB
�oB
�oB
�oB
�{B
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
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�!B
�!B
�'B
�'B
�-B
�-B
�-B
�3B
�3B
�?B
�?B
�FB
�FB
�FB
�FB
�LB
�LB
�RB
�RB
�XB
�^B
�dB
�dB
�jB
�jB
�jB
�qB
�wB
�}B
��B
B
ÖB
ĜB
ĜB
ŢB
ƨB
ǮB
ȴB
ȴB
ɺB
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
��B
��B
��B
��B
��B
��B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�#B
�)B
�/B
�/B
�5B
�5B
�;B
�BB
�BB
�BB
�BB
�HB
�NB
�NB
�NB
�NB
�NB
�TB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�fB
�fB
�fB
�mB
�sB
�sB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��BBBBBBBBBBBB%B+B1B	7B	7B	7B	7B	7B
=B
=B
=BDBDBJBJBPBPBVBVB\B\B\B\B\BbBhBhBoBuBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	O�B	O�B	O�B	M�B	O�B	N�B	N�B	N�B	N�B	M�B	O�B	O�B	N�B	M�B	O�B	O�B	N�B	M�B	N�B	M�B	M�B	N�B	N�B	O�B	M�B	N�B	O�B	N�B	N�B	N�B	M�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	P�B	O�B	O�B	N�B	P�B	O�B	N�B	O�B	P�B	O�B	P�B	O�B	N�B	O�B	O�B	O�B	N�B	O�B	N�B	L�B	P�B	P�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	P�B	O�B	O�B	O�B	O�B	O�B	O�B	O�B	N�B	N�B	P�B	O�B	O�B	O�B	O�B	N�B	N�B	O�B	O�B	O�B	P�B	O�B	N�B	N�B	O�B	O�B	O�B	P�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	O�B	N�B	P�B	O�B	N�B	N�B	O�B	N�B	N�B	N�B	O�B	N�B	P�B	O�B	N�B	O�B	O�B	N�B	M�B	N�B	O�B	O�B	O�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	M�B	P�B	P�B	O�B	O�B	N�B	O�B	O�B	M�B	O�B	P�B	O�B	O�B	O�B	O�B	N�B	P�B	O�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	O�B	P�B	O�B	P�B	P�B	P�B	O�B	O�B	O�B	O�B	N�B	N�B	N�B	N�B	M�B	Q�B	N�B	N�B	N�B	O�B	N�B	N�B	O�B	O�B	M�B	N�B	P�B	N�B	P�B	O�B	N�B	O�B	P�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	O�B	N�B	O�B	O�B	N�B	N�B	N�B	O�B	N�B	O�B	N�B	N�B	M�B	O�B	Q�B	O�B	M�B	L�B	M�B	N�B	L�B	M�B	N�B	L�B	N�B	M�B	K�B	J�B	M�B	N�B	M�B	M�B	M�B	M�B	M�B	M�B	N�B	O�B	L�B	N�B	N�B	N�B	N�B	M�B	L�B	M�B	L�B	N�B	M�B	K�B	O�B	N�B	L�B	M�B	L�B	N�B	M�B	L�B	N�B	P�B	M�B	O�B	N�B	M�B	O�B	O�B	N�B	N�B	N�B	O�B	M�B	O�B	P�B	O�B	N�B	N�B	N�B	O�B	Q�B	P�B	O�B	Q�B	Q�B	L�B	S�B	N�B	N�B	O�B	P�B	M�B	N�B	P�B	M�B	N�B	P�B	N�B	P�B	P�B	M�B	O�B	N�B	O�B	N�B	M�B	P�B	Q�B	O�B	M�B	N�B	M�B	M�B	N�B	N�B	N�B	N�B	N�B	L�B	H�B	R�B	Q�B	S�B	N�B	O�B	R�B	N�B	M�B	N�B	O�B	N�B	N�B	N�B	P�B	M�B	J�B	S�B	R�B	Q�B	S�B	R�B	Q�B	T�B	R�B	Q�B	R�B	P�B	R�B	S�B	R�B	P�B	R�B	R�B	R�B	R�B	R�B	R�B	Q�B	R�B	Q�B	Q�B	P�B	N�B	R�B	T�B	Q�B	S�B	R�B	P�B	Q�B	Q�B	Q�B	P�B	P�B	S�B	Q�B	M�B	R�B	P�B	Q�B	Q�B	Q�B	P�B	VB	Q�B	M�B	VB	O�B	M�B	P�B	R�B	O�B	P�B	M�B	O�B	Q�B	R�B	R�B	Q�B	O�B	R�B	Q�B	Q�B	P�B	O�B	O�B	T�B	P�B	Q�B	Q�B	P�B	N�B	O�B	R�B	T�B	T�B	VB	S�B	W
B	VB	P�B	VB	J�B	P�B	VB	W
B	W
B	S�B	T�B	T�B	S�B	T�B	T�B	S�B	S�B	S�B	S�B	S�B	VB	S�B	T�B	W
B	VB	XB	N�B	S�B	Q�B	R�B	P�B	W
B	S�B	S�B	S�B	R�B	T�B	T�B	M�B	W
B	O�B	S�B	S�B	Q�B	S�B	T�B	VB	T�B	Q�B	T�B	VB	T�B	T�B	S�B	YB	Q�B	W
B	Q�B	S�B	M�B	VB	T�B	T�B	T�B	T�B	S�B	VB	XB	W
B	S�B	XB	N�B	R�B	W
B	R�B	T�B	P�B	S�B	ZB	Q�B	T�B	Q�B	Q�B	Q�B	R�B	O�B	R�B	R�B	Q�B	Q�B	Q�B	VB	Q�B	R�B	T�B	W
B	M�B	M�B	R�B	S�B	N�B	R�B	P�B	P�B	O�B	P�B	P�B	O�B	P�B	Q�B	N�B	J�B	S�B	R�B	O�B	O�B	P�B	O�B	M�B	P�B	O�B	VB	N�B	R�B	S�B	S�B	R�B	S�B	R�B	R�B	S�B	VB	P�B	T�B	T�B	S�B	XB	Q�B	W
B	W
B	S�B	T�B	XB	R�B	VB	VB	P�B	R�B	T�B	T�B	T�B	\)B	T�B	Q�B	VB	W
B	Q�B	S�B	T�B	W
B	T�B	W
B	R�B	XB	XB	]/B	]/B	]/B	]/B	\)B	[#B	[#B	_;B	]/B	\)B	`BB	_;B	bNB	VB	VB	aHB	\)B	^5B	VB	ZB	_;B	^5B	]/B	`BB	_;B	bNB	XB	`BB	`BB	_;B	YB	gmB	[#B	YB	\)B	XB	W
B	jB	s�B	P�B	��B	�%B	D�B	�JB	~�B	iyB	<jB	�PB	r�B	��B	��B	��B	�1B	�7B	�oB	��B	��B	�-B	��B	��B	k�B	�{B	�uB	��B	��B	�VB	��B	�!B	��B	��B	�B	�=B	�oB	x�B	��B	��B	�B	{�B	{�B	p�B	�PB	�+B	}�B	z�B	�B	��B	p�B	�oB	�+B	r�B	x�B	��B	�B	�!B	k�B	q�B	�bB	�=B	|�B	�B	J�B	�hB	��B	�-B	�B	��B	��B	�3B	��B	��B	�XB	y�B	�}B	��B	��B	�B	�B	�B	�HB	�B	�B	�ZB	�ZB	�B	�B	��B	�B	�B	��B	��B	��B
B	��B	�BB
�B
?}B
K�B
�B
B�B
YB
L�B
F�B
iyB
>wB
=qB
N�B
.B
<jB
=qB
8RB
C�B
7LB
L�B
D�B
�B
W
B
iyB
hsB
n�B
l�B
o�B
VB
P�B
iyB
�{B
�\B
��B
r�B
�DB
�JB
��B
u�B
�+B
�PB
�PB
�\B
�bB
�bB
�PB
�PB
�VB
�PB
�PB
�DB
�\B
�hB
�bB
�\B
�1B
��B
��B
��B
�hB
��B
��B
��B
��B
�VB
�B
�B
�!B
�-B
��B
��B
ŢB
ĜB
ÖB
ɺB
��B
��B
�#B
�B
�yB
�B
�B
�TB
�#B
�B
�B
�B
�B
�B
��B
��BB%B%B
=B%BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             B	N�B	N�B	M�B	N�B	N�B	O�B	O�B	O�B	O�B	O�B	O�B	N�B	O�B	O�B	O�B	OB	M�B	M�B	M�B	N�B	O�B	O�B	O�B	M�B	OgB	Q�B	R�B	RB	Q�B	Q�B	P�B	Q�B	S�B	TB	S�B	S�B	TB	U;B	RB	P�B	T�B	T'B	[�B	Y`B	u�B	�*B	�B	�B	�B
,�B
`�B
��B
��B
�B�BgB*�BDNBT�B[�Bd�Bj.Bm�Bv�BznB�FB�%B��B��B��B��B��B�MBB�rB�<B�PB��B��B��B��B�~B��B�B~�ByhBp5BiBd<B\�BW�BKBBB@B=�B;�B:�B: B3<B,�B,�B$)B�B<B�B�B�B
��B
�lB
�_B
��B
��B
�B
�B
�B
�B
��B
��B
�B
�}B
�B
�B
�eB
�LB
�#B
�yB
��B
�CB
�B
�\B
�'B
�wB
�;B
��B
�CB
��B
�B
��B
��B
�<B
�vB
��B
��B
��B
�B
��B
��B
��B
��B
�JB
�AB
��B
�TB
�XB
~�B
p�B
f�B
uIB
� B
�"B
�UB
�B
��B
��B
�oB
��B
٩B
�7B
�=B
��B
«B
�TB
�XB
�B
�JB
��B
��B
��B
��B
��B
��B
��B
�uB
�%B
�]B
�B
�B
��B
��B
�B
��B
�;B
�B
��B
��B
�fB
��B
��B
�7B
�zB
��B
��B
��B
��B
��B
��B
�6B
��B
��B
��B
�bB
{1B
x.B
usB
rB
q3B
pB
n�B
o B
m�B
mB
j&B
fqB
cxB
^B
XwB
X�B
U�B
L�B
JMB
I�B
H;B
I�B
B�B
?B
=B
<tB
9�B
7�B
5B
.�B
'~B
#�B
! B
�B
�B
$B
nB
B
�B
B
�B
lB
 ~B	��B	��B	�RB	�@B	�B	��B	�B	�B	�lB	�&B	��B	��B	�B	�;B	��B	��B	�_B	��B	��B	�lB	��B	�`B	�B	B	|>B	{B	vCB	u:B	t6B	s�B	r6B	psB	k�B	c�B	^�B	[�B	V�B	KLB	F�B	F�B	GB	FB	E/B	DB	B�B	>B	=B	7�B	5�B	1�B	.B	%<B	!�B	�B	�B	�B	wB	VB	�B	�B		B	�B	6B	]B	mB	|B	�B	�B	�B	
B	{B	�B	�B	�B��B��B�nB��B�YB��B�YB��B�B��B�LB�ZB�\BٗB�tB�bB�KB�@B�@B�HB٤B��B�B��BۈB�MB�&B�B�B�B��BۼB�sB�[B�.B�EB�/B�=B�WB�B�hB�,B�yB�B�NB�.B�'B��B	�B	B	8B	�B		�B	�B	fB	�B	7B	3B	B	�B	>B	$�B	%�B	(�B	+�B	,�B	.�B	2�B	3�B	7�B	:}B	9�B	9�B	8�B	9|B	9B	7uB	7QB	7�B	7�B	8�B	8�B	9B	8lB	:hB	;�B	=�B	@�B	B�B	C�B	C�B	F@B	F�B	G�B	I�B	N�B	O�B	P�B	P�B	QkB	SeB	U]B	U`B	S2B	T�B	W�B	ZB	]B	^?B	^BB	_NB	_�B	d9B	e�B	f2B	g�B	e�B	g�B	f�B	g�B	g;B	f�B	f�B	g:B	iB	h�B	i�B	i�B	m9B	p5B	t^B	w�B	zB	}~B	�sB	�*B	�FB	�.B	�4B	�VB	��B	��B	�{B	�HB	��B	�xB	�bB	�jB	��B	��B	��B	��B	��B	�TB	��B	��B	�B	��B	��B	�2B	�}B	�YB	��B	��B	�HB	�<B	�HB	��B	��B	��B	�BB	�.B	��B	�VB	јB	ӴB	�2B	�B	�;B	�yB	�$B	�B	�B	�&B	�#B	�2B	�BB	�CB	��B	�tB	�wB	�KB	�DB	��B	�sB	�B	�B	�|B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	��B	�XB	��B	�B	��B	�?B	�XB	��B	��B	�yB	��B	�B	�EB	��B	�@B	��B	��B	�B	�~B	�B	�nB	�B	�B	�B	�B	��B	��B	�JB	� B	�[B	��B	��B	�.B	��B	�*B	��B	�*B	�B	�B	�^B	�B	��B	��B	�*B	�qB	�JB	�B	��B	��B	�B	�.B	�GB	��B	�KB	�,B	��B	�B	�B	�PB	�B	��B	��B	�?B	��B	�B	ݱB	�0B	ޒB	�B	�bB	�tB	�nB	�kB	�B	�B	�B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�RB
�B
�B
!B
XB
tB
TB
B
cB
�B
pB
�B
�B
!�B
!�B
"�B
$B
))B
./B
/BB
0HB
1^B
2�B
6gB
7KB
7]B
7�B
8B
7�B
7lB
8zB
;kB
;vB
<�B
<�B
?�B
A�B
B�B
B�B
B�B
C�B
D�B
E�B
GB
H�B
H�B
J�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N	B
OB
OeB
Q
B
P�B
P�B
P�B
QB
R0B
TB
VB
WB
XB
XB
XB
Z)B
[<B
];B
^OB
`hB
bgB
d~B
e`B
e|B
f�B
f�B
goB
i�B
izB
jmB
m�B
o�B
p�B
q�B
r�B
s�B
s�B
s�B
t�B
u�B
v�B
w�B
y�B
z�B
{�B
|�B
|�B
|�B
~B
~�B
�B
�!B
�3B
�B
�BB
�-B
�.B
�1B
�-B
�4B
�=B
�aB
��B
�uB
�\B
�mB
�qB
�qB
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
��B
��B
��B
� B
�B
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
�B
��B
�B
�B
�B
�KB
�<B
�)B
�0B
�1B
�1B
�>B
�6B
�7B
�-B
�2B
�=B
�8B
�:B
�PB
�HB
�GB
�TB
�WB
�dB
�vB
�^B
�mB
�gB
�yB
�bB
�fB
�tB
�kB
�wB
�~B
�vB
�eB
��B
B
ËB
ĶB
��B
ŽB
ƨB
ǬB
ȲB
ȸB
ɺB
ʵB
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
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
�B
�(B
�0B
�7B
�GB
�UB
�JB
�BB
�pB
�<B
�GB
�LB
�\B
�\B
�fB
�kB
�cB
�qB
�hB
�~B
�\B
�_B
�|B
�dB
�XB
�PB
�`B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
��B
��B
��B
��B
��B
��B
��B
�%B,BBBBBBBBBB9B?B7B0B	AB	9B	8B	8B	;B
TB
MB
NBKBHBTBlBvBkBbBYB^BhB\BiB`B|B�B�B�B~B�BxB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B	O�B	O�B	O�B	M�B	O�B	N�B	N�B	N�B	N�B	M�B	O�B	O�B	N�B	M�B	O�B	O�B	N�B	M�B	N�B	M�B	M�B	N�B	N�B	O�B	M�B	N�B	O�B	N�B	N�B	N�B	M�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	P�B	O�B	O�B	N�B	P�B	O�B	N�B	O�B	P�B	O�B	P�B	O�B	N�B	O�B	O�B	O�B	N�B	O�B	N�B	L�B	P�B	P�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	P�B	O�B	O�B	O�B	O�B	O�B	O�B	O�B	N�B	N�B	P�B	O�B	O�B	O�B	O�B	N�B	N�B	O�B	O�B	O�B	P�B	O�B	N�B	N�B	O�B	O�B	O�B	P�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	O�B	N�B	P�B	O�B	N�B	N�B	O�B	N�B	N�B	N�B	O�B	N�B	P�B	O�B	N�B	O�B	O�B	N�B	M�B	N�B	O�B	O�B	O�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	N�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	M�B	P�B	P�B	O�B	O�B	N�B	O�B	O�B	M�B	O�B	P�B	O�B	O�B	O�B	O�B	N�B	P�B	O�B	N�B	O�B	O�B	O�B	N�B	O�B	O�B	N�B	N�B	O�B	P�B	O�B	P�B	P�B	P�B	O�B	O�B	O�B	O�B	N�B	N�B	N�B	N�B	M�B	Q�B	N�B	N�B	N�B	O�B	N�B	N�B	O�B	O�B	M�B	N�B	P�B	N�B	P�B	O�B	N�B	O�B	P�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	O�B	O�B	M�B	N�B	O�B	O�B	N�B	O�B	O�B	N�B	O�B	N�B	O�B	O�B	N�B	N�B	N�B	O�B	N�B	O�B	N�B	N�B	M�B	O�B	Q�B	O�B	M�B	L�B	M�B	N�B	L�B	M�B	N�B	L�B	N�B	M�B	K�B	J�B	M�B	N�B	M�B	M�B	M�B	M�B	M�B	M�B	N�B	O�B	L�B	N�B	N�B	N�B	N�B	M�B	L�B	M�B	L�B	N�B	M�B	K�B	O�B	N�B	L�B	M�B	L�B	N�B	M�B	L�B	N�B	P�B	M�B	O�B	N�B	M�B	O�B	O�B	N�B	N�B	N�B	O�B	M�B	O�B	P�B	O�B	N�B	N�B	N�B	O�B	Q�B	P�B	O�B	Q�B	Q�B	L�B	S�B	N�B	N�B	O�B	P�B	M�B	N�B	P�B	M�B	N�B	P�B	N�B	P�B	P�B	M�B	O�B	N�B	O�B	N�B	M�B	P�B	Q�B	O�B	M�B	N�B	M�B	M�B	N�B	N�B	N�B	N�B	N�B	L�B	H�B	R�B	Q�B	S�B	N�B	O�B	R�B	N�B	M�B	N�B	O�B	N�B	N�B	N�B	P�B	M�B	J�B	S�B	R�B	Q�B	S�B	R�B	Q�B	T�B	R�B	Q�B	R�B	P�B	R�B	S�B	R�B	P�B	R�B	R�B	R�B	R�B	R�B	R�B	Q�B	R�B	Q�B	Q�B	P�B	N�B	R�B	T�B	Q�B	S�B	R�B	P�B	Q�B	Q�B	Q�B	P�B	P�B	S�B	Q�B	M�B	R�B	P�B	Q�B	Q�B	Q�B	P�B	VB	Q�B	M�B	VB	O�B	M�B	P�B	R�B	O�B	P�B	M�B	O�B	Q�B	R�B	R�B	Q�B	O�B	R�B	Q�B	Q�B	P�B	O�B	O�B	T�B	P�B	Q�B	Q�B	P�B	N�B	O�B	R�B	T�B	T�B	VB	S�B	W
B	VB	P�B	VB	J�B	P�B	VB	W
B	W
B	S�B	T�B	T�B	S�B	T�B	T�B	S�B	S�B	S�B	S�B	S�B	VB	S�B	T�B	W
B	VB	XB	N�B	S�B	Q�B	R�B	P�B	W
B	S�B	S�B	S�B	R�B	T�B	T�B	M�B	W
B	O�B	S�B	S�B	Q�B	S�B	T�B	VB	T�B	Q�B	T�B	VB	T�B	T�B	S�B	YB	Q�B	W
B	Q�B	S�B	M�B	VB	T�B	T�B	T�B	T�B	S�B	VB	XB	W
B	S�B	XB	N�B	R�B	W
B	R�B	T�B	P�B	S�B	ZB	Q�B	T�B	Q�B	Q�B	Q�B	R�B	O�B	R�B	R�B	Q�B	Q�B	Q�B	VB	Q�B	R�B	T�B	W
B	M�B	M�B	R�B	S�B	N�B	R�B	P�B	P�B	O�B	P�B	P�B	O�B	P�B	Q�B	N�B	J�B	S�B	R�B	O�B	O�B	P�B	O�B	M�B	P�B	O�B	VB	N�B	R�B	S�B	S�B	R�B	S�B	R�B	R�B	S�B	VB	P�B	T�B	T�B	S�B	XB	Q�B	W
B	W
B	S�B	T�B	XB	R�B	VB	VB	P�B	R�B	T�B	T�B	T�B	\)B	T�B	Q�B	VB	W
B	Q�B	S�B	T�B	W
B	T�B	W
B	R�B	XB	XB	]/B	]/B	]/B	]/B	\)B	[#B	[#B	_;B	]/B	\)B	`BB	_;B	bNB	VB	VB	aHB	\)B	^5B	VB	ZB	_;B	^5B	]/B	`BB	_;B	bNB	XB	`BB	`BB	_;B	YB	gmB	[#B	YB	\)B	XB	W
B	jB	s�B	P�B	��B	�%B	D�B	�JB	~�B	iyB	<jB	�PB	r�B	��B	��B	��B	�1B	�7B	�oB	��B	��B	�-B	��B	��B	k�B	�{B	�uB	��B	��B	�VB	��B	�!B	��B	��B	�B	�=B	�oB	x�B	��B	��B	�B	{�B	{�B	p�B	�PB	�+B	}�B	z�B	�B	��B	p�B	�oB	�+B	r�B	x�B	��B	�B	�!B	k�B	q�B	�bB	�=B	|�B	�B	J�B	�hB	��B	�-B	�B	��B	��B	�3B	��B	��B	�XB	y�B	�}B	��B	��B	�B	�B	�B	�HB	�B	�B	�ZB	�ZB	�B	�B	��B	�B	�B	��B	��B	��B
B	��B	�BB
�B
?}B
K�B
�B
B�B
YB
L�B
F�B
iyB
>wB
=qB
N�B
.B
<jB
=qB
8RB
C�B
7LB
L�B
D�B
�B
W
B
iyB
hsB
n�B
l�B
o�B
VB
P�B
iyB
�{B
�\B
��B
r�B
�DB
�JB
��B
u�B
�+B
�PB
�PB
�\B
�bB
�bB
�PB
�PB
�VB
�PB
�PB
�DB
�\B
�hB
�bB
�\B
�1B
��B
��B
��B
�hB
��B
��B
��B
��B
�VB
�B
�B
�!B
�-B
��B
��B
ŢB
ĜB
ÖB
ɺB
��B
��B
�#B
�B
�yB
�B
�B
�TB
�#B
�B
�B
�B
�B
�B
��B
��BB%B%B
=B%BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                             <T��<T��<T�=<T��<T�N<U�<U�<T��<T��<T�K<T�<T��<T�=<T�=<T��<UK<T��<U�<U �<U 5<T��<T�=<T��<T�j<U�<T��<T�0<T�l<T�j<T�<U<U
v<T��<T�2<T��<U <T��<U�<T��<U�<T�<Uj\<U!u<]��<YϏ<f� <_0<�9�<��H<{�K<pQA<k�n<y6�<f2�<f��<j��<m��<mD�<gv<i��<f}8<e��<fd�<ea�<e}<e��<e΂<jk�<fuk<ei�<fX�<k�2<fH�<e`�<etX<eai<ec><eu0<e��<e�:<e��<fl4<e��<eɫ<f�8<i-�<f��<f�<gq�<f��<p��<fg�<e��<e�2<e��<epa<e�H<f��<f !<e˗<i�c<h{c<o��<eي<e��<f� <f��<e�<f��<e�{<f,<es�<e`V<e`z<eb}<e`D<ee<eb}<ee<e�,<q	Z<g=�<e�*<ewa<el1<e�K<e�n<e�(<e|<e��<e�\<e�x<e��<g�1<gRI<f~�<gx?<gkq<f�8<ey5<e��<gN'<gó<e��<e�X<f�<h�Q<e�	<fj�<fU�<i%<o52<y�+<n_�<ju�<fL<m��<���<gq�<f1^<e�<e�C<e�<f�<eq#<e��<f��<fF<k�9<g��<fL<e�f<eȶ<e��<e�u<e�*<ev�<f��<gX<g;�<e�%<e��<f��<f 9<ez&<ea�<eaP<e`o<e��<f�<e�B<fj�<h�<e˗<f�<f��<hp�<g�%<gk<g�(<f2�<etX<ejV<e��<e��<e��<f~<e�W<e~<gX�<h��<f^�<fuk<e�6<ev�<e��<e�<em�<e��<e��<f��<f�L<g��<h�<gs�<f��<j_<l˻<f !<f��<e��<f�8<j��<f4<e�<e�H<fO<f�<hE�<j�<i�<f�T<f<fp�<f�<fTa<f��<go�<g��<fM*<i��<i  <fd�<e��<e��<g�R<p�r<\��<UK�<UZ<U�<U�<U�<Z�<^/Z<^��<Vk <Y�><e��<`3,<VZ�<Y��<[mH<WĞ<V��<V�<Ui[<U�l<V#<U�X<U$g<U#6<U$g<U�!<Vx<Y��<[J�<X��<Y3�<\S�<b��<U+J<U<UP<U�<U�<U0�<VU:<W֣<V��<V�V<X�<Yr<Y��<Zc�<V(S<U��<U��<U�Q<W�V<W.�<Ubs<T��<T��<U"	<U�<U2�<V��<Y�<U�<U�<V<X��<Udf<U�<UW%<Vc�<Y=g<U6S<U��<U(�<U}@<VSq<U��<X�5<^��<ZGc<Y
<U�L<U}<U	!<U$g<U�<U(<U�<U�<U �<U�<U,�<U=�<U��<V<V2i<U�<T�%<T�=<T��<U��<U\�<U4�<U�<UP<T�<T�<T��<T��<T�{<U_<T�<U	�<T�K<U|)<U�<U:�<UO<U�<U�<T�K<T�Z<U�<U�<U�<T�0<U �<UP<U<<U-E<UN=<U$g<U�<U�<U�<U<<U0<U�<U-�<U�<U!u<U�<U<J<U�<U*�<U�<UW%<U�<T�<U	t<UGs<U	t<UA<UTl<T��<T�0<U�<U�<U�<Ul<T�j<T��<U8�<U#<T�l<T��<T�l<T��<T��<T��<U(<U�<U#<U~<U�<U�<U�<T�Z<T��<T�0<T�Z<T��<U@+<Ux�<U�<Uea<U)[<U#�<U	<U(�<U��<UhZ<U�<UX<Ug\<U8�<UZ<U�<U�<U!<U�<U�<T�<U <U.�<U�<U<U�<T��<T�{<U�<UA<UA<U�<U�<U"	<U�<T�K<T�j<U�<U�<UV<T��<V\i<Uf^<U�<UP<U<J<U�<U�<U�<U �<U�<U��<U��<UTl<Uq<U�2<V��<UA�<T��<U)[<Ud<UX<U"�<UI<UV;<U�<T��<U<U<T��<T�Z<T��<T�<T�K<T��<U+<U [<U6S<UE<Ug<T��<T��<U�<UK<Ud<U�<T�<U�<UX<UK<U�<U�<U}<U�<UX<U<UM<U<T�<T�<U�<U�<UD�<U<U�<U�<U)�<U7<U��<WF<V�w<Unm<U�<U-�<Ug<U2�<U�<VV<T�<T��<T��<U]<T�<T�l<T��<T�j<U|)<T�N<UT<UP�<U �<U�<T��<U�<Ut�<_�X<Z��<\l5<U��<U<<T��<U�<VZ�<V�'<W�1<V+�<V <UP<T��<T�<T��<U.<Ud<U��<U#<U�<T��<T��<UK<Ul<T�l<T��<U�[<U#<UD�<T�N<U%�<U��<UA<U�<T��<T�j<T��<T��<T��<U�<U�<U	�<T�Z<T��<U <Ug<U<U�<T�<T�K<T��<T��<T��<U <U�<T��<U@+<U P<U%<U�<T��<T�N<T�j<U�<T�N<T�<T��<T��<T�0<U �<T��<T�Z<Ug<U<T��<U�<U�<U<U)�<U �<T��<T��<U
<UW%<UV<U [<U�<T�<T��<T��<U�<T��<U�<T�l<T�j<U 5<UX<T��<T��<U�<U �<U 5<U�<T�<T��<T��<U <UX<T�{<U�<U�<U<U+�<U+<T��<T�K<T��<T��<U�<UX<T�0<T��<T��<T��<T�{<T�K<T�l<T�K<T��<UX<T�l<U �<T��<T��<UX<T��<T��<T�%<T��<T��<T�l<T��<T�K<T�j<T��<T��<T��<T��<T�=<T�Z<T��<T��<T��<T��<e`�<T��<e`�<e`�<T�N<e`D<e`V<ea�<U�<T��<ec�<e`�<T�%<T�K<T��<T��<e`B<T��<eh<ea�<e`�<e`D<e`D<T��<ef�<ed<ea!<e`B<e`P<e`�<efa<e`D<e`�<ea�<T�l<ea!<ea�<e`K<ea�<e`K<e`V<eb8<er�<ec�<e`P<e`D<ee<ech<eb<efa<ef�<e`z<e`D<e`B<e`D<e`B<e`D<e`D<e`G<e`D<e`B<e`B<e`�<e`B<e`�<e`�<eb<ej<ec�<e`�<e`�<e`�<e`�<eai<e`�<e`z<e`B<e`P<e`z<e`P<e`P<e`�<e`D<e`B<e`�<e`�<ea�<ed<e`�<ea�<e`�<ea�<e`D<e`D<e`z<e`B<e`�<e`�<e`B<ea�<e`G<e`]<e`�<ea�<ef�<ea�<e`B<e`D<e`D<e`K<e`B<e`�<e`G<e`G<e`�<e`�<ea�<ea�<ec�<e`o<e`�<ea�<e`B<e`�<e`�<e`K<e`V<e`G<e`D<e`o<e`�<e`�<e`�<e`B<e`o<e`�<ec><ec�<ea�<e`]<e`B<e`D<e`�<ea�<e`f<e`B<ed�<e`V<e`B<e`D<e`�<e`�<ea�<eb<e`�<eai<e`�<ec<e`D<e`B<ea�<e`D<e`�<eaP<e`�<ea�<ef'<ea8<e`�<e`�<e`K<e`�<e`�<e`B<eai<ea�<e`�<e`G<e`B<e`B<e`�<e`B<e`D<e`D<e`D<ea�<e`V<e`K<e`z<e`D<e`�<ea�<e`G<e`�<e`�<e`�<e`B<e`�<e`�<e`D<e`B<e`�<e`G<e`D<e`G<e`P<e`G<e`G<edJ<ech<e`�<e`D<e`B<e`K<e`G<e`B<e`B<e`B<e`B<ea�<ea�<e`�<e`B<e`z<e`D<e`B<e`B<e`K<eai<e`�<e`�<e`]<e`K<e`z<eb�<ech<ea�<e`�<e`G<e`D<e`�<e`B<e`�<e`K<ea�<ea�<eb8<ea�<e`o<e`�<e`G<eaP<e`�<ea�<e`�<e`�<ec�<ea�<e`z<e`�<e`D<ea�<ea�<efa<ea�<e`D<ef�<ea�<e`G<e`�<e`K<e`B<e`P<e`B<e`G<e`G<e`�<e`B<e`�<e`D<e`B<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<T��<e`B<T��<T��<T��<e`B<T��<T��<e`B<T��<e`B<e`B<e`B<T��<e`B<e`B<e`B<e`B<e`B<e`B<e`B<T��<T��<e`B<e`B<e`B<T��<e`B<e`B<e`B<e`B<e`B<e`B<e`B<T��<e`B<e`B<e`B<T��<e`B<e`B<e`B<e`B<T��<T��<T��<e`B<T��<e`B<T��<T��<T��<e`B<e`B<e`B<T��<T��<e`B<e`B<T��<T��<T��<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0004), vertically averaged dS =0.008(+/-0.013),                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW: r =1(+/-0.0004), vertically averaged dS =0.008(+/-0.013),                                                                                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                No thermal mass adjustment on non-primary profiles.; No significant drift detected in conductivity                                                                                                                                                              202207120000002022071200000020220712000000202207120000002022071200000020220712000000AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030120160620210301201606QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030120160620210301201606QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               WHOIWHOIARSQARSQWHQCWHQCV0.5V0.5                                                                                                                                2021032900000020210329000000QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                    WHOI    ARSQ    WHQC    V0.5                                                                                                                                              20211025000000    CF                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARSQARSQCTM CTM V1.0V1.0                                                                                                                                2022071100000020220711000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARCAARCAOWC OWC V2.0V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     2022071200000020220712000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                