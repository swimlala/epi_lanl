CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       $Woods Hole Oceanographic Institution   source        
Argo float     history       92021-03-02T01:06:08Z creation; 2022-07-12T13:01:51Z DMQC;      
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
_FillValue                 �  \L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  d4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � a4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � i   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �lArgo profile    3.1 1.2 19500101000000  20210302010608  20220712090151  1902223 1902223 US ARGO PROJECT                                                 US ARGO PROJECT                                                 BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         BRECK OWENS, STEVEN JAYNE, P.E. ROBBINS                         PRES            TEMP            PSAL            PRES            TEMP            PSAL               .   .AA  AOAO7993                            7993                            2C  2C  DD  S2A                             S2A                             7529                            7529                            SBE602 15Aug17 ARM V2.4         SBE602 15Aug17 ARM V2.4         854 854 @���.@���.11  @���W�@���W��D�L�A��D�L�A�@D߲V��@D߲V��11  GPS     GPS     Primary sampling: averaged [nominal 2 dbar binned data sampled at 0.5 Hz from a SBE41CP]                                                                                                                                                                        Near-surface sampling: discrete, pumped [data sampled at 1.0Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�  @�\@@  @�  @�  @�G�@�G�A   A��A#�
A@  A`��A�Q�A�  A��A�  A�Q�A�Q�A�  A�  B   B(�B  B  B (�B(  B/�
B8  B?�
BG�
BP(�BW�
B_�
Bh  Bp  Bx  B�
B��B�{B�{B�  B��B�  B��B�  B�(�B�  B�  B�{B�  B�  B�  B�  B�{B�  B�  B�  B�{B�{B�  B��B��B�  B��B��B��B�  B�{B��C  C  C
=C
=C	��C��C
=C
=C
=C
=C  C��C��C  C  C   C"  C$  C&  C(  C*  C,
=C.  C/��C2  C4  C6  C8  C:  C<  C>  C@  CB  CD
=CF  CG��CJ  CL  CM��CO��CR  CT  CU��CW��CY��C[��C]��C`  Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr  Cs��Cv  Cx
=Cz  C{��C~
=C�C�C�  C�  C�
=C�  C�  C���C���C���C���C�  C�C�C�C�  C���C�  C�  C�  C�  C���C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�C�  C���C���C�C�  C���C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�C�C�C�C�C�C�C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�C���C���C�  C�C�C�  C�  C�  C���C���C���C�  C�  C�C�C���C���C���C�  C�  C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C�C�  C�  C�  C�  C�  C���D � D�D� D  D��D  D}qD�qD}qD  D� D�qD}qD  D}qD�qD��D	�D	��D
  D
}qD
�qD� D�D��D�D� D�qD� D  D}qD  D� D�qD� D�D��D�D� D�qD� D�D��D  Dz�D  D��D  D� D�qD� D�D��D  D� D  D��D  D� D  D}qD  D��D   D ��D!  D!}qD"  D"��D#  D#}qD$  D$� D%  D%� D%�qD&� D'  D'� D(�D(� D)  D)��D*  D*� D*�qD+}qD+�qD,� D,�qD-}qD.  D.� D/D/��D0  D0� D1  D1� D2  D2��D3  D3� D3�qD4}qD5  D5�D6D6� D7  D7� D8  D8� D9  D9� D9�qD:� D;  D;� D<�D<� D<�qD=� D>  D>� D?�D?}qD?�qD@� DA  DA� DB  DB� DC  DC� DD�DD� DD�qDE� DF�DF� DF��DG}qDG�qDH� DI  DI� DJ�DJ��DK�DK� DL  DL� DL�qDM� DM�qDN}qDO  DO� DP�DP��DQ�DQ��DR  DR� DS  DS��DT  DT}qDT�qDU}qDV  DV��DW�DW� DW�qDX}qDY  DY� DY�qDZ� D[  D[� D\�D\� D\�qD]}qD^  D^}qD_  D_� D`  D`� D`�qDa}qDb�Db��Db�qDc}qDd  Dd� Dd�qDe� Df�Df� Df�qDg}qDg�qDh}qDh�qDi}qDj  Dj��Dj�qDk}qDl  Dl}qDl�qDm� Dn  Dnz�Dn��Do� Dp�Dp� Dq  Dq� Dr  Dr��Ds  Ds� Ds�qDt}qDu  Du� Dv  Dv� Dv�qDw� Dx�Dx��Dy�Dy� Dy�qDz� Dz�qD{}qD{�qD|}qD}  D}��D~�D~� D  D� D�qD�@ D�� D�� D���D�@ D�� D���D���D�>�D�� D�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�~�D�� D�  D�>�D��HD��HD�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D�~�D���D�  D�@ D��HD�� D�  D�@ D�~�D�� D�HD�AHD��HD���D�  D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�� D���D�  D�AHD��HD��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�>�D�}qD��qD���D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D���D���D�>�D�� D���D���D�=qD�~�D�� D�  D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D���D���D�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D���D�HD�@ D�~�D�� D�HD�B�D�~�D���D���D�@ D�� D���D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D��HD�� D���D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�@ D�~�D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�@ D��HD�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�� D���D���D�@ D D¾�D�  D�@ DÀ Dþ�D�  D�@ DĀ D�� D���D�>�Dŀ D��HD�HD�@ D�~�D�� D���D�@ D�~�DǾ�D�HD�@ DȁHD�� D���D�>�D�~�D�� D�  D�>�Dʀ D��HD�HD�@ D�~�D˾�D�  D�@ D̀ D̾�D���D�>�D̀ D�� D�  D�AHD΁HD��HD�  D�>�D�~�DϾ�D�  D�AHDЁHD�� D�  D�>�D�~�DѽqD�  D�@ DҀ D�� D���D�>�D�~�DӾ�D�  D�>�DԀ D��HD�  D�=qD�}qDվ�D�  D�>�D�~�D�� D�HD�AHDׁHD��HD�HD�AHD؀ Dؾ�D�  D�AHDـ D�� D�HD�AHDڀ Dھ�D�  D�@ Dۀ D�� D�  D�>�D܀ D��HD�  D�@ D݀ D�� D�  D�@ DށHD�� D���D�>�D�~�D߾�D�  D�AHD�� DྸD�  D�@ D� D�� D�  D�>�D�~�D�� D�HD�AHD� D�� D���D�>�D�HD�� D�  D�AHD�HD�� D�  D�@ D� D澸D�  D�>�D�}qD羸D�  D�@ D�~�D�� D�HD�@ D�HD�� D�  D�>�D�~�D�� D�  D�>�D� D�� D���D�@ D�HD��HD�HD�AHD�HD��HD�  D�@ D� D�� D�HD�@ D�HD��HD���D�AHD�� D�� D�  D�@ D� D�D���D�@ D� D�D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�AHD�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D���D���?\)?.{?W
=?�  ?��?���?���?�Q�?�
=?�?��@   @
=q@z�@�R@#�
@+�@:�H@E�@J=q@Tz�@\(�@h��@u@z�H@�G�@�ff@��@���@�z�@�Q�@�p�@��\@��@��@�\)@�z�@���@�p�@\@�ff@�=q@�\)@�@ٙ�@�p�@�  @��@�=q@�\)@�33@�Q�@��HA   A�\A�A
=A	��A(�A{A��A33A�A
=A��A�A{A ��A#33A%�A'�A*=qA,��A/\)A1�A4z�A6ffA8��A<(�A>�RA@��AB�\AE�AG�AJ=qAL(�AN�RAQG�AS33AUAW�AZ=qA\��A_\)AaG�Ac�
AfffAh��Aj�HAmp�Ap  Ar�\Atz�Aw
=Ay��A{�A~{A�Q�A���A��\A��
A���A�ffA�\)A���A���A�33A�(�A�p�A��RA�  A�G�A�=qA��
A��A�{A�\)A���A��A��HA�z�A�p�A�ffA�  A���A�=qA��A���A�{A�
=A�Q�A���A��\A��A���A�ffA�\)A���A��A��HA�(�A�p�A��RA�  A�G�A�=qA��A���A�{A�\)A���A���A��HA�(�A��A�ffAǮA���A�=qA�33A�(�A�p�AθRAϮA���A��A�33A�(�A�p�A�ffA׮Aأ�A��A��HA�(�A��A�ffA�\)A��AᙚA��HA�(�A�p�A�ffA�A��A��A�A�z�A�A�RA�  A���A�=qA�A�z�A�A�
=A�Q�A�G�A��\A��
A��A�{A�\)B (�B ��B��B�B�RB33B�
Bz�B�B��B{B�RB\)B�
Bz�B��B	��B
{B
�RB33B�
BQ�B��Bp�B�B�\B
=B�B(�B��BG�B�BffB
=B�B(�B��BG�B�BffB�HB�B  B��B�BBffB�HB\)B  Bz�B�B��B=qB�HB\)B�
B z�B ��B!��B"{B"�RB#\)B#�
B$Q�B$��B%p�B&{B&�\B'33B'�B(Q�B(��B)p�B)�B*�\B+
=B+�B,(�B,��B-G�B-�B.ffB/
=B/�B0(�B0��B1G�B1B2ffB2�HB3�B4  B4��B5G�B5B6ffB6�HB7\)B7�
B8z�B9�B9��B:=qB:�RB;33B;�
B<Q�B<��B=p�B>{B>�\B?
=B?�B@Q�B@��BAG�BA�BBffBB�HBC�BD  BDz�BE�BEBF=qBF�RBG\)BG�
BHz�BH��BIp�BJ{BJ�\BK33BK�BLQ�BL��BMp�BM�BNffBO
=BO�BP(�BP��BQ�BQBR=qBR�RBS\)BT  BTz�BT��BU��BV{BV�\BW33BW�BX(�BX��BYG�BYBZ=qBZ�HB[�B\  B\z�B]�B]��B^{B^�RB_33B_�
B`Q�B`��Bap�Ba�BbffBc
=Bc�Bd(�Bd��Be�BeBf=qBf�HBg�Bh  Bhz�Bi�Bi��Bj{Bj�RBk33Bk�
BlQ�Bl��Bmp�Bn{Bn�\Bo33Bo�Bp(�Bp��Bqp�Bq�Br�\Bs
=Bs�Bt(�Bt��BuG�BuBvffBv�HBw\)Bx  Bxz�By�By��Bz=qBz�RB{\)B{�
B|Q�B|��B}p�B}�B~�\B33B�B�{B�ffB���B��HB��B�p�B�B�  B�=qB��\B���B��B�p�B��B��B�=qB�z�B���B�
=B�G�B���B��B�(�B�ffB���B���B�33B��B�B�{B�Q�B���B��HB��B�p�B��B�  B�Q�B��\B���B��B�\)B��B�  B�=qB�z�B���B��B�\)B���B��B�=qB�z�B��RB�
=B�G�B���B��B�(�B�z�B��RB�
=B�G�B��B��
B�(�B�ffB���B���B�G�B��B�B�{B�Q�B��\B��HB�33B�p�B�B�  B�Q�B��\B��HB��B�p�B��B�  B�=qB��\B���B��B�\)B���B��B�(�B�z�B��RB���B�G�B���B��
B�{B�Q�B���B���B�33B��B�B�{B�Q�B���B��HB�33B�p�B�B�  B�Q�B��\B��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B�  B�=qB��\B��HB��B�p�B��B�  B�Q�B��\B��HB�33B��B�B�  B�Q�B���B��HB�33B�p�B�B�  B�Q�B��\B��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B��B�=qB�z�B���B��B�\)B��B��B�=qB�z�B���B�
=B�\)B���B��B�=qB�z�B��RB�
=B�\)B��B��
B�{B�ffB��RB��HB�33B��B�B�  B�Q�B���B��HB��B�p�B��B�  B�Q�B�z�B���B��B�\)B��B��B�=qB�z�B���B��B�\)B���B��B�(�B�z�B��RB�
=B�G�B���B�B�{B�ffB���B��HB�33B�p�B��B�  B�=qB�z�B���B�
=B�G�B���B��
B�{B�ffB£�B���B�33BÅB��
B�{B�Q�Bģ�B��HB�33B�p�B�B�  B�Q�BƏ\B���B�
=B�\)BǙ�B��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   ?�  @�\@@  @�  @�  @�G�@�G�A   A��A#�
A@  A`��A�Q�A�  A��A�  A�Q�A�Q�A�  A�  B   B(�B  B  B (�B(  B/�
B8  B?�
BG�
BP(�BW�
B_�
Bh  Bp  Bx  B�
B��B�{B�{B�  B��B�  B��B�  B�(�B�  B�  B�{B�  B�  B�  B�  B�{B�  B�  B�  B�{B�{B�  B��B��B�  B��B��B��B�  B�{B��C  C  C
=C
=C	��C��C
=C
=C
=C
=C  C��C��C  C  C   C"  C$  C&  C(  C*  C,
=C.  C/��C2  C4  C6  C8  C:  C<  C>  C@  CB  CD
=CF  CG��CJ  CL  CM��CO��CR  CT  CU��CW��CY��C[��C]��C`  Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr  Cs��Cv  Cx
=Cz  C{��C~
=C�C�C�  C�  C�
=C�  C�  C���C���C���C���C�  C�C�C�C�  C���C�  C�  C�  C�  C���C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�  C���C�C�  C���C���C�C�  C���C���C���C�  C�  C�  C�  C���C���C�  C���C�  C�C�C�C�C�C�C�C�C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C���C�  C�  C���C�  C�  C�  C�C���C���C�  C�C�C�  C�  C�  C���C���C���C�  C�  C�C�C���C���C���C�  C�  C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  C���C���C���C�C�  C�  C�  C�  C�  C���D � D�D� D  D��D  D}qD�qD}qD  D� D�qD}qD  D}qD�qD��D	�D	��D
  D
}qD
�qD� D�D��D�D� D�qD� D  D}qD  D� D�qD� D�D��D�D� D�qD� D�D��D  Dz�D  D��D  D� D�qD� D�D��D  D� D  D��D  D� D  D}qD  D��D   D ��D!  D!}qD"  D"��D#  D#}qD$  D$� D%  D%� D%�qD&� D'  D'� D(�D(� D)  D)��D*  D*� D*�qD+}qD+�qD,� D,�qD-}qD.  D.� D/D/��D0  D0� D1  D1� D2  D2��D3  D3� D3�qD4}qD5  D5�D6D6� D7  D7� D8  D8� D9  D9� D9�qD:� D;  D;� D<�D<� D<�qD=� D>  D>� D?�D?}qD?�qD@� DA  DA� DB  DB� DC  DC� DD�DD� DD�qDE� DF�DF� DF��DG}qDG�qDH� DI  DI� DJ�DJ��DK�DK� DL  DL� DL�qDM� DM�qDN}qDO  DO� DP�DP��DQ�DQ��DR  DR� DS  DS��DT  DT}qDT�qDU}qDV  DV��DW�DW� DW�qDX}qDY  DY� DY�qDZ� D[  D[� D\�D\� D\�qD]}qD^  D^}qD_  D_� D`  D`� D`�qDa}qDb�Db��Db�qDc}qDd  Dd� Dd�qDe� Df�Df� Df�qDg}qDg�qDh}qDh�qDi}qDj  Dj��Dj�qDk}qDl  Dl}qDl�qDm� Dn  Dnz�Dn��Do� Dp�Dp� Dq  Dq� Dr  Dr��Ds  Ds� Ds�qDt}qDu  Du� Dv  Dv� Dv�qDw� Dx�Dx��Dy�Dy� Dy�qDz� Dz�qD{}qD{�qD|}qD}  D}��D~�D~� D  D� D�qD�@ D�� D�� D���D�@ D�� D���D���D�>�D�� D�� D�  D�>�D�~�D���D�  D�@ D�� D�� D���D�@ D��HD�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�~�D�� D�  D�>�D��HD��HD�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D��HD�� D�  D�@ D�~�D���D�  D�@ D��HD�� D�  D�@ D�~�D�� D�HD�AHD��HD���D�  D�@ D�� D��HD�HD�@ D�~�D�� D�  D�>�D�� D���D�  D�AHD��HD��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�@ D�� D�� D���D�>�D�~�D���D�  D�@ D�� D�� D�  D�>�D�}qD��qD���D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D���D���D�>�D�� D���D���D�=qD�~�D�� D�  D�>�D�~�D���D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D���D���D�>�D�� D�� D�HD�>�D�~�D�� D�  D�@ D�~�D���D�HD�@ D�~�D�� D�HD�B�D�~�D���D���D�@ D�� D���D�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D��HD�� D���D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D�~�D�� D�HD�@ D�~�D�� D���D�@ D�� D���D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�@ D��HD��HD�HD�@ D��HD�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�� D���D���D�@ D D¾�D�  D�@ DÀ Dþ�D�  D�@ DĀ D�� D���D�>�Dŀ D��HD�HD�@ D�~�D�� D���D�@ D�~�DǾ�D�HD�@ DȁHD�� D���D�>�D�~�D�� D�  D�>�Dʀ D��HD�HD�@ D�~�D˾�D�  D�@ D̀ D̾�D���D�>�D̀ D�� D�  D�AHD΁HD��HD�  D�>�D�~�DϾ�D�  D�AHDЁHD�� D�  D�>�D�~�DѽqD�  D�@ DҀ D�� D���D�>�D�~�DӾ�D�  D�>�DԀ D��HD�  D�=qD�}qDվ�D�  D�>�D�~�D�� D�HD�AHDׁHD��HD�HD�AHD؀ Dؾ�D�  D�AHDـ D�� D�HD�AHDڀ Dھ�D�  D�@ Dۀ D�� D�  D�>�D܀ D��HD�  D�@ D݀ D�� D�  D�@ DށHD�� D���D�>�D�~�D߾�D�  D�AHD�� DྸD�  D�@ D� D�� D�  D�>�D�~�D�� D�HD�AHD� D�� D���D�>�D�HD�� D�  D�AHD�HD�� D�  D�@ D� D澸D�  D�>�D�}qD羸D�  D�@ D�~�D�� D�HD�@ D�HD�� D�  D�>�D�~�D�� D�  D�>�D� D�� D���D�@ D�HD��HD�HD�AHD�HD��HD�  D�@ D� D�� D�HD�@ D�HD��HD���D�AHD�� D�� D�  D�@ D� D�D���D�@ D� D�D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�>�D�~�D��HD�HD�@ D�� D���D�  D�@ D�~�D�� D�HD�AHD�� D��HD�  D�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D���D���?\)?.{?W
=?�  ?��?���?���?�Q�?�
=?�?��@   @
=q@z�@�R@#�
@+�@:�H@E�@J=q@Tz�@\(�@h��@u@z�H@�G�@�ff@��@���@�z�@�Q�@�p�@��\@��@��@�\)@�z�@���@�p�@\@�ff@�=q@�\)@�@ٙ�@�p�@�  @��@�=q@�\)@�33@�Q�@��HA   A�\A�A
=A	��A(�A{A��A33A�A
=A��A�A{A ��A#33A%�A'�A*=qA,��A/\)A1�A4z�A6ffA8��A<(�A>�RA@��AB�\AE�AG�AJ=qAL(�AN�RAQG�AS33AUAW�AZ=qA\��A_\)AaG�Ac�
AfffAh��Aj�HAmp�Ap  Ar�\Atz�Aw
=Ay��A{�A~{A�Q�A���A��\A��
A���A�ffA�\)A���A���A�33A�(�A�p�A��RA�  A�G�A�=qA��
A��A�{A�\)A���A��A��HA�z�A�p�A�ffA�  A���A�=qA��A���A�{A�
=A�Q�A���A��\A��A���A�ffA�\)A���A��A��HA�(�A�p�A��RA�  A�G�A�=qA��A���A�{A�\)A���A���A��HA�(�A��A�ffAǮA���A�=qA�33A�(�A�p�AθRAϮA���A��A�33A�(�A�p�A�ffA׮Aأ�A��A��HA�(�A��A�ffA�\)A��AᙚA��HA�(�A�p�A�ffA�A��A��A�A�z�A�A�RA�  A���A�=qA�A�z�A�A�
=A�Q�A�G�A��\A��
A��A�{A�\)B (�B ��B��B�B�RB33B�
Bz�B�B��B{B�RB\)B�
Bz�B��B	��B
{B
�RB33B�
BQ�B��Bp�B�B�\B
=B�B(�B��BG�B�BffB
=B�B(�B��BG�B�BffB�HB�B  B��B�BBffB�HB\)B  Bz�B�B��B=qB�HB\)B�
B z�B ��B!��B"{B"�RB#\)B#�
B$Q�B$��B%p�B&{B&�\B'33B'�B(Q�B(��B)p�B)�B*�\B+
=B+�B,(�B,��B-G�B-�B.ffB/
=B/�B0(�B0��B1G�B1B2ffB2�HB3�B4  B4��B5G�B5B6ffB6�HB7\)B7�
B8z�B9�B9��B:=qB:�RB;33B;�
B<Q�B<��B=p�B>{B>�\B?
=B?�B@Q�B@��BAG�BA�BBffBB�HBC�BD  BDz�BE�BEBF=qBF�RBG\)BG�
BHz�BH��BIp�BJ{BJ�\BK33BK�BLQ�BL��BMp�BM�BNffBO
=BO�BP(�BP��BQ�BQBR=qBR�RBS\)BT  BTz�BT��BU��BV{BV�\BW33BW�BX(�BX��BYG�BYBZ=qBZ�HB[�B\  B\z�B]�B]��B^{B^�RB_33B_�
B`Q�B`��Bap�Ba�BbffBc
=Bc�Bd(�Bd��Be�BeBf=qBf�HBg�Bh  Bhz�Bi�Bi��Bj{Bj�RBk33Bk�
BlQ�Bl��Bmp�Bn{Bn�\Bo33Bo�Bp(�Bp��Bqp�Bq�Br�\Bs
=Bs�Bt(�Bt��BuG�BuBvffBv�HBw\)Bx  Bxz�By�By��Bz=qBz�RB{\)B{�
B|Q�B|��B}p�B}�B~�\B33B�B�{B�ffB���B��HB��B�p�B�B�  B�=qB��\B���B��B�p�B��B��B�=qB�z�B���B�
=B�G�B���B��B�(�B�ffB���B���B�33B��B�B�{B�Q�B���B��HB��B�p�B��B�  B�Q�B��\B���B��B�\)B��B�  B�=qB�z�B���B��B�\)B���B��B�=qB�z�B��RB�
=B�G�B���B��B�(�B�z�B��RB�
=B�G�B��B��
B�(�B�ffB���B���B�G�B��B�B�{B�Q�B��\B��HB�33B�p�B�B�  B�Q�B��\B��HB��B�p�B��B�  B�=qB��\B���B��B�\)B���B��B�(�B�z�B��RB���B�G�B���B��
B�{B�Q�B���B���B�33B��B�B�{B�Q�B���B��HB�33B�p�B�B�  B�Q�B��\B��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B�  B�=qB��\B��HB��B�p�B��B�  B�Q�B��\B��HB�33B��B�B�  B�Q�B���B��HB�33B�p�B�B�  B�Q�B��\B��HB�33B�p�B��B�  B�Q�B��\B��HB��B�p�B��B��B�=qB�z�B���B��B�\)B��B��B�=qB�z�B���B�
=B�\)B���B��B�=qB�z�B��RB�
=B�\)B��B��
B�{B�ffB��RB��HB�33B��B�B�  B�Q�B���B��HB��B�p�B��B�  B�Q�B�z�B���B��B�\)B��B��B�=qB�z�B���B��B�\)B���B��B�(�B�z�B��RB�
=B�G�B���B�B�{B�ffB���B��HB�33B�p�B��B�  B�=qB�z�B���B�
=B�G�B���B��
B�{B�ffB£�B���B�33BÅB��
B�{B�Q�Bģ�B��HB�33B�p�B�B�  B�Q�BƏ\B���B�
=B�\)BǙ�B��
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AZ9XAZA�AZA�AZ5?AZ=qAZ9XAZ1'AZ5?AZA�AZA�AZM�AZM�AZM�AZM�AZVAZn�AZbNAZ=qAZ-AY�AYAY33AX��AX1AV��AUt�ARI�AN�ALVAH�yAE"�AC�ACt�AC"�ACVAB��AB�/AB1A@�+A?�A<��A<bNA<�!A=�hA?VA?��A?�#A?��A@n�AA�7AA�;AB=qACO�AC��ADAD�`AE�FAE��AF�AFz�AGS�AGAG��AG�wAG/AF�+AF{AF(�AFE�AFQ�AFM�AFQ�AFE�AD��AA�^A=�A:M�A7�PA5��A5ƨA4��A4A3��A4�DA5"�A533A4ĜA3�-A3?}A2�A2~�A1A0ZA-�PA+�TA)�A'��A'�FA'��A'S�A&�DA%�
A%O�A%oA$�HA$5?A#K�A"ȴA"�A!7LA �jA z�A �A �RA ��A �A �+A VA �A�^A7LA�A��A�HA�AZA1'A��AK�A��AE�A��A��AE�AI�AVA�A�jA�9A�\AE�AA�
A�wA�FA�hAO�AĜA�/AffA(�A��A�RAI�A{A��A�A�
A�mA{AƨAA�FA`BAoA�7A��A��A|�AC�AO�A
v�A
n�A
�+A
z�A
��A
��A
�A
�A	�;A	%A�RA1A�!A�A��AȴA��A^5A�PAQ�A?}A ��A VA (�@�l�@��@�Z@�
=@�p�@�A�@���@��;@�@�-@��@�u@�F@�~�@�z�@��@�K�@�o@��@���@���@��m@�+@�=q@�V@�;d@ߕ�@܃@���@�ȴ@�$�@�hs@���@�O�@�Ĝ@Ԭ@� �@�t�@�C�@�^5@�`B@�S�@��H@��H@��y@�+@�X@�5?@�Z@Ə\@�/@�Ĝ@�(�@þw@�|�@�33@��y@�@�M�@��@�?}@�Ĝ@�9X@���@�@�ff@��@��D@�=q@�ȴ@���@��/@��m@���@��@�dZ@�$�@��@���@���@��/@���@��@�z�@�Q�@��@�$�@��7@�?}@���@���@�9X@��@�dZ@�@��H@���@�v�@�ff@�^5@�@���@���@�?}@��/@�z�@��@� �@�l�@�"�@��@���@�{@��^@���@�&�@��9@�Z@��m@���@��#@�x�@�p�@�7L@�V@�%@�%@��`@���@��u@�Z@�(�@���@�~�@��@�z�@�K�@�ff@�x�@�+@���@���@���@�1@��@���@��@��@��/@���@��@�%@���@��
@��@�l�@�S�@�K�@�|�@��w@��;@��@�r�@�z�@�G�@�`B@���@�@��h@��P@��+@�v�@�ff@��@���@���@��@�@��@�7L@��@��9@�A�@� �@�(�@�A�@�b@�  @�A�@��@�`B@��T@�{@�^5@��R@�dZ@��u@���@���@��@�o@���@�ȴ@�ȴ@��R@���@���@��R@��!@��+@�E�@�{@���@��@��#@��#@�@��^@��h@�hs@�G�@��@���@���@��/@��j@��@�9X@�(�@� �@��w@�33@��y@���@�^5@�$�@���@��@�j@�9X@�b@��@��@�w@�P@l�@l�@~�y@~V@~5?@}�@}@}�h@}O�@|�@|��@|��@|j@|(�@{t�@{@z~�@z-@y�#@y��@y�7@y&�@x��@xĜ@x��@x�@xQ�@w|�@v5?@u@u�-@u��@u�h@up�@u�@uV@uV@t�j@s��@sƨ@s�F@t9X@t�j@t�@u�T@v�+@v�@w;d@x  @yG�@zM�@z��@{33@{�F@{�m@|9X@|j@|�D@|�@|�/@|��@|1@{��@{C�@z��@z^5@z�@y�@y��@y7L@x��@x�u@w��@v��@v��@vff@vE�@v{@u@u�h@u/@t��@t��@tz�@t�@s�@so@s@r��@r-@qx�@q7L@q%@p�`@p�u@pb@o|�@nȴ@n�+@n{@m@m?}@k�
@j��@jM�@i7L@i7L@iG�@i��@i��@iX@hĜ@f$�@d�/@dZ@cƨ@b-@`Ĝ@_��@_K�@^ȴ@^$�@]O�@[��@[C�@Z��@Y7L@X �@X�9@W�@V$�@T9X@Rn�@Q��@PbN@N��@L�/@L(�@J��@JM�@J-@I��@H��@H��@HĜ@HQ�@G�@Gl�@F�@Fȴ@Fȴ@F��@Gl�@G��@G�@G�w@G�;@G�;@G�@Hb@HbN@HbN@HA�@HA�@HbN@HbN@H�@HbN@HQ�@G;d@F�@F��@F$�@E�@E@E��@E�h@E?}@D��@D��@D�j@D�@D�@D��@D��@Dz�@D��@D�@D�/@EV@D��@E�@E�@F@E�h@E�-@Fv�@G;d@G�@H��@I�#@J-@I�^@I�7@J�@Kt�@M/@N{@Nv�@Nȴ@Nȴ@N�@N�+@NE�@N$�@N$�@M�@M�@M@MO�@M?}@L��@Lj@K�m@K�F@K��@K�@KS�@K"�@J�H@J��@J�!@Jn�@JM�@J-@J�@I��@I��@I�#@I��@I��@I�7@I�@H�`@HĜ@H�9@H��@Hr�@HbN@Hb@G�;@G�;@G��@G\)@G+@F�R@E`B@C�
@B��@B�@A&�@@�9@@r�@@1'@?�@?�@>E�@=�@=��@=�h@=/@<��@<�@<1@;�m@;�F@;�@;dZ@;C�@;33@;"�@;o@:��@:��@:=q@9�@9�#@9��@9�7@9hs@97L@9�@8Ĝ@8bN@8 �@8  @7��@7�w@7�w@7�@7�P@7�P@7\)@7K�@7K�@7K�@7K�@7+@6��@6�R@6��@6v�@6V@6V@6V@6E�@65?@6@5�T@5�-@5��@5`B@5/@5�@4��@4�j@4�D@4Z@4I�@4I�@41@3�F@3ƨ@3ƨ@3�F@3��@333@3"�@333@3C�@3S�@3�@41@4(�@4Z@4�D@4�D@4z�@4z�@4I�@49X@4�@4��@4�/@4�@4�@4�@4�@4�@4�/@4�/@4�/@4�/@4�@4�@4�@4�@4�@4�/@4�j@4��@4�D@4z�@4j@4Z@4I�@4�@4�@41@41@3��@3�
@3ƨ@3ƨ@3��@3��@3�@3t�@3t�@3�@3�@3�@3�@3t�@3t�@3dZ@3S�@3S�@3C�@3"�@3o@3@3@2�@2�H@2��@2�!@2�\@2�\@2~�@2~�@2^5@2-@2�@2J@2J@2J@1��@1��@1�@1�^@1�^@1�^@1��@1��@1hs@17L@1�@1%@0��@0��@0��@0�`@0�`@0��@0�9@0�u@0�@0�@0r�@0bN@0Q�@0A�@01'@0b@/�;@/�w@/��@/|�@/|�@/l�@/l�@/\)@/\)@/K�@/;d@/
=@.��@.�@.�@.�@.�y@.�y@.�y@.�y@.�@.ȴ@.�+@.v�@.v�@.ff@.V@.E�@.$�@.{@.@-�@-�T@-�T@-��@-��@-@-�-@-�-@-�h@-`B@-?}@-V@,��@,�j@,��@,z�@,�D@,Z@,9X@,(�@,�@,1@+�m@+�m@+�m@+�
@+�
@+ƨ@+��@+�@+t�@+C�@+"�@+"�@+o@+@*�@*�H@*�H@*��@*��@*��@*��@*��@*��@*��@*��@*�!@*�\@*~�@*=q@*-@*�@*�@*�@*J@*J@)��@)�@)�#@)��@)�^@)��@)��@)�7@)hs@)7L@)�@)%@)%@)%@(��@(��@(�`@(�9@(r�@(r�@(bN@(bN@(Q�@( �@(b@(  @'�@'��@'��@'�@'��@'��@'�@'�@'�P@'|�@'l�@'\)@'\)AZ-AZ1'AZ1'AZ1'AZ9XAZA�AZ=qAZA�AZE�AZA�AZ=qAZA�AZA�AZ9XAZ9XAZA�AZA�AZA�AZA�AZE�AZA�AZA�AZ9XAZ1'AZ5?AZ5?AZ9XAZ5?AZ=qAZ=qAZ9XAZ9XAZ=qAZ=qAZ9XAZ9XAZ9XAZ5?AZ9XAZ=qAZ1'AZ5?AZ=qAZ5?AZ5?AZ5?AZ5?AZ1'AZ-AZ-AZ(�AZ(�AZ5?AZ5?AZ9XAZ=qAZ=qAZ=qAZ=qAZA�AZA�AZA�AZA�AZ=qAZ9XAZ=qAZA�AZA�AZA�AZA�AZ=qAZE�AZVAZQ�AZI�AZM�AZI�AZI�AZM�AZM�AZI�AZI�AZQ�AZQ�AZQ�AZVAZQ�AZQ�AZQ�AZQ�AZM�AZM�AZQ�AZQ�AZI�AZM�AZI�AZI�AZM�AZM�AZM�AZM�AZM�AZI�AZM�AZQ�AZM�AZQ�AZQ�AZM�AZQ�AZQ�AZQ�AZVAZQ�AZbNAZbNAZ^5AZI�AZE�AZ9XAZA�AZE�AZE�AZ=qAZE�AZI�AZE�AZ9XAZ1'AZ9XAZA�AZffAZbNAZ^5AZbNAZbNAZbNAZffAZffAZjAZjAZn�AZn�AZn�AZn�AZn�AZn�AZr�AZn�AZr�AZr�AZn�AZjAZjAZffAZbNAZ^5AZ^5AZZAZjAZn�AZv�AZn�AZZAZQ�AZQ�AZM�AZQ�AZE�AZ9XAZ9XAZ=qAZ9XAZ9XAZ=qAZ9XAZ=qAZA�AZA�AZ9XAZ5?AZ1'AZ1'AZ5?AZ9XAZ5?AZ5?AZ1'AZ-AZ-AZ-AZ1'AZ$�AZ�AY�;AY�AY�AY��AY�TAY�mAY��AY��AY��AY��AZAZ1AZ1AZAY�AY�TAY�#AY�
AY�
AY�TAY�
AY�TAY�#AY��AY�7AY`BAYXAYx�AYS�AYS�AYK�AY7LAY/AY"�AY/AY\)AY�AYVAYVAYVAX��AY�AYoAYVAYAX��AYVAYAYVAX�RAX�+AXr�AX^5AXVAXv�AXffAXI�AX1'AX�AXJAX{AX�AX5?AX1'AXAWAW�;AW��AW��AWƨAW�AW��AW��AWS�AW33AWoAV��AV�!AV�uAV��AV��AV�9AV��AVr�AVr�AVjAVA�AV9XAV9XAU�wAUO�AU&�AT�AT��AT��AT�/AT�ATE�AR�AR�jAR�AR�ARr�ARI�ARM�ARQ�AR=qARE�AR1'AR �AQƨAQ�AQ
=AO��AN��AO%AOC�AOC�AN��AN�jAN�AN�RAN~�ANbNANQ�ANQ�AN-AM�AM�mAM�#AM��AM%ALVAL=qAL{AK��AKXAKXAKK�AK+AJȴAJ�9AJ�DAJ��AJz�AJA�AIhsAHr�AH-AH1AG�;AG�#AG�
AG�AG�mAG��AG��AG;dAF�/AF9XAE��AE�AD�+ADn�ADffADQ�AD$�AD5?AD-ADbADJAD{AD{ADbAC��AC�AC�AC�AC�AC�AC�#AC�TAC��AC�wAC�^AC�AC��AC��AC�hAC|�ACt�ACp�AChsAChsACdZAC\)ACdZAC`BACS�ACS�ACS�ACO�AC?}AC;dAC33AC/AC+AC�ACVACoACoAC
=AC%ACAB�yAB�yAB�AB��AC%ACAC
=AC�AC�AC&�AC�AC�AC"�AC�AC�AC
=AB��AB��AB��ACVAC
=AB�AB�`AB��ACAB��AB��AB�AB�AB��AB��AB��AB��AB��AB�AB�AB��AB��AB�AB�ABĜAB��AB��AB�ABr�ABn�ABz�AB~�AB��AB�uAB�\AB�DAB5?AA�^AA�hAAt�AA&�AA%AA"�A@�A@�A@�DA@ȴA@r�A@n�A@~�A@v�A@v�A@v�A@jA@jA@z�A@~�A@z�A@~�A@�+A@z�A@�\A@��A@r�A@=qA@I�A?oA>��A>�DA>A�A=�^A=l�A=oA<�A<�RA<��A<�9A<�9A<��A<��A<��A<��A=�A=%A<��A<��A<VA<r�A<�A<��A<�+A<�A<jA<E�A<VA<1'A<1'A<Q�A<E�A<1'A<5?A<VA<ffA<M�A<Q�A<ffA<�9A<��A=%A<��A=VA=%A=%A<�A<�HA<��A=/A=7LA=x�A=�wA=x�A=dZA=�hA=�7A=�7A=�PA=�A>1'A>=qA>ZA>M�A>=qA>~�A?G�A?C�A?A?A?oA?+A?7LA?S�A?�wA?��A?��A?�A?�A?��A?��A?��A?��A?�hA?�A?�A?|�A?�A?�hA?��A?�wA?A?ƨA?�
A?�TA?�TA?��A?�A?�mA?��A?�wA?�TA?��A?�A@{A?�wA?�FA?A?�
A?��A?�^A?�^A?�#A?��A?��A?��A?�^A?ƨA?�
A?��A?ƨA?��A?�TA?�A@�A@E�A@Q�A@r�A@��A@�AAS�AA�AA��AA��AA�AA�AA�PAA��AA�PAA��AA�PAA|�AA��AA�AA|�AA|�AA�AAx�AA|�AAt�AA�AA�FABbAB  AB1'AB�ABJAB�AB�AB  AA�AA�AA�;AA��AB(�ABQ�AA��AA�AA�FAAƨAA�ABQ�AB��ACO�AC�AC"�AC�AB�uAB�ACl�AC�AC��AC|�ACl�AC`BAC\)ACO�AC`BAChsAChsACt�AC�AChsACt�AC�AC�AC|�AC�7ACl�AC�7AC�AC�
AC�;AC�AC�AC�AC�AC�AC��AC��ADADJAD=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   AZ9XAZA�AZA�AZ5?AZ=qAZ9XAZ1'AZ5?AZA�AZA�AZM�AZM�AZM�AZM�AZVAZn�AZbNAZ=qAZ-AY�AYAY33AX��AX1AV��AUt�ARI�AN�ALVAH�yAE"�AC�ACt�AC"�ACVAB��AB�/AB1A@�+A?�A<��A<bNA<�!A=�hA?VA?��A?�#A?��A@n�AA�7AA�;AB=qACO�AC��ADAD�`AE�FAE��AF�AFz�AGS�AGAG��AG�wAG/AF�+AF{AF(�AFE�AFQ�AFM�AFQ�AFE�AD��AA�^A=�A:M�A7�PA5��A5ƨA4��A4A3��A4�DA5"�A533A4ĜA3�-A3?}A2�A2~�A1A0ZA-�PA+�TA)�A'��A'�FA'��A'S�A&�DA%�
A%O�A%oA$�HA$5?A#K�A"ȴA"�A!7LA �jA z�A �A �RA ��A �A �+A VA �A�^A7LA�A��A�HA�AZA1'A��AK�A��AE�A��A��AE�AI�AVA�A�jA�9A�\AE�AA�
A�wA�FA�hAO�AĜA�/AffA(�A��A�RAI�A{A��A�A�
A�mA{AƨAA�FA`BAoA�7A��A��A|�AC�AO�A
v�A
n�A
�+A
z�A
��A
��A
�A
�A	�;A	%A�RA1A�!A�A��AȴA��A^5A�PAQ�A?}A ��A VA (�@�l�@��@�Z@�
=@�p�@�A�@���@��;@�@�-@��@�u@�F@�~�@�z�@��@�K�@�o@��@���@���@��m@�+@�=q@�V@�;d@ߕ�@܃@���@�ȴ@�$�@�hs@���@�O�@�Ĝ@Ԭ@� �@�t�@�C�@�^5@�`B@�S�@��H@��H@��y@�+@�X@�5?@�Z@Ə\@�/@�Ĝ@�(�@þw@�|�@�33@��y@�@�M�@��@�?}@�Ĝ@�9X@���@�@�ff@��@��D@�=q@�ȴ@���@��/@��m@���@��@�dZ@�$�@��@���@���@��/@���@��@�z�@�Q�@��@�$�@��7@�?}@���@���@�9X@��@�dZ@�@��H@���@�v�@�ff@�^5@�@���@���@�?}@��/@�z�@��@� �@�l�@�"�@��@���@�{@��^@���@�&�@��9@�Z@��m@���@��#@�x�@�p�@�7L@�V@�%@�%@��`@���@��u@�Z@�(�@���@�~�@��@�z�@�K�@�ff@�x�@�+@���@���@���@�1@��@���@��@��@��/@���@��@�%@���@��
@��@�l�@�S�@�K�@�|�@��w@��;@��@�r�@�z�@�G�@�`B@���@�@��h@��P@��+@�v�@�ff@��@���@���@��@�@��@�7L@��@��9@�A�@� �@�(�@�A�@�b@�  @�A�@��@�`B@��T@�{@�^5@��R@�dZ@��u@���@���@��@�o@���@�ȴ@�ȴ@��R@���@���@��R@��!@��+@�E�@�{@���@��@��#@��#@�@��^@��h@�hs@�G�@��@���@���@��/@��j@��@�9X@�(�@� �@��w@�33@��y@���@�^5@�$�@���@��@�j@�9X@�b@��@��@�w@�P@l�@l�@~�y@~V@~5?@}�@}@}�h@}O�@|�@|��@|��@|j@|(�@{t�@{@z~�@z-@y�#@y��@y�7@y&�@x��@xĜ@x��@x�@xQ�@w|�@v5?@u@u�-@u��@u�h@up�@u�@uV@uV@t�j@s��@sƨ@s�F@t9X@t�j@t�@u�T@v�+@v�@w;d@x  @yG�@zM�@z��@{33@{�F@{�m@|9X@|j@|�D@|�@|�/@|��@|1@{��@{C�@z��@z^5@z�@y�@y��@y7L@x��@x�u@w��@v��@v��@vff@vE�@v{@u@u�h@u/@t��@t��@tz�@t�@s�@so@s@r��@r-@qx�@q7L@q%@p�`@p�u@pb@o|�@nȴ@n�+@n{@m@m?}@k�
@j��@jM�@i7L@i7L@iG�@i��@i��@iX@hĜ@f$�@d�/@dZ@cƨ@b-@`Ĝ@_��@_K�@^ȴ@^$�@]O�@[��@[C�@Z��@Y7L@X �@X�9@W�@V$�@T9X@Rn�@Q��@PbN@N��@L�/@L(�@J��@JM�@J-@I��@H��@H��@HĜ@HQ�@G�@Gl�@F�@Fȴ@Fȴ@F��@Gl�@G��@G�@G�w@G�;@G�;@G�@Hb@HbN@HbN@HA�@HA�@HbN@HbN@H�@HbN@HQ�@G;d@F�@F��@F$�@E�@E@E��@E�h@E?}@D��@D��@D�j@D�@D�@D��@D��@Dz�@D��@D�@D�/@EV@D��@E�@E�@F@E�h@E�-@Fv�@G;d@G�@H��@I�#@J-@I�^@I�7@J�@Kt�@M/@N{@Nv�@Nȴ@Nȴ@N�@N�+@NE�@N$�@N$�@M�@M�@M@MO�@M?}@L��@Lj@K�m@K�F@K��@K�@KS�@K"�@J�H@J��@J�!@Jn�@JM�@J-@J�@I��@I��@I�#@I��@I��@I�7@I�@H�`@HĜ@H�9@H��@Hr�@HbN@Hb@G�;@G�;@G��@G\)@G+@F�R@E`B@C�
@B��@B�@A&�@@�9@@r�@@1'@?�@?�@>E�@=�@=��@=�h@=/@<��@<�@<1@;�m@;�F@;�@;dZ@;C�@;33@;"�@;o@:��@:��@:=q@9�@9�#@9��@9�7@9hs@97L@9�@8Ĝ@8bN@8 �@8  @7��@7�w@7�w@7�@7�P@7�P@7\)@7K�@7K�@7K�@7K�@7+@6��@6�R@6��@6v�@6V@6V@6V@6E�@65?@6@5�T@5�-@5��@5`B@5/@5�@4��@4�j@4�D@4Z@4I�@4I�@41@3�F@3ƨ@3ƨ@3�F@3��@333@3"�@333@3C�@3S�@3�@41@4(�@4Z@4�D@4�D@4z�@4z�@4I�@49X@4�@4��@4�/@4�@4�@4�@4�@4�@4�/@4�/@4�/@4�/@4�@4�@4�@4�@4�@4�/@4�j@4��@4�D@4z�@4j@4Z@4I�@4�@4�@41@41@3��@3�
@3ƨ@3ƨ@3��@3��@3�@3t�@3t�@3�@3�@3�@3�@3t�@3t�@3dZ@3S�@3S�@3C�@3"�@3o@3@3@2�@2�H@2��@2�!@2�\@2�\@2~�@2~�@2^5@2-@2�@2J@2J@2J@1��@1��@1�@1�^@1�^@1�^@1��@1��@1hs@17L@1�@1%@0��@0��@0��@0�`@0�`@0��@0�9@0�u@0�@0�@0r�@0bN@0Q�@0A�@01'@0b@/�;@/�w@/��@/|�@/|�@/l�@/l�@/\)@/\)@/K�@/;d@/
=@.��@.�@.�@.�@.�y@.�y@.�y@.�y@.�@.ȴ@.�+@.v�@.v�@.ff@.V@.E�@.$�@.{@.@-�@-�T@-�T@-��@-��@-@-�-@-�-@-�h@-`B@-?}@-V@,��@,�j@,��@,z�@,�D@,Z@,9X@,(�@,�@,1@+�m@+�m@+�m@+�
@+�
@+ƨ@+��@+�@+t�@+C�@+"�@+"�@+o@+@*�@*�H@*�H@*��@*��@*��@*��@*��@*��@*��@*��@*�!@*�\@*~�@*=q@*-@*�@*�@*�@*J@*J@)��@)�@)�#@)��@)�^@)��@)��@)�7@)hs@)7L@)�@)%@)%@)%@(��@(��@(�`@(�9@(r�@(r�@(bN@(bN@(Q�@( �@(b@(  @'�@'��@'��@'�@'��@'��@'�@'�@'�P@'|�@'l�@'\)@'\)AZ-AZ1'AZ1'AZ1'AZ9XAZA�AZ=qAZA�AZE�AZA�AZ=qAZA�AZA�AZ9XAZ9XAZA�AZA�AZA�AZA�AZE�AZA�AZA�AZ9XAZ1'AZ5?AZ5?AZ9XAZ5?AZ=qAZ=qAZ9XAZ9XAZ=qAZ=qAZ9XAZ9XAZ9XAZ5?AZ9XAZ=qAZ1'AZ5?AZ=qAZ5?AZ5?AZ5?AZ5?AZ1'AZ-AZ-AZ(�AZ(�AZ5?AZ5?AZ9XAZ=qAZ=qAZ=qAZ=qAZA�AZA�AZA�AZA�AZ=qAZ9XAZ=qAZA�AZA�AZA�AZA�AZ=qAZE�AZVAZQ�AZI�AZM�AZI�AZI�AZM�AZM�AZI�AZI�AZQ�AZQ�AZQ�AZVAZQ�AZQ�AZQ�AZQ�AZM�AZM�AZQ�AZQ�AZI�AZM�AZI�AZI�AZM�AZM�AZM�AZM�AZM�AZI�AZM�AZQ�AZM�AZQ�AZQ�AZM�AZQ�AZQ�AZQ�AZVAZQ�AZbNAZbNAZ^5AZI�AZE�AZ9XAZA�AZE�AZE�AZ=qAZE�AZI�AZE�AZ9XAZ1'AZ9XAZA�AZffAZbNAZ^5AZbNAZbNAZbNAZffAZffAZjAZjAZn�AZn�AZn�AZn�AZn�AZn�AZr�AZn�AZr�AZr�AZn�AZjAZjAZffAZbNAZ^5AZ^5AZZAZjAZn�AZv�AZn�AZZAZQ�AZQ�AZM�AZQ�AZE�AZ9XAZ9XAZ=qAZ9XAZ9XAZ=qAZ9XAZ=qAZA�AZA�AZ9XAZ5?AZ1'AZ1'AZ5?AZ9XAZ5?AZ5?AZ1'AZ-AZ-AZ-AZ1'AZ$�AZ�AY�;AY�AY�AY��AY�TAY�mAY��AY��AY��AY��AZAZ1AZ1AZAY�AY�TAY�#AY�
AY�
AY�TAY�
AY�TAY�#AY��AY�7AY`BAYXAYx�AYS�AYS�AYK�AY7LAY/AY"�AY/AY\)AY�AYVAYVAYVAX��AY�AYoAYVAYAX��AYVAYAYVAX�RAX�+AXr�AX^5AXVAXv�AXffAXI�AX1'AX�AXJAX{AX�AX5?AX1'AXAWAW�;AW��AW��AWƨAW�AW��AW��AWS�AW33AWoAV��AV�!AV�uAV��AV��AV�9AV��AVr�AVr�AVjAVA�AV9XAV9XAU�wAUO�AU&�AT�AT��AT��AT�/AT�ATE�AR�AR�jAR�AR�ARr�ARI�ARM�ARQ�AR=qARE�AR1'AR �AQƨAQ�AQ
=AO��AN��AO%AOC�AOC�AN��AN�jAN�AN�RAN~�ANbNANQ�ANQ�AN-AM�AM�mAM�#AM��AM%ALVAL=qAL{AK��AKXAKXAKK�AK+AJȴAJ�9AJ�DAJ��AJz�AJA�AIhsAHr�AH-AH1AG�;AG�#AG�
AG�AG�mAG��AG��AG;dAF�/AF9XAE��AE�AD�+ADn�ADffADQ�AD$�AD5?AD-ADbADJAD{AD{ADbAC��AC�AC�AC�AC�AC�AC�#AC�TAC��AC�wAC�^AC�AC��AC��AC�hAC|�ACt�ACp�AChsAChsACdZAC\)ACdZAC`BACS�ACS�ACS�ACO�AC?}AC;dAC33AC/AC+AC�ACVACoACoAC
=AC%ACAB�yAB�yAB�AB��AC%ACAC
=AC�AC�AC&�AC�AC�AC"�AC�AC�AC
=AB��AB��AB��ACVAC
=AB�AB�`AB��ACAB��AB��AB�AB�AB��AB��AB��AB��AB��AB�AB�AB��AB��AB�AB�ABĜAB��AB��AB�ABr�ABn�ABz�AB~�AB��AB�uAB�\AB�DAB5?AA�^AA�hAAt�AA&�AA%AA"�A@�A@�A@�DA@ȴA@r�A@n�A@~�A@v�A@v�A@v�A@jA@jA@z�A@~�A@z�A@~�A@�+A@z�A@�\A@��A@r�A@=qA@I�A?oA>��A>�DA>A�A=�^A=l�A=oA<�A<�RA<��A<�9A<�9A<��A<��A<��A<��A=�A=%A<��A<��A<VA<r�A<�A<��A<�+A<�A<jA<E�A<VA<1'A<1'A<Q�A<E�A<1'A<5?A<VA<ffA<M�A<Q�A<ffA<�9A<��A=%A<��A=VA=%A=%A<�A<�HA<��A=/A=7LA=x�A=�wA=x�A=dZA=�hA=�7A=�7A=�PA=�A>1'A>=qA>ZA>M�A>=qA>~�A?G�A?C�A?A?A?oA?+A?7LA?S�A?�wA?��A?��A?�A?�A?��A?��A?��A?��A?�hA?�A?�A?|�A?�A?�hA?��A?�wA?A?ƨA?�
A?�TA?�TA?��A?�A?�mA?��A?�wA?�TA?��A?�A@{A?�wA?�FA?A?�
A?��A?�^A?�^A?�#A?��A?��A?��A?�^A?ƨA?�
A?��A?ƨA?��A?�TA?�A@�A@E�A@Q�A@r�A@��A@�AAS�AA�AA��AA��AA�AA�AA�PAA��AA�PAA��AA�PAA|�AA��AA�AA|�AA|�AA�AAx�AA|�AAt�AA�AA�FABbAB  AB1'AB�ABJAB�AB�AB  AA�AA�AA�;AA��AB(�ABQ�AA��AA�AA�FAAƨAA�ABQ�AB��ACO�AC�AC"�AC�AB�uAB�ACl�AC�AC��AC|�ACl�AC`BAC\)ACO�AC`BAChsAChsACt�AC�AChsACt�AC�AC�AC|�AC�7ACl�AC�7AC�AC�
AC�;AC�AC�AC�AC�AC�AC��AC��ADADJAD=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�#B�#B�B�B�B�)B�#B�B�B��B��B��BǮB��B�-B��B� BXB49BB
�#B
��B
�jB
�XB
�RB
�RB
�9B
��B
�hB
�DB
`BB
ZB
ZB
m�B
�B
�\B
�uB
�{B
��B
�'B
�FB
�wB
��B
��B
�/B
�fB
��B
��BB
=B!�B1'BN�Bn�B�\B��B�RBȴB��B��B��B��B��B��B��B�1BR�B:^B"�B.B)�B �B�B,B;dB=qBA�B?}B;dB8RB33B,B �BB
�B
�BB
��B
ŢB
��B
��B
ĜB
�}B
�RB
�FB
�3B
�B
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
�{B
�{B
�bB
�PB
�DB
�%B
{�B
v�B
r�B
q�B
hsB
O�B
J�B
I�B
J�B
L�B
N�B
O�B
Q�B
Q�B
S�B
S�B
S�B
\)B
]/B
YB
XB
H�B
I�B
K�B
M�B
N�B
S�B
XB
bNB
ffB
iyB
m�B
r�B
o�B
[#B
\)B
\)B
ZB
^5B
ffB
cTB
cTB
ffB
ffB
iyB
r�B
y�B
y�B
{�B
s�B
p�B
m�B
cTB
^5B
ZB
VB
T�B
S�B
P�B
J�B
G�B
B�B
?}B
>wB
<jB
7LB
1'B
-B
(�B
$�B
�B
�B
�B
uB
bB
VB
JB
	7B
B
B
B
B
B
  B	��B	��B	�B	�B	�B	�ZB	�)B	��B	��B	��B	ȴB	ƨB	��B	�jB	�XB	�RB	�LB	�?B	�3B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	�{B	�\B	�JB	�=B	�7B	�+B	�+B	�%B	�B	�B	�B	�B	�B	~�B	}�B	{�B	y�B	w�B	u�B	p�B	m�B	bNB	^5B	\)B	YB	T�B	Q�B	K�B	H�B	D�B	A�B	=qB	8RB	7LB	6FB	5?B	5?B	33B	1'B	/B	/B	.B	.B	,B	)�B	(�B	'�B	&�B	&�B	%�B	$�B	$�B	$�B	"�B	"�B	 �B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	{B	{B	�B	�B	{B	{B	{B	{B	uB	uB	oB	hB	\B	VB	1B	B	B��B��B��B�B�B�B�B��B��B��B��B��B�B�B�B�sB�yB�B�B�B�B�B�B�B�B��B��B��B	  B	B	DB	
=B	+B	B	B	B	B	B��B��B��B	  B��B��B��B��B��B��B	  B	  B	  B	B	1B	
=B	VB	\B	oB	{B	�B	 �B	'�B	49B	:^B	:^B	;dB	=qB	?}B	@�B	@�B	@�B	B�B	D�B	G�B	I�B	J�B	J�B	K�B	K�B	L�B	L�B	L�B	M�B	M�B	M�B	P�B	XB	YB	ZB	[#B	[#B	\)B	]/B	]/B	_;B	aHB	aHB	bNB	bNB	bNB	cTB	dZB	e`B	e`B	ffB	hsB	hsB	hsB	hsB	hsB	hsB	k�B	n�B	n�B	o�B	p�B	p�B	r�B	s�B	t�B	u�B	v�B	w�B	x�B	x�B	z�B	{�B	|�B	|�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�%B	�%B	�+B	�+B	�+B	�+B	�1B	�7B	�7B	�DB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	�B	�!B	�3B	�FB	�LB	�XB	�^B	�dB	�jB	�}B	B	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�)B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�`B	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�mB	�mB	�fB	�ZB	�TB	�HB	�HB	�;B	�5B	�BB	�ZB	�BB	�BB	�/B	�)B	�#B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�;B	�HB	�NB	�TB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
%B
	7B

=B
DB
VB
bB
hB
oB
{B
�B
�B
�B
�B
!�B
#�B
'�B
+B
+B
,B
/B
33B
7LB
<jB
>wB
@�B
@�B
A�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
H�B
H�B
I�B
J�B
J�B
K�B
M�B
N�B
O�B
P�B
T�B
VB
XB
ZB
[#B
[#B
\)B
_;B
_;B
_;B
cTB
dZB
e`B
e`B
e`B
ffB
gmB
hsB
hsB
hsB
iyB
jB
jB
k�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
u�B
v�B
v�B
w�B
x�B
y�B
z�B
|�B
�B
�B
�+B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�JB
�JB
�JB
�JB
�PB
�VB
�\B
�bB
�hB
�oB
�oB
�oB
�uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�-B
�3B
�9B
�?B
�FB
�LB
�LB
�XB
�^B
�jB
��B
��B
B
B
B
B
B
ĜB
ĜB
ŢB
ŢB
ŢB
ƨB
ǮB
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
�B
�B
�
B
�
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�)B
�)B
�)B
�)B
�/B
�5B
�;B
�;B
�BB
�BB
�BB
�HB
�NB
�TB
�TB
�TB
�TB
�ZB
�ZB
�ZB
�`B
�fB
�fB
�fB
�fB
�mB
�yB
�yB
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
��B  B  B  BBBBBBBBB%B+B+B1B1B	7B
=B
=BDBDBJBJBPBPBVBVB\B\B\BbBbBbBbBbBbBbBhBhBhBhBhBoBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B�B�B�B�#B�B�#B�#B�B�B�B�#B�B�B�)B�B�B�#B�#B�B�B�#B�)B�B�#B�B�#B�#B�B�B�B�#B�#B�B�B�#B�#B�#B�B�B�B�/B�B�B�#B�#B�B�B�#B�#B�B�B�B�B�B�B�B�B�B�#B�B�B�B�B�#B�#B�B�B�B�#B�B�#B�B�
B�/B�#B�#B�B�B�)B�#B�#B�#B�B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�)B�B�#B�#B�B�B�#B�#B�#B�#B�B�B�B�B�#B�B�B�#B�B�#B�#B�
B�#B�)B�5B�B�)B�B�B�)B�)B�B�B�B�/B�)B�B�B��B�)B�)B�#B�#B�)B�#B�#B�#B�)B�#B�)B�)B�B�)B�)B�)B�#B�)B�#B�)B�)B�)B�#B�#B�)B�#B�)B�#B�B�B�/B�;B�#B�B�B�B�5B��B�B�B�B�B�B�B�B�B�
B�)B�B�
B�B�B�
B�B�B�B�B�B�
B�
B�B�B�;B��B��B�B��B��B��B��B�
B�B��B�B��B�
B�B��B��BɺB��B��B��B��B�B�B�
B��B��BB�
B�B��B��B��B��B��B��B�B��B��BĜB��B�wB��BɺB��BɺB�}BĜBB�;B�
B�XB��BɺB�XBĜB��B��BÖB��B�dB�}B�LB�wB��BǮB�BɺB�3BĜB�!B�}B�dB�}B�-B�B�dB�B�3B��B�B��B�-B�B��B�B��B��B��B�}B�9B��B��B��B�B�PB�%B�RB�TBv�B|�B|�Bz�B~�Br�Bw�Bu�Bt�Bt�Bt�B�Br�B�B�?BM�BP�BB�BVBp�BL�BF�BK�BT�BE�BC�BC�BL�BC�B6FB:^BC�BbNBI�B#�B:^B0!B+B�B�B�B.BPB�B\BoB�B0!B1BB
�B
��B
�B
�B
�`B
�B
�B
�mB
��B
��B
�B
��B
�HB
�B
��B
ɺB
ǮB
��B
��B
ƨB
��B
ĜB
��B
��B
ÖB
ȴB
ŢB
�^B
ŢB
�qB
B
ŢB
�jB
�}B
��B
�}B
��B
��B
�dB
�qB
��B
�dB
�dB
�jB
�XB
�RB
�^B
�XB
�^B
�dB
�XB
�XB
�^B
�jB
�XB
�XB
�XB
�XB
�dB
�XB
�LB
�LB
�^B
�RB
�XB
�qB
�LB
�FB
�9B
�RB
�FB
�LB
�LB
�9B
�^B
�qB
�XB
�LB
�dB
�^B
�^B
�wB
�LB
�?B
�XB
�RB
�LB
�^B
�3B
�FB
�XB
�LB
�RB
�?B
�RB
�?B
�?B
�FB
�9B
�LB
�?B
�3B
�-B
�RB
�FB
�wB
�B
�'B
�B
�!B
��B
��B
��B
�B
�B
��B
�!B
ĜB
�B
��B
��B
��B
�{B
��B
��B
��B
�DB
�B
�uB
�VB
�\B
�hB
�\B
�uB
�oB
�\B
�7B
�\B
�VB
�JB
�VB
�oB
�=B
�JB
��B
��B
�1B
��B
w�B
r�B
�B
�B
l�B
s�B
p�B
XB
e`B
W
B
bNB
`BB
ZB
W
B
N�B
\)B
bNB
_;B
k�B
bNB
\)B
T�B
\)B
[#B
YB
\)B
dZB
O�B
`BB
S�B
N�B
]/B
XB
T�B
O�B
R�B
\)B
O�B
O�B
Q�B
XB
O�B
hsB
\)B
k�B
]/B
t�B
[#B
q�B
jB
hsB
n�B
ffB
�1B
n�B
`BB
p�B
s�B
l�B
YB
l�B
x�B
u�B
t�B
x�B
gmB
YB
�DB
��B
�B
�B
�JB
�7B
�bB
�B
��B
�bB
�\B
�oB
�oB
�VB
�=B
�\B
��B
�PB
�VB
�VB
�hB
�JB
�JB
�\B
�bB
��B
�\B
�{B
�{B
�PB
��B
��B
��B
��B
�%B
��B
�bB
��B
�B
�oB
�hB
�VB
�{B
��B
�oB
�1B
��B
�uB
��B
��B
�PB
�oB
��B
��B
�hB
��B
�oB
�%B
�VB
��B
��B
�=B
��B
r�B
��B
��B
�^B
�'B
�!B
�B
��B
�-B
�B
�B
�LB
�B
�-B
�-B
�9B
�B
�-B
�B
�XB
�B
�-B
��B
�qB
�B
ŢB
�RB
�9B
�wB
B
�?B
�^B
�dB
�^B
��B
�^B
�)B
�jB
�B
�'B
�!B
�-B
��B
�jB
�NB
��B
��B
�mB
�B
��B
�XB
��B
�/B
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
��B
��B
ɺB
�5B
��B
�
B
��B
��B
ɺB
��B
��B
�#B
�#B
�#B
�)B
�NB
�NB
ƨB
�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   B��B�&B�HB�B�2B�8B��B��B�B��B� B� B�B�B��B�EBۀB�RBتB՜B�`B�B��B�lB��B��B�B_�B=�BB
��B
�&B
�NB
��B
��B
��B
��B
��B
��B
�xB
a�B
YLB
W�B
i�B
�pB
��B
�wB
��B
��B
�B
�,B
��B
ͰB
��B
ڸB
�AB
�TB
�B �BB �B1*BO#Bp(B�)B�B�B�AB̦B��B��B�<BѰBӕB�TB�XBZ�B?�B#B0�B,.B!�BgB*gB;:B>�BDgB@�B<PB9�B5jB0OB(<B
6B
��B
�hB
�B
�B
̅B
��B
ƬB
��B
�B
��B
�B
��B
��B
��B
�$B
��B
�IB
�fB
��B
�dB
�B
�B
�7B
�XB
��B
� B
��B
��B
��B
��B
�B
��B
�}B
��B
�ZB
�eB
�B
|�B
w�B
r�B
u&B
m�B
QB
J�B
J1B
K�B
M�B
O\B
P,B
RB
R]B
T�B
UzB
S�B
]TB
^B
\�B
^.B
J%B
J[B
LB
M�B
OB
S�B
W�B
c	B
fwB
i�B
n�B
y>B
s�B
[JB
\B
\�B
Z�B
^DB
h�B
c�B
cB
f{B
e�B
i
B
r�B
zcB
|>B
~RB
t�B
r�B
qJB
eAB
aaB
Z�B
VxB
U�B
VgB
T�B
M�B
I�B
CaB
@B
?�B
?$B
9B
3'B
/mB
*�B
(�B
"eB
�B
�B
fB
2B
�B
B
B
oB
�B
`B
iB
RB
�B	�NB	��B	�$B	�hB	�B	��B	��B	�B	�ZB	��B	��B	��B	�xB	�VB	��B	�"B	�GB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�\B	��B	�4B	�dB	� B	�*B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	~�B	|�B	z�B	y+B	wKB	tKB	r�B	c�B	_�B	]�B	Z�B	W�B	T<B	M�B	J�B	FjB	D7B	?(B	8�B	7pB	6WB	5�B	6�B	5IB	2B	/�B	/}B	.�B	.�B	,�B	*wB	)�B	(&B	'NB	'"B	%�B	$�B	%aB	%%B	#3B	#ZB	!fB	!SB	�B	QB	�B	B	�B	B	xB	B	�B	�B	EB	!B	MB	9B	�B	B	�B	�B	�B	�B	~B	�B	�B	�B	�B	�B	B	aB	hB		FB	�B	pB	 �B�iB�B�WB�B��B��B�B��B��B�pB�JB��B�(B�B��B�B��B�B�B�?B�8B�mB�LB�1B��B��B��B��B	�B	�B	@B	�B	AB	,B	�B	�B	xB��B�B�B	B��B��B��B�"B��B��B	 IB	 B��B	B	rB		}B	�B	�B	�B	oB	�B	&B	&/B	3�B	:UB	:�B	;�B	=rB	?�B	@�B	@xB	@lB	B�B	D�B	HB	JB	J�B	J�B	K�B	K�B	L�B	L�B	MB	NB	NB	NB	QB	XB	Y>B	ZSB	[~B	[�B	\FB	]DB	]�B	`B	a�B	a�B	b�B	b�B	cB	d8B	eZB	e�B	e�B	f�B	hpB	h�B	h�B	h�B	h{B	h�B	k�B	n�B	n�B	o�B	p�B	p�B	r�B	s�B	t�B	u�B	wB	xZB	y1B	y8B	{"B	|'B	|�B	}&B	GB	�AB	�B	�$B	�,B	�CB	��B	�"B	��B	�3B	�5B	�5B	�EB	�kB	�:B	�0B	�rB	��B	�\B	�HB	��B	��B	�<B	��B	�B	�jB	�fB	�*B	��B	�(B	��B	��B	��B	�B	�B	�0B	�DB	�JB	�JB	��B	�(B	��B	�B	�%B	�B	�B	�B	�B	�FB	�)B	�MB	ռB	ٔB	�oB	�NB	�GB	�SB	�pB	�_B	߇B	�jB	�lB	�B	�B	��B	�B	�tB	�B	��B	�B	�B	�B	�B	��B	��B	�B	�%B	��B	� B	��B	�B	�B	�rB	�B	�oB	�B	�B	�lB	��B	�B	�XB	��B	��B	�B	�,B	��B	�B	�fB	��B	��B	��B	�B	�hB	��B	��B	�vB	�B	��B	��B	�B	��B	�B	��B	�/B	�FB	ڹB	פB	�B	�iB	�B	�&B	ԸB	��B	�B	�RB	�JB	�bB	�nB	�B	�B	��B	��B	�B	�7B	�?B	�<B	�]B	�YB	�SB	�@B	�B	��B	�B	�|B	�B	�B	�B	�B	�}B	��B	��B	�#B	��B	� B	��B	��B	�&B	�#B	�B	�B	�
B
B
B
B
4B
B
	"B

B
&B
^B
CB
B
B
�B
iB
�B
 B
"B
!4B
"�B
'�B
+UB
+B
*�B
.�B
1�B
6�B
<B
>:B
@zB
@zB
A�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H$B
IB
H�B
I�B
J�B
J�B
K�B
NB
N�B
O�B
QB
UB
VB
X!B
Z4B
[!B
[?B
\`B
_4B
_LB
_�B
czB
duB
emB
enB
e�B
fxB
g�B
h�B
hxB
h�B
i�B
j�B
j�B
l�B
n�B
n�B
nB
nRB
n�B
n�B
o�B
pB
pB
qEB
p�B
p�B
q�B
q�B
rB
sB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
u
B
vB
v�B
v�B
w�B
x�B
zB
z�B
}1B
�UB
�NB
�DB
�`B
�JB
�>B
�MB
�ZB
�EB
�kB
�SB
�GB
�IB
�LB
�eB
�yB
��B
�lB
��B
��B
�kB
�pB
�~B
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
��B
��B
�B
�)B
��B
��B
�B
�B
�[B
�B
�B
�B
�
B
��B
��B
�B
�B
�B
�>B
�TB
�LB
�vB
�`B
�B
�TB
�kB
�|B
B
B
B
B
¡B
ęB
ĝB
ŝB
ŔB
ŤB
ƩB
ǰB
ʿB
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�*B
�;B
�2B
�3B
�+B
�8B
�:B
�HB
�DB
�RB
�<B
�KB
�BB
�^B
�pB
�^B
�aB
�XB
�VB
�dB
�ZB
�fB
�B
�_B
�dB
�sB
�uB
�B
�B
�B
�B
�B
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
�B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�B
��B
�B
��B
�
B
�
B
��B B +B B2BBB&B6B5BBGB@B7B7B>BLB	7B
8B
JB@BUBdBdB\BxBoBZBdBjBkBnBcBnBaB`BaBeBsBhBjBtB�B}B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B�B�B�B�#B�B�#B�#B�B�B�B�#B�B�B�)B�B�B�#B�#B�B�B�#B�)B�B�#B�B�#B�#B�B�B�B�#B�#B�B�B�#B�#B�#B�B�B�B�/B�B�B�#B�#B�B�B�#B�#B�B�B�B�B�B�B�B�B�B�#B�B�B�B�B�#B�#B�B�B�B�#B�B�#B�B�
B�/B�#B�#B�B�B�)B�#B�#B�#B�B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�)B�B�#B�#B�B�B�#B�#B�#B�#B�B�B�B�B�#B�B�B�#B�B�#B�#B�
B�#B�)B�5B�B�)B�B�B�)B�)B�B�B�B�/B�)B�B�B��B�)B�)B�#B�#B�)B�#B�#B�#B�)B�#B�)B�)B�B�)B�)B�)B�#B�)B�#B�)B�)B�)B�#B�#B�)B�#B�)B�#B�B�B�/B�;B�#B�B�B�B�5B��B�B�B�B�B�B�B�B�B�
B�)B�B�
B�B�B�
B�B�B�B�B�B�
B�
B�B�B�;B��B��B�B��B��B��B��B�
B�B��B�B��B�
B�B��B��BɺB��B��B��B��B�B�B�
B��B��BB�
B�B��B��B��B��B��B��B�B��B��BĜB��B�wB��BɺB��BɺB�}BĜBB�;B�
B�XB��BɺB�XBĜB��B��BÖB��B�dB�}B�LB�wB��BǮB�BɺB�3BĜB�!B�}B�dB�}B�-B�B�dB�B�3B��B�B��B�-B�B��B�B��B��B��B�}B�9B��B��B��B�B�PB�%B�RB�TBv�B|�B|�Bz�B~�Br�Bw�Bu�Bt�Bt�Bt�B�Br�B�B�?BM�BP�BB�BVBp�BL�BF�BK�BT�BE�BC�BC�BL�BC�B6FB:^BC�BbNBI�B#�B:^B0!B+B�B�B�B.BPB�B\BoB�B0!B1BB
�B
��B
�B
�B
�`B
�B
�B
�mB
��B
��B
�B
��B
�HB
�B
��B
ɺB
ǮB
��B
��B
ƨB
��B
ĜB
��B
��B
ÖB
ȴB
ŢB
�^B
ŢB
�qB
B
ŢB
�jB
�}B
��B
�}B
��B
��B
�dB
�qB
��B
�dB
�dB
�jB
�XB
�RB
�^B
�XB
�^B
�dB
�XB
�XB
�^B
�jB
�XB
�XB
�XB
�XB
�dB
�XB
�LB
�LB
�^B
�RB
�XB
�qB
�LB
�FB
�9B
�RB
�FB
�LB
�LB
�9B
�^B
�qB
�XB
�LB
�dB
�^B
�^B
�wB
�LB
�?B
�XB
�RB
�LB
�^B
�3B
�FB
�XB
�LB
�RB
�?B
�RB
�?B
�?B
�FB
�9B
�LB
�?B
�3B
�-B
�RB
�FB
�wB
�B
�'B
�B
�!B
��B
��B
��B
�B
�B
��B
�!B
ĜB
�B
��B
��B
��B
�{B
��B
��B
��B
�DB
�B
�uB
�VB
�\B
�hB
�\B
�uB
�oB
�\B
�7B
�\B
�VB
�JB
�VB
�oB
�=B
�JB
��B
��B
�1B
��B
w�B
r�B
�B
�B
l�B
s�B
p�B
XB
e`B
W
B
bNB
`BB
ZB
W
B
N�B
\)B
bNB
_;B
k�B
bNB
\)B
T�B
\)B
[#B
YB
\)B
dZB
O�B
`BB
S�B
N�B
]/B
XB
T�B
O�B
R�B
\)B
O�B
O�B
Q�B
XB
O�B
hsB
\)B
k�B
]/B
t�B
[#B
q�B
jB
hsB
n�B
ffB
�1B
n�B
`BB
p�B
s�B
l�B
YB
l�B
x�B
u�B
t�B
x�B
gmB
YB
�DB
��B
�B
�B
�JB
�7B
�bB
�B
��B
�bB
�\B
�oB
�oB
�VB
�=B
�\B
��B
�PB
�VB
�VB
�hB
�JB
�JB
�\B
�bB
��B
�\B
�{B
�{B
�PB
��B
��B
��B
��B
�%B
��B
�bB
��B
�B
�oB
�hB
�VB
�{B
��B
�oB
�1B
��B
�uB
��B
��B
�PB
�oB
��B
��B
�hB
��B
�oB
�%B
�VB
��B
��B
�=B
��B
r�B
��B
��B
�^B
�'B
�!B
�B
��B
�-B
�B
�B
�LB
�B
�-B
�-B
�9B
�B
�-B
�B
�XB
�B
�-B
��B
�qB
�B
ŢB
�RB
�9B
�wB
B
�?B
�^B
�dB
�^B
��B
�^B
�)B
�jB
�B
�'B
�!B
�-B
��B
�jB
�NB
��B
��B
�mB
�B
��B
�XB
��B
�/B
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
��B
��B
ɺB
�5B
��B
�
B
��B
��B
ɺB
��B
��B
�#B
�#B
�#B
�)B
�NB
�NB
ƨB
�B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                   <ebZ<e`o<edJ<ea�<ea8<ea�<eb�<ec<e`�<ebZ<e`G<e`G<e`B<ea8<eg�<ea�<es<eg�<e��<e��<fz<f-X<g��<j�<p�<�H<�̈<��<��<���<m$<f��<e�w<ej�<eh�<ey5<hC<m~h<jƑ<�Y�<g�<e�f<i'�<m�^<f�<e�f<e`D<g)�<iVU<f�<fZ<i��<f 9<e��<h��<g��<e��<e�:<f�<h�<fK�<e`G<el1<f� <g-�<f�<ef�<e}<ec�<e`f<e`�<es<rU�<��<���<���<�^<u*�<ej�<iYE<h�<f !<hJ�<f�<ed<fWL<i�#<fP	<eي<fK�<h�<o^6<�M�<sga<zmE<s�<ek?<e��<g�<gʘ<g��<f��<e��<e�	<g^�<hޱ<f�<g��<h��<fx�<e�c<ea�<e�<eh<epa<ex�<e�<e�H<f<fm�<ep�<es�<es�<e�<e��<e��<e�-<f�/<e�K<g�1<m�<e��<fO<edz<l�<sؙ<f=�<ec�<e<e�<e��<e��<em.<ea�<e|<e��<f�&<ed<f"�<e�O<l^<yb�<f�Q<e��<eo�<eb�<eh�<ef'<e|�<e�o<e`�<ef'<fGw<|6�<o^6<ec�<e`�<erN<e��<e`�<h9<eeH<ef�<ea8<e~�<e{<ea<e��<h�x<h��<f<g��<l��<go�<j�W<e�H<e}�<eܤ<h�M<l��<jƑ<gA�<e�O<e�H<foB<i|�<g�<g��<h}�<gv<m8x<iD�<ge<fd�<e޺<e�}<fX�<g7�<j�<fU�<e��<eq�<eu�<fP	<g �<fm�<f�<f��<g�<jb<uL{<o��<f�<f��<e��<f(<h'�<mښ<eي<U�<e�}<U��<eq#<fj�<V�V<ZQ�<e��<T�0<T�{<U�<YϏ<`�Q<Y��<Yf�<Wu<UK�<U��<U;�<U�<UZ<U�<U�<U$g<U2�<U�R<UO�<Ulb<U�!<U�!<U�O<V�<Ve<\��<c�o<V�K<Vl�<Va�<V�^<Y��<X.�<WV<W,n<V�<YC�<V��<U�<U �<T��<U�<U��<W�M<U��<U"	<U~<U-�<U:<UhZ<U!u<U,�<U�<U�<U�<T��<T�l<U&�<U
<U~<U)�<U:�<U-E<T�
<U4�<U�M<U P<U<<U�<Uk^<U)[<T�
<U�2<U=�<U-E<UTl<V�W<V:�<U2<T��<U�<U�<T�Z<T��<U�<T��<U�<U�<U�<U:<Wm�<W�m<U��<Wu<V&�<Vp�<\[�<X?<V�]<VLY<U7<V�L<U��<U<T��<V�<VN<Z�<V^8<V�'<U<<U.<U�<U �<T�%<U�<U_<U<Ul<U'v<U �<U�<U}<U�<U<U�<Z_�<Vi)<U <U [<UO<V�e<\�<Ui[<U�<U�<U�R<Ubs<T��<UD�<U�<T�K<U [<U
v<T��<UV<U�<US�<UTl<U�<U�<U2<U�M<V�<V��<V�-<U9E<T�%<UK<U(<T��<T�2<U �<T�=<T�2<T�{<U}<U	<U(<U+<T��<T��<T�%<U �<T��<U.<U�<U�<U
�<UM<T�
<U�<U�<Ud<U�<T��<T��<U9E<UhZ<U�<UT<U�<U�<UW%<Uw�<U��<U�<U	t<U�<T��<T�Z<U �<T��<T�<Ud<U�<T��<U<U�<UX<U<U(<T��<UX<U�<U <U+J<U�<U�<U�<U�<T��<UP<U�<U�<T�j<T�N<T��<U�<UD+<U�<UA<T�j<T��<T��<U [<U�<T�{<T�<U	�<U/I<U+<T��<U�<UT<U<UJ�<U$g<U.<U�<U6S<U��<Ua{<U�<U�<U�<U]<U�<U�<T��<T��<U [<T��<U4�<U�<U�<U�<U<<UM<UX<U�<U�<U�<U�<UR�<U"�<U�<U+<U <U �<U�<U<U�<U�<U<U�<UZ<U�<U�<T�j<UP<U P<U)�<U�<U+<T��<U	!<U�<U0<U,�<UM<U~<U�<U�<U�<U[�<U�<Udf<T��<T�j<U�<T��<U}<U)�<WCb<U�R<UV<U%�<U��<U��<Ut�<U�<UZ<U(<U>�<U��<U,�<U"�<Uӣ<Ui[<U�<U@�<VV<Vl�<V&�<U>�<U��<U�b<V��<U5�<U�R<U�<U <U�<UTl<T�
<U<U�<U�<US<U�<T�j<T��<U+<U!<U<T��<T�{<T�N<T��<T�Z<T��<U�<T�<T��<T��<T��<T��<T��<T��<T�l<Uf^<U�<UK<U�<U �<U+<T��<T�j<U	!<U�<U�<T��<T��<T��<T�K<T��<T��<T�N<T��<UK<U <T�<U 5<U�<U#<em�<U <e�C<U:<U2�<U3t<Uv�<ejV<eoF<ea�<fG<e�<fg�<e�<ep<eh^<e`o<e`o<ej�<ee~<ea�<e`B<ec<e`D<ec�<er�<ea!<ef�<e~�<eu�<ec�<ea�<e`�<ed<ed<efa<e`�<ea�<egW<eaP<ea�<e`�<eai<e`D<ea�<ef�<e`]<e`�<eq#<ech<ea�<e`�<e`�<ec�<e`�<eiw<ec<e`P<ef�<ef�<ec�<eu�<f�<f%l<e�n<e}<e�f<eq�<eg�<ef�<exI<e��<e��<ejV<eb8<ee�<em�<e~<ev<ea<ea�<ec�<ech<ea�<eb<e`�<e`�<e`�<eg<ed<em�<eh�<e`�<ec�<eb<eb<ech<ea�<ej<em�<efa<ea�<ec�<e`�<e`B<e`�<eb<e`B<ec�<e`�<e`G<e`B<e`D<ea�<ec�<ee�<e`�<ech<ea�<e`K<e`B<e`�<e`�<ec<eb<ec�<e`�<ef�<ech<e`�<ej�<e`G<ec><eb�<e`�<e`P<ef'<ei/<e`V<e`B<e`z<ea!<eoF<e`�<e`z<e`�<e`z<edJ<eu0<ea�<ec�<ec><e`B<e`�<e`B<ed<e`f<eoF<eaP<ea�<e`�<e`K<e`B<e`B<e`B<e`�<e`G<e`B<e`P<e`�<e`D<e`B<e`D<e`D<e`f<ea�<ea�<e`�<e`�<e`�<e`�<e`�<ec><e`B<e`�<e`D<e`�<eaP<e`�<e`B<ea�<e`�<e`�<e`f<e`D<e`�<e`D<e`D<e`B<e`�<e`B<e`�<e`z<e`K<e`�<eb8<e`�<e`z<e`D<e`�<e`�<ea�<e`�<eai<e`B<e`o<e`B<ea�<ec�<e`�<e`�<e`K<e`D<e`�<e`B<e`�<ech<e`B<e`D<e`�<e`�<ec�<ec�<eaP<e`�<e`�<e`B<e`B<e`�<e`K<e`�<ea�<ea�<e`f<e`D<e`o<e`�<e`�<e`�<e`�<eai<ed�<eai<ebZ<ea�<e`D<e`�<e`B<e`�<e`B<e`�<e`�<ec�<e`�<ea�<e`D<e`B<e`�<e`B<e`B<e`B<e`�<ea<eeH<e`z<e`G<e`z<e`z<e`�<eai<e`f<e`�<e`�<e`z<e`B<e`�<e`D<e`�<e`�<e`D<ea�<edJ<ea�<edz<eeH<e`�<eb<eaP<e`o<ec�<ea�<e`�<e`�<e`�<ea�<e`B<e`P<e`�<e`K<e`�<ea�<ea�<e`�<ec�<ea�<e`K<e`f<e`�<e`�<e`�<e`B<e`�<e`B<e`D<e`B<e`G<e`�<e`B<e`D<e`�<ea�<e`�<ee�<e`�<e`�<e`B<e`K<e`�<e`D<e`�<e`o<e`�<e`�<e`�<e`�<e`�<e`�<eb8<ec�<eaP<e`�<e`D<e`B<e`�<e`G<e`�<edJ<ee�<e`B<e`�<e`D<e`�<ec�<e`�<e`�<e`�<ea�<e`B<ea�<e`�<e`D<e`�<e`K<ea�<e`�<e`�<e`�<e`G<e`G<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`B<e`BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = CTM_ADJ_PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                              PRES_ADJUSTED = PRES                                                                                                                                                                                                                                            TEMP_ADJUSTED = TEMP                                                                                                                                                                                                                                            PSAL_ADJ = PSAL, multiplicative adjustment term r = 1, no additional adjustment necessary.                                                                                                                                                                      None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            CTM: alpha=0.141C, tau=6.89s, rise rate = 10 cm/s with error equal to the adjustment;OW: r =1(+/-0.0004), vertically averaged dS =0.009(+/-0.014),                                                                                                              None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW: r =1(+/-0.0004), vertically averaged dS =0.009(+/-0.014),                                                                                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                PSAL_ADJ corrects Conductivity Thermal Mass (CTM), Johnson et al., 2007, JAOT.; No significant drift detected in conductivity                                                                                                                                   SOLO-W floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface.  Additional correction was unnecessary in DMQC;      PRES_ADJ_ERR: SBE sensor accuracy + resolution error                                                   No significant temperature drift detected;  TEMP_ADJ_ERR: SBE sensor accuracy + resolution error                                                                                                                                                                No thermal mass adjustment on non-primary profiles.; No significant drift detected in conductivity                                                                                                                                                              202207120000002022071200000020220712000000202207120000002022071200000020220712000000AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030201060820210302010608QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030201060820210302010608QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               WHOIWHOIARSQARSQWHQCWHQCV0.5V0.5                                                                                                                                2021032900000020210329000000QC  QC                                  G�O�G�O�G�O�G�O�G�O�G�O�                                    WHOI    ARSQ    WHQC    V0.5                                                                                                                                              20211025000000    CF                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARSQARSQCTM CTM V1.0V1.0                                                                                                                                2022071100000020220711000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                WHOIWHOIARCAARCAOWC OWC V2.0V2.0ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     ARGO_for_DMQC_2021V03; CTD_for_DMQC_2021V02                     2022071200000020220712000000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                