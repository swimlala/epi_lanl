CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-11-10T06:41:44Z creation; 2023-04-26T19:14:28Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20191110064144  20230426191428  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               C   CAA  AOAO7316_008644_067                 7316_008644_067                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @���G�#@���G�#11  @���w1��@���w1��@(��n�@(��n��c�ߏG0@�c�ߏG0@11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@E�@�G�@�p�@�  @�  @��RA  A\)A+�A@  A`��A�Q�A���A�Q�A��A��AϮA�Q�A�Q�B (�B�
B�
B(�B   B'�
B0  B8  B@  BG�
BO�BW�
B_�
Bg�Bo�
Bx(�B�{B��B��B��B�  B��
B��B�(�B��\B��B�  B�  B�  B��B��
B�  B�  B�  B�{B�{B�{B�{B�{B��B�  B��B�  B��B�  B�{B��B��C   C��C�C  C
=C

=C  C  C
=C  C
=C
=C
=C{C
=C
=C   C!��C$
=C&{C(
=C*
=C,{C.  C/��C1��C4  C6  C8
=C:  C<  C>
=C@
=CB
=CD{CF
=CH
=CJ{CL
=CN
=CP  CQ��CT
=CV
=CX
=CZ
=C[��C]��C`  Cb  Cc��Cf  Ch
=Cj{Cl{Cn{Cp  Cr  Ct
=Cv
=Cx  Cy��C{�C}��C�  C�C�  C���C���C�  C�  C�C�C�C�  C���C�C�  C���C�C�C�C�
=C���C���C�C�
=C�  C���C�  C�C�  C���C���C���C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C���C���C�  C�C���C�C�C���C���C���C�  C�C�
=C�C���C�  C�C�C�  C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C�C�  C���C���C���C�C�C�C�C�  C�C�C�  C�  C�  C���C���C���C���C���C�  C�  C�  C�C���C���C���C���C�  C�C�  C�C�
=C�  C���C���C���C���C���C�C�
=C�  C���C���C���C�  C�  C�  C�  C�  C�C�  C���D � D�D}qD  D��D�qD� DD��D  D}qD  D�D�D��D  D}qD�qD	}qD	�qD
}qD
�qD}qD�qDz�D  D��D  D}qD�D� D�qDz�D��Dz�D  D�DD�D�D� D  D}qD��Dz�D  D��D  D}qD�qD}qD��Dz�D  D�D�D� D  D}qD�qDz�D  D��D �D � D �qD!��D"  D"}qD#�D#� D#�qD$� D%D%��D&  D&z�D'  D'�D(�D(� D(��D)}qD*�D*}qD*�qD+}qD,�D,��D-  D-}qD.�D.}qD/  D/� D/�qD0z�D1  D1�D2�D2��D3  D3}qD4�D4� D4�qD5��D6  D6��D7D7��D8D8� D9  D9}qD:  D:�D;D;}qD<  D<� D<�qD=}qD>�D>�D?�D?}qD?�qD@� D@�qDA}qDB  DB� DC  DC��DD  DD}qDD�qDE}qDE�qDF}qDF�qDG}qDH  DH� DI  DI}qDI�qDJ}qDJ�qDK� DL  DL}qDM  DM� DN  DN� DO  DO� DO�qDP� DQ�DQ��DR  DR� DSDS�DTDT��DU  DU��DV�DV}qDW  DW��DX�DX�DY�DY� DZ�DZ��D[�D[� D[�qD\z�D\�qD]� D]�qD^z�D^�qD_� D`  D`� Da  Da� Db  Db��Dc  Dc� Dc�qDdz�De  De}qDf  Df� Df��Dg}qDg�qDh}qDh�qDi��Dj�Dj� Dk�Dk��Dl  Dl}qDl�qDm��Dn  Dnz�Dn��Do� Dp�Dp��Dq  Dq� Dr�Dr��Ds�Ds� Dt  Dt}qDu  Du��Dv�Dv}qDw  Dw��Dw�qDx� Dy  Dy}qDz  Dz� Dz�qD{}qD{�qD|}qD|��D}}qD~  D~� D~�qD}qD��D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD�  D�AHD��HD��HD���D�=qD�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D�HD�@ D�� D��HD�  D�@ D�~�D���D�HD�AHD�� D�� D�  D�@ D�� D���D�  D�B�D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD�~�D��HD�  D�>�D��HD��HD���D�>�D�~�D�� D��D�B�D�~�D�� D�  D�@ D�� D���D��D�AHD�~�D���D�  D�@ D��HD�� D�HD�AHD���D��HD�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�=qD�~�D���D���D�AHD���D��HD�HD�AHD���D�� D���D�@ D�� D���D�  D�@ D�� D��HD���D�@ D�� D��HD�  D�>�D��HD��HD�  D�@ D�~�D��HD�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�>�D�� D�� D��)D�@ D���D��HD�HD�AHD��HD�� D�HD�AHD�~�D��qD�  D�@ D�}qD��qD��qD�=qD�}qD���D�  D�>�D�� D���D�HD�@ D�� D�� D�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�HD�B�D��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�AHD�~�D��HD�HD�AHD�~�D��)D��qD�>�D�� D���D���D�@ D�~�D���D�HD�B�D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D���D�>�D�� D���D���D�@ DHD�� D���D�=qD�~�Dþ�D���D�@ DāHD�� D���D�>�Dŀ D�� D�  D�@ D�~�Dƾ�D���D�@ Dǀ DǾ�D���D�>�D�~�DȾ�D�  D�@ D�~�D�� D���D�>�Dʀ D�� D���D�>�Dˀ D�� D�  D�@ D�~�D̾�D�  D�@ D�~�D�� D�HD�AHD�~�DνqD�  D�AHDρHD�� D�  D�@ DЀ D��HD�HD�>�D�~�DѾ�D���D�@ DҀ DҾ�D��qD�@ DӀ D�� D�HD�@ D�~�D�� D�HD�AHDՁHD�� D�  D�@ Dր D��HD�HD�AHDׁHD��HD�  D�>�D�~�D��HD�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�~�D�� D��D�@ Dހ D޾�D�  D�>�D�}qD߾�D�  D�>�D�� D�� D���D�>�D� D�� D��qD�>�D� D�� D�  D�>�D� D�� D�HD�B�D�HD�� D�  D�AHD� D�� D���D�AHD悏D��HD�HD�AHD�~�D�� D�HD�AHD� D��HD�  D�@ D�~�D�qD�  D�AHD� D�� D���D�>�D� D��HD�  D�>�D� D��HD�  D�AHD킏D��HD�  D�>�D� D��HD�  D�>�D� D��HD�  D�>�D�� D�� D���D�>�D� D��HD��D�AHD� D�D��qD�>�D�HD�� D���D�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�~�D��qD���D�@ D�� D�� D�  D�@ D�~�D��HD�HD�@ D���D�� D���D�E>�G�?B�\?�=q?�Q�?��@�@�R@5@O\)@^�R@u@��@�33@�p�@���@�
=@��@�\)@��H@�@���A�\A��A��A�Ap�A$z�A,(�A2�\A8Q�A@  AG
=AMp�AS�
A[�Ab�\Ai��Ap  Aw
=A}p�A��A�p�A�G�A�(�A�
=A���A��A��A��A���A��A���A��A�A�Q�A��A��
A�{A�  A�G�A��A�A��A�G�A�33A�p�A�\)A���A��HA��AƸRA�Q�A��HA���A�ffAϮA�=qA�(�A�A׮A��AۅA��A�
=A�G�A�33A���A�
=A���A�\A�z�A�ffA��A�=qA��
A�ffA�  A�G�A��A�A�\)B z�BG�BffB\)BQ�B��B�B
=B�
B��B	B
�RB�BQ�Bp�BffB33B  B��B{B
=B�B��B��B�RB�BQ�B�B=qB33B  B��B�B�HB�B z�B!��B"�\B#33B$Q�B%G�B&=qB&�HB(  B)�B)�B*�\B+�B,��B-B.�\B/\)B0z�B1p�B2ffB3
=B4(�B5G�B6{B6�RB7�B8��B9��B:ffB;\)B<Q�B=G�B=�B>�HB@  B@��BAG�BB{BC33BD  BD��BE��BFffBG\)BH(�BH��BI��BJffBK33BL(�BL��BM��BNffBO�BPQ�BP��BR{BS33BT(�BT��BV{BW33BXz�BYG�BZ�\B[�B\��B]�B_
=B`  Ba�Bb=qBc�Bd��Bf{Bg
=Bh(�Bip�Bj�RBl  Bm�Bn=qBo\)Bp��Bq�Bs33Btz�BuBv�HBx  ByG�Bz�\B|  B}G�B~�\B�  B�z�B�
=B�B�z�B��B�B�z�B��B��B�Q�B���B���B�Q�B�
=B��B�z�B��B��
B�z�B��B�B�z�B�33B�  B���B�p�B�(�B��HB��B�(�B��HB��B�=qB�
=B�B�z�B��B��B�z�B�
=B��B�Q�B���B�p�B��B�z�B��HB�\)B��B�  B�Q�B�z�B���B��HB�
=B�33B�\)B��B��B��
B�  B�(�B�Q�B��\B��RB��HB�
=B�33B�\)B�p�B��B��
B�  B�(�B�ffB���B���B��B�\)B��B��B��
B�{B�=qB�z�B���B��HB�
=B�\)B���B��
B�(�B�ffB���B��HB�33B�p�B��B��B�=qB�z�B���B���B�G�B��B��
B�{B�ffB��RB���B�G�B���B��B�Q�B���B���B�G�B���B��B�(�B�z�B���B��B�p�B�B�{B�ffB��RB��B�\)B��B�{B�ffB��RB�
=B�p�B�B�(�B��\B��HB�G�B��B�{B�ffB���B�33B��B�  B�Q�B���B��B��B��B�ffB��RB��B��B��B�Q�B��RB�
=B�p�B��
B�(�B��\B���B�G�B��B�  B�ffB���B��B�p�B��B�=qBď\B���B�G�BŮB�  B�ffBƸRB��B�p�B��
B�Q�BȸRB�
=B�p�B��
B�(�Bʏ\B��HB�33B˙�B��B�Q�Ḅ�B��BͅB��
B�=qBΣ�B�
=B�p�B�B�(�B�z�B��HB�33Bљ�B��B�Q�Bң�B���B�\)B�B�{B�z�B���B�33BՅB��
B�=qB֏\B���B��B�\)B׮B�  B�Q�B؏\B���B�G�BمB��B�=qBڏ\B���B��B�p�BۮB�  B�=qB܏\B��HB��B݅B��
B�=qBޏ\B���B�\)Bߙ�B��B�=qB�z�B��HB�33BᙚB�  B�Q�B�\B��HB�33B㙚B�  B�ffB��B���B�G�B�B�{B�z�B���B�
=B�p�B��
B�Q�B��B���B�G�B�B�(�B�\B��HB�33B뙚B�  B�ffB��HB�33B�B�  B�ffB���B�G�BB�(�B��B��B�B��B�ffB��HB�p�B��
B�=qB���B�33B��B�(�B��\B�
=B��B�{B��\B���B�p�B�  B��\B�
=B��B��B�ffB���B��B��B�ffB�
=B��C   C 33C z�C C
=CG�C�C��C{C\)C��C�HC(�CffCC
=CQ�C�\C��C{Cp�C�RC��C=qC�\C�HC(�Cp�C�RC  CQ�C��C�C	33C	z�C	��C
�C
p�C
�C
��CG�C��C��C=qC�C��C�Cz�C��C{C\)C��C  CQ�C��C��C=qC�\C�C=qC�C��C(�C�C��C{Cp�CC
=C\)C�RC  CG�C�C��C=qC��C��C=qC�C�HC33Cz�C��C(�Cz�CC{Cp�C�RC  C\)C�C��CG�C��C��C=qC�C�C=qCz�C�HC33C�C��C (�C z�C ��C!�C!z�C!��C"{C"z�C"C#
=C#p�C#C$
=C$\)C$�C%  C%G�C%��C%��C&33C&�\C&�HC'(�C'z�C'��C((�C(ffC(�C){C)ffC)�C*  C*ffC*�C*��C+\)C+�C+��C,G�C,��C-  C-G�C-�\C-�C.G�C.�\C.�
C/=qC/�\C/�HC033C0�\C0�
C1�C1z�C1�
C2(�C2p�C2C3(�C3z�C3�RC4
=C4p�C4��C5
=C5\)C5�RC6
=C6Q�C6��C7  C7\)C7��C7�C8G�C8��C8��C9=qC9�\C9�HC:=qC:��C:�HC;(�C;�C;�HC<33C<z�C<��C=(�C=z�C=��C>{C>p�C>��C?�C?p�C?�RC@�C@z�C@��CA{CAp�CA��CB�CBp�CBCC(�CC�CC�
CD�CDz�CD�
CE33CE�CE�
CF=qCF��CF�HCG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         ?�  @�\@E�@�G�@�p�@�  @�  @��RA  A\)A+�A@  A`��A�Q�A���A�Q�A��A��AϮA�Q�A�Q�B (�B�
B�
B(�B   B'�
B0  B8  B@  BG�
BO�BW�
B_�
Bg�Bo�
Bx(�B�{B��B��B��B�  B��
B��B�(�B��\B��B�  B�  B�  B��B��
B�  B�  B�  B�{B�{B�{B�{B�{B��B�  B��B�  B��B�  B�{B��B��C   C��C�C  C
=C

=C  C  C
=C  C
=C
=C
=C{C
=C
=C   C!��C$
=C&{C(
=C*
=C,{C.  C/��C1��C4  C6  C8
=C:  C<  C>
=C@
=CB
=CD{CF
=CH
=CJ{CL
=CN
=CP  CQ��CT
=CV
=CX
=CZ
=C[��C]��C`  Cb  Cc��Cf  Ch
=Cj{Cl{Cn{Cp  Cr  Ct
=Cv
=Cx  Cy��C{�C}��C�  C�C�  C���C���C�  C�  C�C�C�C�  C���C�C�  C���C�C�C�C�
=C���C���C�C�
=C�  C���C�  C�C�  C���C���C���C�C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C���C���C�  C�C���C�C�C���C���C���C�  C�C�
=C�C���C�  C�C�C�  C���C�C�  C���C�  C�  C���C���C���C�  C�  C�C�C�  C�C�  C���C���C���C�C�C�C�C�  C�C�C�  C�  C�  C���C���C���C���C���C�  C�  C�  C�C���C���C���C���C�  C�C�  C�C�
=C�  C���C���C���C���C���C�C�
=C�  C���C���C���C�  C�  C�  C�  C�  C�C�  C���D � D�D}qD  D��D�qD� DD��D  D}qD  D�D�D��D  D}qD�qD	}qD	�qD
}qD
�qD}qD�qDz�D  D��D  D}qD�D� D�qDz�D��Dz�D  D�DD�D�D� D  D}qD��Dz�D  D��D  D}qD�qD}qD��Dz�D  D�D�D� D  D}qD�qDz�D  D��D �D � D �qD!��D"  D"}qD#�D#� D#�qD$� D%D%��D&  D&z�D'  D'�D(�D(� D(��D)}qD*�D*}qD*�qD+}qD,�D,��D-  D-}qD.�D.}qD/  D/� D/�qD0z�D1  D1�D2�D2��D3  D3}qD4�D4� D4�qD5��D6  D6��D7D7��D8D8� D9  D9}qD:  D:�D;D;}qD<  D<� D<�qD=}qD>�D>�D?�D?}qD?�qD@� D@�qDA}qDB  DB� DC  DC��DD  DD}qDD�qDE}qDE�qDF}qDF�qDG}qDH  DH� DI  DI}qDI�qDJ}qDJ�qDK� DL  DL}qDM  DM� DN  DN� DO  DO� DO�qDP� DQ�DQ��DR  DR� DSDS�DTDT��DU  DU��DV�DV}qDW  DW��DX�DX�DY�DY� DZ�DZ��D[�D[� D[�qD\z�D\�qD]� D]�qD^z�D^�qD_� D`  D`� Da  Da� Db  Db��Dc  Dc� Dc�qDdz�De  De}qDf  Df� Df��Dg}qDg�qDh}qDh�qDi��Dj�Dj� Dk�Dk��Dl  Dl}qDl�qDm��Dn  Dnz�Dn��Do� Dp�Dp��Dq  Dq� Dr�Dr��Ds�Ds� Dt  Dt}qDu  Du��Dv�Dv}qDw  Dw��Dw�qDx� Dy  Dy}qDz  Dz� Dz�qD{}qD{�qD|}qD|��D}}qD~  D~� D~�qD}qD��D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD�  D�AHD��HD��HD���D�=qD�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D�HD�@ D�� D��HD�  D�@ D�~�D���D�HD�AHD�� D�� D�  D�@ D�� D���D�  D�B�D��HD��HD�HD�>�D�� D��HD���D�@ D��HD�� D���D�>�D�� D�� D�  D�@ D�~�D�� D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�@ D��HD��HD��D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�AHD�~�D��HD�  D�>�D��HD��HD���D�>�D�~�D�� D��D�B�D�~�D�� D�  D�@ D�� D���D��D�AHD�~�D���D�  D�@ D��HD�� D�HD�AHD���D��HD�  D�@ D�� D�� D�HD�AHD�� D�� D�  D�=qD�~�D���D���D�AHD���D��HD�HD�AHD���D�� D���D�@ D�� D���D�  D�@ D�� D��HD���D�@ D�� D��HD�  D�>�D��HD��HD�  D�@ D�~�D��HD�HD�AHD�� D���D��qD�=qD�~�D�� D�  D�>�D�� D�� D��)D�@ D���D��HD�HD�AHD��HD�� D�HD�AHD�~�D��qD�  D�@ D�}qD��qD��qD�=qD�}qD���D�  D�>�D�� D���D�HD�@ D�� D�� D�HD�@ D�}qD�� D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D�~�D���D�HD�B�D��HD�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD��D�AHD�~�D��HD�HD�AHD�~�D��)D��qD�>�D�� D���D���D�@ D�~�D���D�HD�B�D��HD��HD�  D�>�D�~�D�� D�  D�@ D�~�D�� D���D�>�D�� D���D���D�@ DHD�� D���D�=qD�~�Dþ�D���D�@ DāHD�� D���D�>�Dŀ D�� D�  D�@ D�~�Dƾ�D���D�@ Dǀ DǾ�D���D�>�D�~�DȾ�D�  D�@ D�~�D�� D���D�>�Dʀ D�� D���D�>�Dˀ D�� D�  D�@ D�~�D̾�D�  D�@ D�~�D�� D�HD�AHD�~�DνqD�  D�AHDρHD�� D�  D�@ DЀ D��HD�HD�>�D�~�DѾ�D���D�@ DҀ DҾ�D��qD�@ DӀ D�� D�HD�@ D�~�D�� D�HD�AHDՁHD�� D�  D�@ Dր D��HD�HD�AHDׁHD��HD�  D�>�D�~�D��HD�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D�~�D�� D��D�@ Dހ D޾�D�  D�>�D�}qD߾�D�  D�>�D�� D�� D���D�>�D� D�� D��qD�>�D� D�� D�  D�>�D� D�� D�HD�B�D�HD�� D�  D�AHD� D�� D���D�AHD悏D��HD�HD�AHD�~�D�� D�HD�AHD� D��HD�  D�@ D�~�D�qD�  D�AHD� D�� D���D�>�D� D��HD�  D�>�D� D��HD�  D�AHD킏D��HD�  D�>�D� D��HD�  D�>�D� D��HD�  D�>�D�� D�� D���D�>�D� D��HD��D�AHD� D�D��qD�>�D�HD�� D���D�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�~�D��qD���D�@ D�� D�� D�  D�@ D�~�D��HD�HD�@ D���D�� D���G�O�>�G�?B�\?�=q?�Q�?��@�@�R@5@O\)@^�R@u@��@�33@�p�@���@�
=@��@�\)@��H@�@���A�\A��A��A�Ap�A$z�A,(�A2�\A8Q�A@  AG
=AMp�AS�
A[�Ab�\Ai��Ap  Aw
=A}p�A��A�p�A�G�A�(�A�
=A���A��A��A��A���A��A���A��A�A�Q�A��A��
A�{A�  A�G�A��A�A��A�G�A�33A�p�A�\)A���A��HA��AƸRA�Q�A��HA���A�ffAϮA�=qA�(�A�A׮A��AۅA��A�
=A�G�A�33A���A�
=A���A�\A�z�A�ffA��A�=qA��
A�ffA�  A�G�A��A�A�\)B z�BG�BffB\)BQ�B��B�B
=B�
B��B	B
�RB�BQ�Bp�BffB33B  B��B{B
=B�B��B��B�RB�BQ�B�B=qB33B  B��B�B�HB�B z�B!��B"�\B#33B$Q�B%G�B&=qB&�HB(  B)�B)�B*�\B+�B,��B-B.�\B/\)B0z�B1p�B2ffB3
=B4(�B5G�B6{B6�RB7�B8��B9��B:ffB;\)B<Q�B=G�B=�B>�HB@  B@��BAG�BB{BC33BD  BD��BE��BFffBG\)BH(�BH��BI��BJffBK33BL(�BL��BM��BNffBO�BPQ�BP��BR{BS33BT(�BT��BV{BW33BXz�BYG�BZ�\B[�B\��B]�B_
=B`  Ba�Bb=qBc�Bd��Bf{Bg
=Bh(�Bip�Bj�RBl  Bm�Bn=qBo\)Bp��Bq�Bs33Btz�BuBv�HBx  ByG�Bz�\B|  B}G�B~�\B�  B�z�B�
=B�B�z�B��B�B�z�B��B��B�Q�B���B���B�Q�B�
=B��B�z�B��B��
B�z�B��B�B�z�B�33B�  B���B�p�B�(�B��HB��B�(�B��HB��B�=qB�
=B�B�z�B��B��B�z�B�
=B��B�Q�B���B�p�B��B�z�B��HB�\)B��B�  B�Q�B�z�B���B��HB�
=B�33B�\)B��B��B��
B�  B�(�B�Q�B��\B��RB��HB�
=B�33B�\)B�p�B��B��
B�  B�(�B�ffB���B���B��B�\)B��B��B��
B�{B�=qB�z�B���B��HB�
=B�\)B���B��
B�(�B�ffB���B��HB�33B�p�B��B��B�=qB�z�B���B���B�G�B��B��
B�{B�ffB��RB���B�G�B���B��B�Q�B���B���B�G�B���B��B�(�B�z�B���B��B�p�B�B�{B�ffB��RB��B�\)B��B�{B�ffB��RB�
=B�p�B�B�(�B��\B��HB�G�B��B�{B�ffB���B�33B��B�  B�Q�B���B��B��B��B�ffB��RB��B��B��B�Q�B��RB�
=B�p�B��
B�(�B��\B���B�G�B��B�  B�ffB���B��B�p�B��B�=qBď\B���B�G�BŮB�  B�ffBƸRB��B�p�B��
B�Q�BȸRB�
=B�p�B��
B�(�Bʏ\B��HB�33B˙�B��B�Q�Ḅ�B��BͅB��
B�=qBΣ�B�
=B�p�B�B�(�B�z�B��HB�33Bљ�B��B�Q�Bң�B���B�\)B�B�{B�z�B���B�33BՅB��
B�=qB֏\B���B��B�\)B׮B�  B�Q�B؏\B���B�G�BمB��B�=qBڏ\B���B��B�p�BۮB�  B�=qB܏\B��HB��B݅B��
B�=qBޏ\B���B�\)Bߙ�B��B�=qB�z�B��HB�33BᙚB�  B�Q�B�\B��HB�33B㙚B�  B�ffB��B���B�G�B�B�{B�z�B���B�
=B�p�B��
B�Q�B��B���B�G�B�B�(�B�\B��HB�33B뙚B�  B�ffB��HB�33B�B�  B�ffB���B�G�BB�(�B��B��B�B��B�ffB��HB�p�B��
B�=qB���B�33B��B�(�B��\B�
=B��B�{B��\B���B�p�B�  B��\B�
=B��B��B�ffB���B��B��B�ffB�
=B��C   C 33C z�C C
=CG�C�C��C{C\)C��C�HC(�CffCC
=CQ�C�\C��C{Cp�C�RC��C=qC�\C�HC(�Cp�C�RC  CQ�C��C�C	33C	z�C	��C
�C
p�C
�C
��CG�C��C��C=qC�C��C�Cz�C��C{C\)C��C  CQ�C��C��C=qC�\C�C=qC�C��C(�C�C��C{Cp�CC
=C\)C�RC  CG�C�C��C=qC��C��C=qC�C�HC33Cz�C��C(�Cz�CC{Cp�C�RC  C\)C�C��CG�C��C��C=qC�C�C=qCz�C�HC33C�C��C (�C z�C ��C!�C!z�C!��C"{C"z�C"C#
=C#p�C#C$
=C$\)C$�C%  C%G�C%��C%��C&33C&�\C&�HC'(�C'z�C'��C((�C(ffC(�C){C)ffC)�C*  C*ffC*�C*��C+\)C+�C+��C,G�C,��C-  C-G�C-�\C-�C.G�C.�\C.�
C/=qC/�\C/�HC033C0�\C0�
C1�C1z�C1�
C2(�C2p�C2C3(�C3z�C3�RC4
=C4p�C4��C5
=C5\)C5�RC6
=C6Q�C6��C7  C7\)C7��C7�C8G�C8��C8��C9=qC9�\C9�HC:=qC:��C:�HC;(�C;�C;�HC<33C<z�C<��C=(�C=z�C=��C>{C>p�C>��C?�C?p�C?�RC@�C@z�C@��CA{CAp�CA��CB�CBp�CBCC(�CC�CC�
CD�CDz�CD�
CE33CE�CE�
CF=qCF��CF�HCG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�A�A�%A�%A��;A�wA�A�\A�A�v�A�bNA�VA�?}A�9XA�?}A�33A��A�{A�%A���A��A��A��A��yA��yA��A��TA��HA��TA��/A�O�A�(�A��A�t�A�O�A���A� �A��A��A�^5A��RA�ĜA���A��;A���A��yA���A���A��!A�O�A��^A�C�A�+A�;dA�M�A��A�C�A�A���A���A���A��FA~I�Au�TAn1'Ah  Ad�A`E�A[��AXffASS�APQ�AL�AJ�jAE��AB��A@A<jA89XA7��A6  A5�PA4n�A3�A.�RA*�A*A*ZA*��A-VA/�hA0��A1�A2�jA2�A1��A1C�A0^5A/S�A.A�A,��A+x�A)��A(��A(��A'�mA&A�A%S�A$�\A$jA$Q�A$^5A$�9A$�yA$�A$�`A$�jA$r�A$bNA%+A%t�A$�A#�#A"�A"�A!��A!+A ��A�AC�A�A�9AƨA�Al�A`BA33A��AffA9XA1A�TA��A�A/A�jA9XA7LA�HA�!AbNA$�A(�A�#A33A��A��A�A�7A/A�9Av�A5?A��A7LA�A��A^5AA�A9XA5?A�A�;A�A+A��A�!AbNA  A�^A�A/A�/A��A�uA�DA9XA��A�AdZA;dA�A�RA�\AI�A��A�PAhsA7LAA
��A
z�A
VA
1'A	��A	��A	�7A	dZA	G�A	�AĜA{AA��Al�A�HA5?A��A7LAz�A$�AbA�mA?}A��A�RAz�A9XAA�^A�AO�A7LA%A �A �A 1@��
@�l�@��@��R@�n�@��T@���@�X@��/@�j@�1'@��@���@�+@��!@�~�@�5?@��7@��@�bN@���@�ff@��#@�/@�Q�@�(�@�w@�P@�+@�V@��@��@��@�r�@�ƨ@�C�@�V@�7@���@�bN@��@�l�@��y@�!@�V@��@�(�@���@�!@�+@�v�@�^5@��@�G�@�I�@�o@���@�M�@�@�h@�X@��`@��@�M�@��@���@���@ݙ�@���@��@�t�@��@�M�@�@ّh@�hs@��/@؛�@�Q�@��@ם�@�+@�ȴ@֗�@�X@�j@�Z@�  @Ӆ@ҸR@Ѻ^@�?}@���@ЋD@�Q�@��@�S�@�M�@Ͳ-@�?}@��/@̣�@ˮ@ʸR@�J@ɡ�@�Ĝ@�1'@���@�t�@�33@��@��H@���@�~�@�E�@�5?@��T@�p�@�V@�bN@ÍP@�K�@�"�@��y@�^5@�p�@�&�@�%@�Ĝ@�1'@�ȴ@��#@�/@��/@���@��@��@�;d@��@��@�~�@�-@�hs@�bN@��F@��P@�@��+@�@���@�x�@�7L@���@�I�@�1@��@�l�@��@�@��H@��!@���@�~�@�$�@���@���@�?}@��9@�j@���@��@�S�@�
=@��!@�V@�J@��@�Ĝ@�j@�ƨ@���@��P@�S�@���@���@���@��@�t�@�"�@�@��@��H@��@���@�~�@�-@���@���@�7L@��@���@��/@��u@�"�@�=q@�$�@�J@���@���@��@��#@��-@�hs@�/@��@�%@���@���@���@��@�Q�@��
@���@���@�\)@�+@�ȴ@��\@�n�@�M�@�=q@�@��^@��h@�&�@���@��`@���@��9@���@��m@�+@��@��R@�v�@��@��@��-@�x�@�/@��@�V@��`@���@��@�Q�@� �@�b@�1@��@�t�@�@�=q@��T@��^@�x�@�X@�/@��/@���@�A�@�1@��w@�;d@���@��y@�ȴ@�ff@��@�@��-@�`B@�1@�C�@��@�ȴ@�ff@���@�x�@�hs@�hs@�`B@�`B@�X@�X@�G�@���@���@��u@�r�@�b@�\)@��@��R@��+@��+@�v�@�=q@��@��@��#@��h@�7L@��@���@���@�z�@�j@��@��w@��@���@��@�K�@�+@���@�v�@�=q@�{@���@���@�p�@�`B@�O�@�V@���@�I�@�9X@��@��
@�ƨ@��@��P@�\)@�C�@�+@���@��\@�M�@�$�@���@��h@�p�@�?}@�Ĝ@�A�@�;@K�@~�y@~�+@}��@}/@}V@|�@|�/@|��@|z�@|1@{C�@z�!@zn�@z=q@y��@y��@y�@y�7@yG�@y%@x�u@x1'@w�@wK�@w+@w+@v��@v$�@u�T@u��@up�@u/@t�@tI�@s�F@so@r=q@rJ@q�#@qhs@pĜ@o|�@n��@nV@nV@n@m��@m`B@m�@l�@l9X@k��@j��@j�@i�7@h��@hĜ@h �@gl�@g+@f�y@fv�@e�T@e��@e`B@d�@dZ@dI�@dI�@dI�@d9X@c�m@c�@cS�@c"�@b��@a��@aX@a%@`�`@`��@`�@`b@_��@_�P@_l�@^��@^ff@]�@]@]��@]�h@]?}@\��@\j@[ƨ@[�@[S�@[33@[o@Z�H@Z�!@Z�\@Z=q@Y��@Y��@Y�7@Y7L@X�9@W�@V�R@V@U�T@U@U�@T�@T�D@TI�@T1@S��@S"�@R��@R�@Q��@Qhs@Q�@P��@P��@P �@O��@Ol�@O
=@Nȴ@N��@Nff@NE�@N5?@M�-@M?}@L��@Lz�@L9X@L1@Kt�@J�@J�!@JM�@I�7@I%@HĜ@H�@HQ�@G�@G�P@G;d@G
=@F�@Fȴ@Fȴ@F��@F$�@F{@E�T@Ep�@EV@D��@DZ@C�
@CdZ@CC�@Co@B��@BJ@A��@@��@?��@?;d@>�@>�+@>E�@=�@=��@=@=p�@=V@<��@<�j@<�j@<I�@;��@;�
@;��@;�@;t�@;33@:�!@:^5@9��@9x�@9&�@8�u@8b@7;d@6�@6��@6ff@5�@5�T@5�T@5�T@5�-@5�h@5�@4�@4�j@4z�@4(�@3�F@3dZ@2�@2��@2=q@2�@2J@1�^@1x�@1x�@1X@1&�@0�`@0��@0�@0Q�@0  @/�@/l�@/
=@.��@.V@-�@-?}@,z�@,�@+�m@+dZ@+o@*�H@*��@*�\@*M�@*=q@*-@)�@)��@)hs@)hs@(��@(�@(bN@(A�@(b@( �@( �@(b@(  @'�@'�@';d@&��@&�y@&��@%�T@%?}@$��@$��@$�j@$�j@$��@$�D@$j@$(�@#�F@#��@#��@#t�@#33@"�!@"n�@"J@!��@!�7@!%@ ��@ �9@ r�@  �@�w@�@��@��@l�@\)@+@
=@��@�@��@��@�+@E�@5?@$�@{@�@��@�-@�/@z�@j@(�@1@�m@��@��@�@dZ@@��@�\@^5@�#@hs@7L@%@��@�9@r�@r�@A�@1'@b@  @��@|�@K�@�@
=@
=@��@�y@ȴ@��@��@�+@V@5?@$�@�@��@`B@?}@�@�D@z�@z�@z�@z�@j@j@I�@�@�m@�F@dZ@o@�@��@��@�\@~�@M�@J@��@hs@G�@�@Ĝ@�@  @�;@��@�@�y@ȴ@E�@$�@��@�h@�@?}@�@��@�j@�@j@9X@(�@(�@(�@�A���A�  A�  A�%A�  A�  A�  A�%A�A�A�1A�1A�A�A�%A��A��;A�ĜA���A�^A�-A��A囦A�uA�+A�~�A�~�A�x�A�p�A�hsA�bNA�ZA�S�A�ZA�\)A�S�A�O�A�O�A�G�A�=qA�;dA�;dA�9XA�7LA�5?A�9XA�=qA�9XA�;dA�A�A�C�A�=qA�?}A�?}A�;dA�7LA�7LA�9XA�33A�/A�1'A�-A�"�A��A��A��A��A��A��A��A�{A��A��A��A�oA�{A��A�{A�VA�oA�{A�JA�%A�1A�%A�A�  A�A���A���A���A���A���A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��yA��yA��A��A��yA��mA��A��A��A��yA��mA��yA��A��yA��`A��mA��A��yA��mA��A��A��A��mA��A��A��A��yA��A��A��yA��TA��TA��`A��TA��;A��HA��mA��`A��TA��TA��mA��`A��HA��HA��TA��HA��#A��#A��HA��TA��;A��;A��TA��`A��`A��TA��`A��mA��TA��/A��/A��HA��TA��;A��#A��A��#A��/A��
A�ȴA�-A��A�\A�x�A�A�9A�~�A���A߉7A�^5Aݡ�A��A�(�A�1A�A���A�ƨA�hsA�/A�9XA�  Aҝ�A�r�A�
=Aѩ�A�|�A��A�/A��/A�-AͬA��A�ƨA̰!A̡�A�hsA��`A�-Aʰ!A�bNA�7LA�-A�$�A�{AɓuA�O�A��A�{A�JA���A�ĜA�hsAŶFA��
A�VA�E�A��HA¶FA¥�AuA�hsA��A��-A��A�z�A���A�7LA��A�ȴA��+A�(�A��^A�9XA���A�~�A�S�A�&�A��A��A��HA�x�A��A��FA�jA� �A�oA��A�A�dZA��#A�-A���A���A���A��A�\)A�9XA�{A��yA��wA���A�\)A�"�A��A��A�ȴA���A���A�ȴA�ƨA�ĜA�ƨA�ȴA�ƨA��A��A�O�A�+A�bA��yA�hsA�9XA��A��-A�n�A�+A��mA��hA�-A��
A���A��A�t�A�dZA�I�A�/A��A���A�ƨA���A�hsA�%A��+A�bA��A�jA��A��yA�ȴA���A��!A���A��+A�t�A�VA��A���A���A�|�A�`BA�E�A�(�A��A��A�v�A�XA�E�A�1'A��A���A���A��
A���A���A�t�A�ZA�^5A��A���A��HA���A��wA���A���A���A��A��A��FA�^5A���A��9A�I�A��jA��+A�$�A�VA���A�oA��uA�`BA�A��DA�9XA�  A�ƨA��uA�M�A�oA���A��A��/A��A���A���A��7A�hsA�JA�A�A��A�VA��A��mA���A�C�A���A��+A��A��;A���A�^5A�{A�  A���A��DA�7LA�ffA�?}A���A���A���A�1'A��;A�ȴA���A�?}A���A�t�A��A�n�A��uA�|�A��TA�l�A���A�ffA�"�A��DA��#A�p�A��A���A��7A��A�t�A�p�A�ffA�dZA�M�A�A�A�=qA�-A��A���A��\A�jA�K�A�A�A�1'A��A�oA�%A�A��A��A��A��HA���A���A�ȴA���A��FA��DA��\A��PA���A�|�A�jA��A�A��
A�v�A�
=A��-A�O�A��A��!A���A�v�A�;dA�  A���A���A�p�A�XA�?}A�$�A��A���A��hA�33AA`BAVA~�RA~  A|^5A{�Ay�#AyVAx��Ax�`AxbNAw7LAu�mAtȴAt$�Ar�!AqVApVAo�Ao��Ao�hAn��AnVAm33Alz�Al  Ak&�Aj=qAi?}AhjAg��AghsAg
=AfȴAf��Af9XAe�Ae|�AedZAd�HAdZAc�^Ab��Aa�
AaƨAa��Aa\)A`��A`A_&�A]�mA]�PA]33A\ȴA\ �A[�A[oAZ�uAZ�AY��AYG�AX��AXn�AX^5AX5?AW��AV��AU`BAT-ASdZAR�yARffAR{AQ�mAQ�^AQ�AP��APz�AO�TAO&�AN�\AN�AM�AMK�AL�/ALr�AL=qAK�;AK�wAKx�AKC�AKAJ�!AIƨAH5?AGG�AF�\AF�AE�AD-AC�AC��AC`BAC�AB�jAA�TAAp�AAp�AA�A@�\A?��A?�7A?O�A>�A>�/A>(�A<�A;��A:ȴA:=qA9�7A8��A8bNA81A7A7��A7��A7�FA7A7ƨA7��A7t�A7%A6bNA6bA5�A5�mA5��A5�^A5ƨA5��A5�FA5�PA5�A5`BA5�A4�jA4�\A4^5A4M�A49XA4(�A3�A3��A3t�A3oA2�A2 �A1oA/��A.�/A.1'A-�hA,��A+�A+A*�`A*ȴA*��A*ZA*$�A)��A*  A)��A*A*1A*bA* �A*-A*v�A*n�A*n�A*r�A*r�A*�+A*�DA*��A*��A+�7A+��A+��A,bA,ffA-��A.�A/&�A/O�A/p�A/�A/�A/�wA/�A0I�A0��A1
=A1oA1�A17LA1K�A1t�A1ƨA1�;A1�mA1��A2(�A2�A2�jA2�/A3
=A3A3A2��A2��A2r�A21'A1�A1��A1�TA21A2(�A2$�A1��A1��A1�A1�A1S�A1"�A0�`A0��A0��A0��A0Q�A0-A0�A0  A/�TA/��A/�7A/�A.�/A.ĜA.��A.�A.jA.Q�A.  A-�;A-�A-S�A-�A,��A,��A,�A,E�A+�FA+�A+|�A+C�A*��A*�RA*z�A*=qA)�;A)C�A(�HA(�RA(�jA(�9A(�jA(��A(�A(��A(��A(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         A�  A�A�A�%A�%A��;A�wA�A�\A�A�v�A�bNA�VA�?}A�9XA�?}A�33A��A�{A�%A���A��A��A��A��yA��yA��A��TA��HA��TA��/A�O�A�(�A��A�t�A�O�A���A� �A��A��A�^5A��RA�ĜA���A��;A���A��yA���A���A��!A�O�A��^A�C�A�+A�;dA�M�A��A�C�A�A���A���A���A��FA~I�Au�TAn1'Ah  Ad�A`E�A[��AXffASS�APQ�AL�AJ�jAE��AB��A@A<jA89XA7��A6  A5�PA4n�A3�A.�RA*�A*A*ZA*��A-VA/�hA0��A1�A2�jA2�A1��A1C�A0^5A/S�A.A�A,��A+x�A)��A(��A(��A'�mA&A�A%S�A$�\A$jA$Q�A$^5A$�9A$�yA$�A$�`A$�jA$r�A$bNA%+A%t�A$�A#�#A"�A"�A!��A!+A ��A�AC�A�A�9AƨA�Al�A`BA33A��AffA9XA1A�TA��A�A/A�jA9XA7LA�HA�!AbNA$�A(�A�#A33A��A��A�A�7A/A�9Av�A5?A��A7LA�A��A^5AA�A9XA5?A�A�;A�A+A��A�!AbNA  A�^A�A/A�/A��A�uA�DA9XA��A�AdZA;dA�A�RA�\AI�A��A�PAhsA7LAA
��A
z�A
VA
1'A	��A	��A	�7A	dZA	G�A	�AĜA{AA��Al�A�HA5?A��A7LAz�A$�AbA�mA?}A��A�RAz�A9XAA�^A�AO�A7LA%A �A �A 1@��
@�l�@��@��R@�n�@��T@���@�X@��/@�j@�1'@��@���@�+@��!@�~�@�5?@��7@��@�bN@���@�ff@��#@�/@�Q�@�(�@�w@�P@�+@�V@��@��@��@�r�@�ƨ@�C�@�V@�7@���@�bN@��@�l�@��y@�!@�V@��@�(�@���@�!@�+@�v�@�^5@��@�G�@�I�@�o@���@�M�@�@�h@�X@��`@��@�M�@��@���@���@ݙ�@���@��@�t�@��@�M�@�@ّh@�hs@��/@؛�@�Q�@��@ם�@�+@�ȴ@֗�@�X@�j@�Z@�  @Ӆ@ҸR@Ѻ^@�?}@���@ЋD@�Q�@��@�S�@�M�@Ͳ-@�?}@��/@̣�@ˮ@ʸR@�J@ɡ�@�Ĝ@�1'@���@�t�@�33@��@��H@���@�~�@�E�@�5?@��T@�p�@�V@�bN@ÍP@�K�@�"�@��y@�^5@�p�@�&�@�%@�Ĝ@�1'@�ȴ@��#@�/@��/@���@��@��@�;d@��@��@�~�@�-@�hs@�bN@��F@��P@�@��+@�@���@�x�@�7L@���@�I�@�1@��@�l�@��@�@��H@��!@���@�~�@�$�@���@���@�?}@��9@�j@���@��@�S�@�
=@��!@�V@�J@��@�Ĝ@�j@�ƨ@���@��P@�S�@���@���@���@��@�t�@�"�@�@��@��H@��@���@�~�@�-@���@���@�7L@��@���@��/@��u@�"�@�=q@�$�@�J@���@���@��@��#@��-@�hs@�/@��@�%@���@���@���@��@�Q�@��
@���@���@�\)@�+@�ȴ@��\@�n�@�M�@�=q@�@��^@��h@�&�@���@��`@���@��9@���@��m@�+@��@��R@�v�@��@��@��-@�x�@�/@��@�V@��`@���@��@�Q�@� �@�b@�1@��@�t�@�@�=q@��T@��^@�x�@�X@�/@��/@���@�A�@�1@��w@�;d@���@��y@�ȴ@�ff@��@�@��-@�`B@�1@�C�@��@�ȴ@�ff@���@�x�@�hs@�hs@�`B@�`B@�X@�X@�G�@���@���@��u@�r�@�b@�\)@��@��R@��+@��+@�v�@�=q@��@��@��#@��h@�7L@��@���@���@�z�@�j@��@��w@��@���@��@�K�@�+@���@�v�@�=q@�{@���@���@�p�@�`B@�O�@�V@���@�I�@�9X@��@��
@�ƨ@��@��P@�\)@�C�@�+@���@��\@�M�@�$�@���@��h@�p�@�?}@�Ĝ@�A�@�;@K�@~�y@~�+@}��@}/@}V@|�@|�/@|��@|z�@|1@{C�@z�!@zn�@z=q@y��@y��@y�@y�7@yG�@y%@x�u@x1'@w�@wK�@w+@w+@v��@v$�@u�T@u��@up�@u/@t�@tI�@s�F@so@r=q@rJ@q�#@qhs@pĜ@o|�@n��@nV@nV@n@m��@m`B@m�@l�@l9X@k��@j��@j�@i�7@h��@hĜ@h �@gl�@g+@f�y@fv�@e�T@e��@e`B@d�@dZ@dI�@dI�@dI�@d9X@c�m@c�@cS�@c"�@b��@a��@aX@a%@`�`@`��@`�@`b@_��@_�P@_l�@^��@^ff@]�@]@]��@]�h@]?}@\��@\j@[ƨ@[�@[S�@[33@[o@Z�H@Z�!@Z�\@Z=q@Y��@Y��@Y�7@Y7L@X�9@W�@V�R@V@U�T@U@U�@T�@T�D@TI�@T1@S��@S"�@R��@R�@Q��@Qhs@Q�@P��@P��@P �@O��@Ol�@O
=@Nȴ@N��@Nff@NE�@N5?@M�-@M?}@L��@Lz�@L9X@L1@Kt�@J�@J�!@JM�@I�7@I%@HĜ@H�@HQ�@G�@G�P@G;d@G
=@F�@Fȴ@Fȴ@F��@F$�@F{@E�T@Ep�@EV@D��@DZ@C�
@CdZ@CC�@Co@B��@BJ@A��@@��@?��@?;d@>�@>�+@>E�@=�@=��@=@=p�@=V@<��@<�j@<�j@<I�@;��@;�
@;��@;�@;t�@;33@:�!@:^5@9��@9x�@9&�@8�u@8b@7;d@6�@6��@6ff@5�@5�T@5�T@5�T@5�-@5�h@5�@4�@4�j@4z�@4(�@3�F@3dZ@2�@2��@2=q@2�@2J@1�^@1x�@1x�@1X@1&�@0�`@0��@0�@0Q�@0  @/�@/l�@/
=@.��@.V@-�@-?}@,z�@,�@+�m@+dZ@+o@*�H@*��@*�\@*M�@*=q@*-@)�@)��@)hs@)hs@(��@(�@(bN@(A�@(b@( �@( �@(b@(  @'�@'�@';d@&��@&�y@&��@%�T@%?}@$��@$��@$�j@$�j@$��@$�D@$j@$(�@#�F@#��@#��@#t�@#33@"�!@"n�@"J@!��@!�7@!%@ ��@ �9@ r�@  �@�w@�@��@��@l�@\)@+@
=@��@�@��@��@�+@E�@5?@$�@{@�@��@�-@�/@z�@j@(�@1@�m@��@��@�@dZ@@��@�\@^5@�#@hs@7L@%@��@�9@r�@r�@A�@1'@b@  @��@|�@K�@�@
=@
=@��@�y@ȴ@��@��@�+@V@5?@$�@�@��@`B@?}@�@�D@z�@z�@z�@z�@j@j@I�@�@�m@�F@dZ@o@�@��@��@�\@~�@M�@J@��@hs@G�@�@Ĝ@�@  @�;@��@�@�y@ȴ@E�@$�@��@�h@�@?}@�@��@�j@�@j@9X@(�@(�@(�G�O�A���A�  A�  A�%A�  A�  A�  A�%A�A�A�1A�1A�A�A�%A��A��;A�ĜA���A�^A�-A��A囦A�uA�+A�~�A�~�A�x�A�p�A�hsA�bNA�ZA�S�A�ZA�\)A�S�A�O�A�O�A�G�A�=qA�;dA�;dA�9XA�7LA�5?A�9XA�=qA�9XA�;dA�A�A�C�A�=qA�?}A�?}A�;dA�7LA�7LA�9XA�33A�/A�1'A�-A�"�A��A��A��A��A��A��A��A�{A��A��A��A�oA�{A��A�{A�VA�oA�{A�JA�%A�1A�%A�A�  A�A���A���A���A���A���A��A���A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��yA��yA��A��A��yA��mA��A��A��A��yA��mA��yA��A��yA��`A��mA��A��yA��mA��A��A��A��mA��A��A��A��yA��A��A��yA��TA��TA��`A��TA��;A��HA��mA��`A��TA��TA��mA��`A��HA��HA��TA��HA��#A��#A��HA��TA��;A��;A��TA��`A��`A��TA��`A��mA��TA��/A��/A��HA��TA��;A��#A��A��#A��/A��
A�ȴA�-A��A�\A�x�A�A�9A�~�A���A߉7A�^5Aݡ�A��A�(�A�1A�A���A�ƨA�hsA�/A�9XA�  Aҝ�A�r�A�
=Aѩ�A�|�A��A�/A��/A�-AͬA��A�ƨA̰!A̡�A�hsA��`A�-Aʰ!A�bNA�7LA�-A�$�A�{AɓuA�O�A��A�{A�JA���A�ĜA�hsAŶFA��
A�VA�E�A��HA¶FA¥�AuA�hsA��A��-A��A�z�A���A�7LA��A�ȴA��+A�(�A��^A�9XA���A�~�A�S�A�&�A��A��A��HA�x�A��A��FA�jA� �A�oA��A�A�dZA��#A�-A���A���A���A��A�\)A�9XA�{A��yA��wA���A�\)A�"�A��A��A�ȴA���A���A�ȴA�ƨA�ĜA�ƨA�ȴA�ƨA��A��A�O�A�+A�bA��yA�hsA�9XA��A��-A�n�A�+A��mA��hA�-A��
A���A��A�t�A�dZA�I�A�/A��A���A�ƨA���A�hsA�%A��+A�bA��A�jA��A��yA�ȴA���A��!A���A��+A�t�A�VA��A���A���A�|�A�`BA�E�A�(�A��A��A�v�A�XA�E�A�1'A��A���A���A��
A���A���A�t�A�ZA�^5A��A���A��HA���A��wA���A���A���A��A��A��FA�^5A���A��9A�I�A��jA��+A�$�A�VA���A�oA��uA�`BA�A��DA�9XA�  A�ƨA��uA�M�A�oA���A��A��/A��A���A���A��7A�hsA�JA�A�A��A�VA��A��mA���A�C�A���A��+A��A��;A���A�^5A�{A�  A���A��DA�7LA�ffA�?}A���A���A���A�1'A��;A�ȴA���A�?}A���A�t�A��A�n�A��uA�|�A��TA�l�A���A�ffA�"�A��DA��#A�p�A��A���A��7A��A�t�A�p�A�ffA�dZA�M�A�A�A�=qA�-A��A���A��\A�jA�K�A�A�A�1'A��A�oA�%A�A��A��A��A��HA���A���A�ȴA���A��FA��DA��\A��PA���A�|�A�jA��A�A��
A�v�A�
=A��-A�O�A��A��!A���A�v�A�;dA�  A���A���A�p�A�XA�?}A�$�A��A���A��hA�33AA`BAVA~�RA~  A|^5A{�Ay�#AyVAx��Ax�`AxbNAw7LAu�mAtȴAt$�Ar�!AqVApVAo�Ao��Ao�hAn��AnVAm33Alz�Al  Ak&�Aj=qAi?}AhjAg��AghsAg
=AfȴAf��Af9XAe�Ae|�AedZAd�HAdZAc�^Ab��Aa�
AaƨAa��Aa\)A`��A`A_&�A]�mA]�PA]33A\ȴA\ �A[�A[oAZ�uAZ�AY��AYG�AX��AXn�AX^5AX5?AW��AV��AU`BAT-ASdZAR�yARffAR{AQ�mAQ�^AQ�AP��APz�AO�TAO&�AN�\AN�AM�AMK�AL�/ALr�AL=qAK�;AK�wAKx�AKC�AKAJ�!AIƨAH5?AGG�AF�\AF�AE�AD-AC�AC��AC`BAC�AB�jAA�TAAp�AAp�AA�A@�\A?��A?�7A?O�A>�A>�/A>(�A<�A;��A:ȴA:=qA9�7A8��A8bNA81A7A7��A7��A7�FA7A7ƨA7��A7t�A7%A6bNA6bA5�A5�mA5��A5�^A5ƨA5��A5�FA5�PA5�A5`BA5�A4�jA4�\A4^5A4M�A49XA4(�A3�A3��A3t�A3oA2�A2 �A1oA/��A.�/A.1'A-�hA,��A+�A+A*�`A*ȴA*��A*ZA*$�A)��A*  A)��A*A*1A*bA* �A*-A*v�A*n�A*n�A*r�A*r�A*�+A*�DA*��A*��A+�7A+��A+��A,bA,ffA-��A.�A/&�A/O�A/p�A/�A/�A/�wA/�A0I�A0��A1
=A1oA1�A17LA1K�A1t�A1ƨA1�;A1�mA1��A2(�A2�A2�jA2�/A3
=A3A3A2��A2��A2r�A21'A1�A1��A1�TA21A2(�A2$�A1��A1��A1�A1�A1S�A1"�A0�`A0��A0��A0��A0Q�A0-A0�A0  A/�TA/��A/�7A/�A.�/A.ĜA.��A.�A.jA.Q�A.  A-�;A-�A-S�A-�A,��A,��A,�A,E�A+�FA+�A+|�A+C�A*��A*�RA*z�A*=qA)�;A)C�A(�HA(�RA(�jA(�9A(�jA(��A(�A(��A(��A(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�xB��B�%B�~B%zB�WB��B�]B�"B�JB�B��B�PB��B��BMB�B�B�B�B
=BxBBbB{B�BVB$�B'�B*�B*�BE9B	J#B	��B	��B	��B	�_B	��B	�B
?B
^jB
��B
��B
�vB
�2B
�>B
�GB
�B
��B
��B
�1B
��B
h
B
8B
$�B
$B
�B
B
xB	�+B	�	B
(XB	�.B	��B	�?B	��B	xlB	f2B	OvB	7LB	%B	!B	�B	�B	B	�B��B	�B	�B	�B	4B	hB	MB	�B	B	JB�B�B�iB��B	/�B	u�B	�_B	��B	��B	�[B	ɆB	�mB	��B	یB	�]B	�WB	�B	�B	҉B	֡B	�B	�pB	�dB	�RB	�B	�
B	��B	�B	�JB	��B	�"B	�cB
+B
�B
'�B
9�B
<B
=<B
@B
E�B
K^B
L0B
N�B
PHB
OBB
L�B
M6B
RTB
R�B
U�B
V�B
WsB
YKB
Z�B
Z�B
\�B
]�B
^jB
^5B
aB
aHB
^jB
]�B
\)B
[�B
^B
]dB
_�B
d&B
aHB
`vB
a|B
bB
`�B
_�B
`vB
_�B
^�B
]�B
]�B
\�B
\�B
\]B
[�B
[WB
[#B
\�B
\)B
\�B
[�B
Z�B
ZB
Z�B
X�B
W?B
WsB
V�B
V9B
V�B
VmB
VmB
X�B
XEB
Y�B
XyB
XEB
XB
XyB
W
B
W
B
VB
TaB
S�B
S�B
R�B
S&B
R B
P�B
P�B
VB
VB
V9B
UgB
S�B
T�B
T�B
X�B
U�B
T�B
TaB
R�B
R�B
P}B
OB
N�B
L�B
K�B
M�B
K)B
HB
G�B
IB
G�B
E�B
C�B
B[B
D�B
C�B
D�B
CaB
B�B
A B
@�B
AUB
>�B
>BB
>BB
<�B
<jB
<jB
<jB
<jB
:�B
:�B
:*B
:�B
9�B
8�B
8�B
8RB
7�B
6zB
8B
5?B
5�B
5B
4B
3�B
2�B
2aB
1�B
2aB
1'B
0�B
0�B
/�B
/�B
.�B
.�B
.�B
,�B
,qB
,B
,B
+kB
+B
+B
*�B
,B
*eB
(�B
(�B
($B
'�B
($B
'�B
(XB
&�B
%FB
&B
$�B
$B
#�B
$�B
&B
$@B
#:B
"�B
"�B
"hB
#:B
"�B
"4B
"hB
!-B
 �B
 �B
�B
!-B
 �B
!-B
!-B
 \B
 �B
�B
�B
 �B
CB
CB
�B
CB
qB
7B
�B
�B
1B
�B
�B
�B
7B
1B
_B
B
�B
�B
B
�B
 B
 B
(B
\B
�B
�B
�B
�B
@B
�B
uB
B
uB
uB
@B
{B
uB
�B
:B
B
�B
�B
�B
bB
�B
.B
�B
�B
"B
�B
B
�B
B
B
�B
�B
xB

�B
B
"B
PB
�B
�B
�B
�B
�B
�B
(B
�B
bB
�B
4B
�B
�B
�B
B
�B
oB
oB
�B
�B
�B
FB
�B
�B
+B
�B
_B
�B
�B
�B
�B
kB
�B
�B
kB
B
�B
1B
�B
B
_B
eB
eB
�B
�B
�B
_B
_B
_B
_B
�B
�B
�B
�B
�B
�B
_B
�B
xB
B
xB
�B
xB
xB
�B
�B
IB
OB
OB
�B
�B
�B
�B
OB
B
 'B
 �B
 'B
 \B
 �B
 �B
!�B
!�B
"4B
"hB
"4B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
&B
%�B
&B
%�B
&B
&LB
&B
&LB
&�B
&�B
&�B
&�B
'RB
'�B
'�B
(XB
(�B
(�B
(�B
(�B
*�B
+6B
-CB
-B
-CB
-�B
-�B
-�B
.IB
-�B
/OB
.}B
.�B
/�B
/OB
/B
/OB
/OB
/�B
/�B
.�B
/OB
1'B
0�B
1[B
1�B
2-B
3�B
2aB
2aB
2aB
2aB
2aB
2-B
2-B
2aB
2�B
2�B
2�B
2�B
2�B
49B
49B
4�B
4�B
4�B
4�B
5�B
6�B
6�B
7LB
7�B
8�B
8�B
8�B
9�B
9XB
9XB
;0B
;dB
;dB
;0B
;�B
<B
;�B
<�B
<jB
<�B
=qB
=�B
>BB
>B
>BB
>B
>�B
?HB
?�B
?}B
?�B
@OB
?�B
@�B
@OB
@�B
@�B
@�B
AUB
A�B
A�B
B[B
CaB
C-B
CaB
C�B
DgB
E9B
E�B
E�B
E�B
E�B
GB
F�B
GB
F�B
F�B
GB
GB
GzB
HB
HB
HKB
H�B
H�B
HKB
H�B
H�B
H�B
H�B
IB
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
MB
M6B
L�B
L�B
MjB
MB
OBB
OBB
N�B
N�B
N�B
OB
OBB
OB
OvB
O�B
PHB
P}B
P�B
QB
QNB
P�B
Q�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
S�B
S�B
S[B
S�B
S[B
S[B
S�B
S�B
S�B
S�B
T�B
T�B
UgB
U2B
T�B
T�B
UgB
U�B
U�B
VB
U�B
V�B
VmB
W?B
V�B
V�B
W
B
WsB
W
B
XB
W�B
XEB
XEB
XB
XEB
XEB
XEB
XEB
X�B
X�B
XyB
X�B
X�B
YB
Y�B
[WB
Z�B
ZQB
ZB
ZQB
Z�B
[#B
[#B
[WB
[�B
[�B
[�B
]/B
\�B
\�B
]/B
\�B
\�B
]dB
]dB
]�B
]�B
^B
^B
^5B
^B
^5B
^�B
_B
_;B
_B
_pB
_;B
`vB
`B
`B
aB
a�B
a�B
a�B
bB
a�B
bNB
b�B
b�B
c B
b�B
c B
b�B
c B
c�B
c B
cTB
c�B
d&B
d&B
d�B
d�B
e`B
d�B
e,B
e,B
e�B
e�B
gB
gmB
h
B
h
B
hsB
hsB
h�B
h�B
h�B
iDB
iyB
i�B
i�B
iyB
jKB
jB
jKB
jB
jB
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
l�B
m�B
m�B
m�B
n/B
n�B
n�B
ncB
n�B
n�B
n�B
n�B
o5B
o5B
oiB
o�B
p;B
poB
p�B
qB
qAB
qvB
qvB
q�B
q�B
q�B
rB
rGB
rGB
rB
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
v+B
v`B
v�B
v`B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
wfB
xB
x8B
xlB
x�B
x�B
xlB
xlB
xlB
x�B
xlB
xlB
x�B
x�B
x�B
y	B
y�B
z�B
{B
{B
{B
{B
z�B
{B
{�B
|B
|�B
{�B
{�B
{�B
|B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~(B
.B
~�B
cB
.B
cB
.B
cB
~�B
�B
�iB
�B
�4B
�4B
�B
�B
��B
�4B
�4B
��B
�iB
�iB
�iB
�B
��B
�oB
�AB
�AB
�AB
�uB
��B
��B
��B
�B
�{B
�GB
�GB
�B
�MB
�MB
��B
�MB
�SB
�B
�MB
��B
��B
�SB
�B
��B
��B
��B
�YB
�%B
�YB
�YB
��B
��B
��B
��B
�+B
�_B
��B
��B
��B
��B
��B
��B
�fB
�fB
�1B
�1B
�fB
�1B
�fB
�1B
�1B
�fB
�fB
��B
��B
�7B
�B
��B
��B
�	B
��B
�	B
�=B
��B
�rB
�=B
��B
�xB
�DB
�DB
�DB
��B
�JB
�B
�B
�B
��B
�PB
��B
�PB
��B
�"B
�VB
�VB
�VB
��B
�(B
�(B
��B
�(B
��B�B�JB�xB��B�MB�%B�lB��B��B��B�PB��B=B/�B&�B��B��B �B��B�VB��B��B�(B�B��B�PB��B�B��B�PB�B��B��B��B�B�"B��B��B�PB��B��B��B��B��B��B��B��B �B�BBGB%B�B�B�B�B�B�B�B�B%BB�BYB�B�B�B�B�B�B%B�BMBSB�B�B�B%B_BYB�B1BfB+B+B�B�B_B	lB
�B	7B�B
�BxB
�B
=B�B�B
rB
rB�B�B�BB�B�BBxB�B�BB~B"B\BVB�B�B�B4B.B�BBBoB�BoBFBB�BBSB�B�BSBYB�B�B�BkBB�B�B�B 'B�B�B!�B!�B!-B!-B"�B$�B#�B#�B%�B'�B(XB&�B'B(�B(�B'B&�B($B($B&�B'RB)�B*�B)_B)�B+�B-B,qB+�B+�B,�B,=B+B)�B+B+�B+�B)�B'B(�B*eB1�B0UB0UB2�BU�BrGBx�B�0B	`vB	3�B	C-B	FtB	LdB	��B	��B	�B	�B	u�B	r�B	�4B	�CB	|�B	cB	�fB	��B	� B	�B	��B	�B	��B	�tB	�B	�B	��B	�\B	��B	��B	�aB	�kB	��B	��B	��B	��B	��B	��B	� B	�KB	ɆB	��B	ΥB	یB	�B
$B
.B
9�B
K^B
B�B
@�B
<�B
=�B
B�B
J�B
V9B
hsB
yrB
��B
�B
�YB
��B
�(B
�=B
��B
��B
�[B
��B
��B
�B
ÖB
�NB
�|B
�B
�>B
�B
��B
�B
�MB
��B
��B
��BB�B
��B
��B
��B
��B
�PB
��B
��B
�B
�B
�>B
�xB
�ZB
�B
�B
�QB
�)B
� B
�5B
�B
�vB
�vB
�vB
��B
��B
��B
��B
�vB
�AB
��B
��B
�
B
�]B
�cB
�B
�AB
�vB
�B
�B
�;B
��B
�sB
�QB
��B
��B
�"B
�B
�"B
�B
�ZB
�TB
�B
��B
�B
��B
�B
��B
�RB
�tB
��B
B
��B
��B
�*B
��B
�B
�B
��B
��B
��B
��B
�'B
��B
��B
�6B
��B
��B
�@B
�hB
�~B
�IB
�-B
�B
��B
��B
��B
� B
��B
��B
��B
�{B
��B
�FB
�:B
��B
�uB
�	B
�B
� B
�1B
�oB
�\B
y�B
s�B
{JB
�7B
k�B
jB
e�B
VmB
W�B
Y�B
OB
J�B
GzB
B'B
L0B
7�B
2�B
.IB
*�B
'�B
&�B
&LB
�B
�B
$tB
6�B
FtB
B
�B
1B
(�B
$@B
(B
#B
SB
4B
�B
 �B
�B
B
�B
:B
�B
+�B
~B
B
	�B
+6B
�B
"B
{B
�B
�B
�B
�B	��B
uB
�B
�B
B
\B
0UB
uB
7�B
/B
+�B
!B
�B
hB
�B
+B
B
  B	��B	�cB
B	�PB	��B	�xB	�ZB	��B	�fB	��B	�MB	�oB	�B	��B	�]B	�AB	�B	�B	��B	��B	�B	�TB	�AB	��B	�B	�B	��B
!�B
�B
2�B
B�B
ZQB
4B
&�B
�B
�B
�B
B
B
�B
xB
	7B
DB

=B
�B
B
�B	�2B	�	B	�fB	�B	��B	�|B	��B	�B	��B	�B	��B	�KB	�B
oB	��B	�B	�gB	ƨB	�mB	�)B	�WB	�vB	�zB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B	�(B	�;B	x�B	s�B	o�B	l"B	h�B	n�B	e,B	c�B	aB	iyB	dZB	d&B	h>B	\�B	OB	QNB	K)B	R�B	YB	N�B	S&B	9�B	:�B	;dB	>�B	49B	4�B	3�B	,�B	(�B	)_B	(XB	$@B		B	�B	"�B	0UB	0�B	'�B	�B		B	�B	�B	{B	hB	�B	�B	�B	_B	qB	�B	4B	�B	�B	�B	"B	�B	
=B	YB	B	SB	�B	%B	�B	�B	PB	�B	�B	�B	�B��B�B	  B�(B	�B		�B��B��B		�B		lB	
rB	�B	B	AB��B	{B	�B	B	VB	�B	�B	�B	�B	�B	
rB	�B	MB	YB	bB	bB	B	$B	+B	7B	�B	�B	xB	�B	�B	�B	B	MB	�B	�B	B	�B		B	B	qB	B	�B	=B	!�B	$tB	xB	�B	$B	=B	�B	eB	�B	�B	�B��B	�B�B�B�B�sB�B��B�fB��B�,B�mB��B��B��B�B��B��B�B��B��B�2B�fB��B�]B	qB	�B	7B	&LB	#:B��B	aHB	d�B	t�B	r�B	zB	z�B	~(B	zB	iDB	�AB	�iB	�fB	�.B	�oB	�\B	��B	��B	��B	��B	�B	��B	�LB	��B	�B	��B	�UB	��B	��B	�UB	��B	�gB	��B	��B	�B	��B	�#B	�[B	�sB	�B	�sB	�B	�?B	�9B	��B	�NB	�B	�NB	��B	��B	ӏB	�sB	�EB	�B	ޞB	�jB	�]B	�QB	�B	��B	��B	��B	�B	�#B	�]B	�BB	�;B	ںB	ںB	خB	�B	�B	уB	�}B	�9B	� B	�B	ԕB	�,B	��B	ܒB	՛B	�vB	��B	҉B	��B	ҽB	��B	��B	��B	�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         B�>B��B��B��B'B�.B��B�KB��B��B��B��B��B��B��B�B�BB	BB
fB�B,BnB{B�BvB$�B'�B+~B1�Bl6B	rB	��B	��B	��B	�-B	��B
�B
IiB
mWB
��B
�B
�{BaB	B�B
��B
жB
��B
��B
�,B
{7B
K;B
<�B
/�B
2]B
/�B
6B	��B
�B
<�B
>B

�B	��B	��B	�pB	t�B	^xB	C6B	5�B	)0B	 �B	�B	B	iB	�B	�B	�B	RB	zB	CB	yB	!yB	,zB	�B�B��B�QB��B	'NB	p�B	�<B	��B	�1B	�(B	�+B	ٱB	ךB	�HB	��B	��B	ۚB	ٖB	ҶB	�B	ޒB	��B	�B	��B	ɤB	��B	ܳB	�B	�2B	��B	��B
 hB
.B
ZB
&�B
<HB
>�B
@�B
AB
HYB
NB
NRB
QB
R�B
PuB
M�B
PIB
S7B
SQB
U�B
W�B
X�B
Z�B
[�B
[�B
]NB
^<B
_mB
_zB
b�B
cHB
a�B
_ B
\�B
\�B
^�B
]�B
aB
fZB
bEB
a�B
ciB
dB
b=B
a_B
adB
`�B
`�B
_RB
^�B
]�B
]�B
\�B
[�B
[uB
[�B
]�B
]kB
^B
\�B
[:B
[9B
[�B
Y�B
XB
X�B
W�B
W!B
V�B
V�B
W�B
Y�B
YcB
Z�B
YB
X�B
YxB
Y#B
X#B
X�B
WB
T�B
T�B
T�B
SB
TYB
R�B
QGB
Q�B
W=B
VNB
V�B
U�B
T�B
V4B
W:B
Y�B
V B
U�B
VoB
UfB
T�B
R=B
Q�B
O�B
L�B
L�B
PB
L�B
H�B
H�B
JB
HzB
F�B
D�B
CB
E>B
DwB
F�B
D�B
CB
A�B
A�B
BIB
>�B
>�B
?=B
=UB
=B
=PB
=7B
<�B
;~B
;�B
;B
;�B
9�B
9�B
:*B
9zB
8�B
8�B
9-B
6VB
6�B
6�B
4kB
4dB
3B
3)B
3�B
3&B
1�B
1�B
2/B
14B
0�B
0\B
09B
/�B
-�B
-TB
-B
,�B
+�B
+�B
,fB
-XB
.B
+ B
)DB
(�B
(lB
(�B
)hB
)�B
*{B
'UB
&CB
'B
%@B
$�B
$�B
&SB
)&B
$�B
#�B
#7B
#B
#�B
$�B
$B
#bB
#jB
!�B
!�B
!,B
 �B
!�B
!$B
!�B
!�B
!:B
!�B
 UB
!�B
"!B
�B
�B
�B
�B
.B
#B
iB
�B
�B
�B
�B
�B
`B
B
B
�B
CB
TB
\B
�B
�B
B
�B
B
JB
B
�B
B
�B
EB
�B
�B
QB
HB
�B
�B
�B
 B
�B
*B
�B
dB
B
�B
-B
�B
NB
�B
�B
AB
#B
�B
�B
dB
cB
�B
;B
`B
�B
`B
�B
�B
xB
�B
qB
*B
IB
 B
�B
�B
FB
�B
2B
�B
B
gB
�B
�B
B
tB
FB
lB
NB
�B
�B
�B
�B
�B
�B
vB
�B
�B
 B
[B
�B
�B
2B
B
IB
�B
NB
B
�B
�B
B
�B
�B
}B
�B
�B

B
qB
B
�B
@B
B
�B
B
DB
B
HB
�B
�B
�B
�B
�B
;B
�B
�B
�B
�B
�B
�B
�B
}B
KB
!B
 �B
 �B
 �B
! B
!B
"nB
"HB
"vB
"�B
"�B
#*B
#.B
#�B
$*B
#�B
#�B
#�B
$)B
%B
'_B
&\B
&�B
&kB
&�B
&�B
&�B
&�B
'B
'B
'B
'BB
'�B
'�B
(UB
(�B
(�B
)B
)B
)�B
+�B
,�B
-�B
-kB
-�B
-�B
.B
.OB
.�B
.�B
/�B
/!B
/�B
02B
/|B
/nB
0B
/�B
0)B
/�B
/�B
1�B
2�B
1�B
1�B
2bB
3�B
3�B
2�B
2hB
2uB
2hB
2tB
27B
2[B
2�B
3rB
2�B
2�B
3cB
3�B
5B
4�B
4�B
4�B
4�B
5B
64B
6�B
7B
7�B
8�B
9yB
99B
9RB
9�B
9�B
:9B
;�B
;�B
;�B
;mB
<<B
<RB
<�B
=GB
<�B
<�B
=�B
>KB
>�B
>7B
>oB
>�B
?vB
?�B
?�B
?�B
@aB
@uB
@ B
@�B
@�B
@�B
@�B
AdB
A�B
BsB
A�B
B�B
C�B
CwB
C�B
D�B
EdB
E�B
F3B
F>B
FBB
F�B
G�B
GB
G4B
F�B
GB
G>B
G�B
H<B
H�B
H]B
H�B
H�B
H�B
HhB
H�B
H�B
H�B
IaB
I�B
I�B
JNB
JB
JB
JTB
K.B
KB
KB
K+B
KB
K�B
L2B
LdB
L�B
M�B
MnB
M	B
MJB
NB
N=B
PB
O�B
N�B
N�B
O;B
OTB
O�B
O�B
O�B
PHB
Q&B
Q*B
QxB
Q�B
Q�B
Q�B
R�B
RB
R6B
RdB
R�B
S9B
R�B
S�B
S�B
S�B
SbB
S�B
SvB
S�B
T\B
S�B
T0B
T%B
UjB
UnB
U�B
UVB
UB
UVB
U�B
VB
VB
V1B
VKB
W2B
V�B
WqB
V�B
V�B
W^B
W�B
W�B
X�B
X(B
XuB
XiB
X8B
XzB
XyB
XnB
X�B
Y#B
X�B
X�B
Y;B
Y:B
ZB
Z�B
[�B
Z�B
ZyB
ZiB
Z�B
[OB
[jB
[kB
[�B
\EB
\UB
\�B
]�B
]CB
]QB
]uB
]5B
]FB
]�B
]�B
^,B
^B
^7B
^6B
^XB
^!B
^�B
_FB
_SB
_�B
_KB
_�B
_�B
`�B
`XB
`B
a�B
b/B
a�B
b*B
bOB
bIB
b�B
b�B
cB
cRB
b�B
c*B
c'B
c�B
c�B
c\B
c�B
d!B
dnB
d�B
eEB
edB
e�B
e1B
e}B
e�B
fNB
f�B
g�B
hB
hnB
haB
h�B
h�B
i B
h�B
i4B
i�B
i�B
i�B
i�B
i�B
j�B
j>B
j�B
j�B
j�B
j�B
k7B
kBB
k�B
k�B
lB
l�B
l�B
m�B
m�B
m�B
nB
n�B
n�B
n�B
nmB
n�B
n�B
n�B
o%B
omB
o{B
o�B
pDB
p�B
p�B
q0B
qlB
qfB
q�B
q�B
rB
q�B
rB
rHB
r�B
rbB
reB
r�B
s5B
sB
s6B
s�B
tB
tCB
t[B
t�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
wB
v�B
wB
v�B
w@B
w�B
w�B
w�B
xsB
x_B
x�B
x�B
x�B
xqB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
zIB
{%B
{IB
{�B
{B
{8B
z�B
{?B
{�B
|�B
|�B
{�B
{�B
|1B
|�B
}�B
~ B
~B
~rB
~�B
~�B
~�B
~tB
}B
WB
xB
BB
jB
_B
yB
2B
�B
�yB
�B
�bB
�HB
�B
�B
��B
�IB
�LB
��B
��B
��B
�7B
�jB
��B
��B
�cB
�gB
��B
��B
��B
��B
�EB
�rB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�'B
��B
��B
��B
�hB
�WB
�AB
�"B
�"B
�lB
�,B
�lB
�oB
��B
��B
��B
�B
�\B
��B
��B
��B
��B
�?B
��B
��B
��B
�zB
�5B
�5B
�iB
�DB
�mB
�WB
�eB
��B
��B
��B
��B
�[B
�6B
��B
��B
�B
�B
�MB
��B
�B
��B
�vB
�0B
��B
��B
�oB
��B
�*B
�~B
�CB
��B
�BB
�B
��B
��B
��B
�	B
�EB
�jB
�mB
��B
��B
�9B
�.B
��B
�9G�O�B�B�JB�xB��B�MB�%B�lB��B��B��B�PB��B=B/�B&�B��B��B �B��B�VB��B��B�(B�B��B�PB��B�B��B�PB�B��B��B��B�B�"B��B��B�PB��B��B��B��B��B��B��B��B �B�BBGB%B�B�B�B�B�B�B�B�B%BB�BYB�B�B�B�B�B�B%B�BMBSB�B�B�B%B_BYB�B1BfB+B+B�B�B_B	lB
�B	7B�B
�BxB
�B
=B�B�B
rB
rB�B�B�BB�B�BBxB�B�BB~B"B\BVB�B�B�B4B.B�BBBoB�BoBFBB�BBSB�B�BSBYB�B�B�BkBB�B�B�B 'B�B�B!�B!�B!-B!-B"�B$�B#�B#�B%�B'�B(XB&�B'B(�B(�B'B&�B($B($B&�B'RB)�B*�B)_B)�B+�B-B,qB+�B+�B,�B,=B+B)�B+B+�B+�B)�B'B(�B*eB1�B0UB0UB2�BU�BrGBx�B�0B	`vB	3�B	C-B	FtB	LdB	��B	��B	�B	�B	u�B	r�B	�4B	�CB	|�B	cB	�fB	��B	� B	�B	��B	�B	��B	�tB	�B	�B	��B	�\B	��B	��B	�aB	�kB	��B	��B	��B	��B	��B	��B	� B	�KB	ɆB	��B	ΥB	یB	�B
$B
.B
9�B
K^B
B�B
@�B
<�B
=�B
B�B
J�B
V9B
hsB
yrB
��B
�B
�YB
��B
�(B
�=B
��B
��B
�[B
��B
��B
�B
ÖB
�NB
�|B
�B
�>B
�B
��B
�B
�MB
��B
��B
��BB�B
��B
��B
��B
��B
�PB
��B
��B
�B
�B
�>B
�xB
�ZB
�B
�B
�QB
�)B
� B
�5B
�B
�vB
�vB
�vB
��B
��B
��B
��B
�vB
�AB
��B
��B
�
B
�]B
�cB
�B
�AB
�vB
�B
�B
�;B
��B
�sB
�QB
��B
��B
�"B
�B
�"B
�B
�ZB
�TB
�B
��B
�B
��B
�B
��B
�RB
�tB
��B
B
��B
��B
�*B
��B
�B
�B
��B
��B
��B
��B
�'B
��B
��B
�6B
��B
��B
�@B
�hB
�~B
�IB
�-B
�B
��B
��B
��B
� B
��B
��B
��B
�{B
��B
�FB
�:B
��B
�uB
�	B
�B
� B
�1B
�oB
�\B
y�B
s�B
{JB
�7B
k�B
jB
e�B
VmB
W�B
Y�B
OB
J�B
GzB
B'B
L0B
7�B
2�B
.IB
*�B
'�B
&�B
&LB
�B
�B
$tB
6�B
FtB
B
�B
1B
(�B
$@B
(B
#B
SB
4B
�B
 �B
�B
B
�B
:B
�B
+�B
~B
B
	�B
+6B
�B
"B
{B
�B
�B
�B
�B	��B
uB
�B
�B
B
\B
0UB
uB
7�B
/B
+�B
!B
�B
hB
�B
+B
B
  B	��B	�cB
B	�PB	��B	�xB	�ZB	��B	�fB	��B	�MB	�oB	�B	��B	�]B	�AB	�B	�B	��B	��B	�B	�TB	�AB	��B	�B	�B	��B
!�B
�B
2�B
B�B
ZQB
4B
&�B
�B
�B
�B
B
B
�B
xB
	7B
DB

=B
�B
B
�B	�2B	�	B	�fB	�B	��B	�|B	��B	�B	��B	�B	��B	�KB	�B
oB	��B	�B	�gB	ƨB	�mB	�)B	�WB	�vB	�zB	��B	�tB	��B	��B	��B	��B	��B	��B	��B	�xB	��B	��B	��B	��B	�(B	�;B	x�B	s�B	o�B	l"B	h�B	n�B	e,B	c�B	aB	iyB	dZB	d&B	h>B	\�B	OB	QNB	K)B	R�B	YB	N�B	S&B	9�B	:�B	;dB	>�B	49B	4�B	3�B	,�B	(�B	)_B	(XB	$@B		B	�B	"�B	0UB	0�B	'�B	�B		B	�B	�B	{B	hB	�B	�B	�B	_B	qB	�B	4B	�B	�B	�B	"B	�B	
=B	YB	B	SB	�B	%B	�B	�B	PB	�B	�B	�B	�B��B�B	  B�(B	�B		�B��B��B		�B		lB	
rB	�B	B	AB��B	{B	�B	B	VB	�B	�B	�B	�B	�B	
rB	�B	MB	YB	bB	bB	B	$B	+B	7B	�B	�B	xB	�B	�B	�B	B	MB	�B	�B	B	�B		B	B	qB	B	�B	=B	!�B	$tB	xB	�B	$B	=B	�B	eB	�B	�B	�B��B	�B�B�B�B�sB�B��B�fB��B�,B�mB��B��B��B�B��B��B�B��B��B�2B�fB��B�]B	qB	�B	7B	&LB	#:B��B	aHB	d�B	t�B	r�B	zB	z�B	~(B	zB	iDB	�AB	�iB	�fB	�.B	�oB	�\B	��B	��B	��B	��B	�B	��B	�LB	��B	�B	��B	�UB	��B	��B	�UB	��B	�gB	��B	��B	�B	��B	�#B	�[B	�sB	�B	�sB	�B	�?B	�9B	��B	�NB	�B	�NB	��B	��B	ӏB	�sB	�EB	�B	ޞB	�jB	�]B	�QB	�B	��B	��B	��B	�B	�#B	�]B	�BB	�;B	ںB	ںB	خB	�B	�B	уB	�}B	�9B	� B	�B	ԕB	�,B	��B	ܒB	՛B	�vB	��B	҉B	��B	ҽB	��B	��B	��B	�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
=��=!ǚ<��<���<c��<C��<f�<�&4<<�M<��<Sʡ<I��<F��<�
<�0X<��E<���<Sq><*�<Ma;<Й�<�u<���<�c�<��&=t<ۢv<#�
<#�<�֦<�NO<��<ދ<�%*<�� <TlY<{��<�sj<R��<�:<8Q�<G��<#�
<���<@</J?<`A<d�<#�
<#�
<#�
<#�
<#�
<�[H<\a-<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019111006414420191110064144IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019112004003120191120040031QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019112004003120191120040031QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573120200109065731IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                