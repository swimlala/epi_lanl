CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-12-07T23:29:03Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20181207232903  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               !   !AA  AOAO7316_008644_033                 7316_008644_033                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @ؖy��s@ؖy��s11  @ؖzOv_�@ؖzOv_�@+H¤T�@+H¤T��cӊ��?��cӊ��?�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @�G�@��\@��R@޸R@��RA\)A\)A+�A?\)A_\)A\)A�Q�A�Q�A�  A��A�  A�  A�A��B  B  B(�B   B(  B0  B8(�B@  BH(�BP  BW�
B_�
Bg�
Bp  BzffB}B�  B�{B�  B�{B�{B�  B�  B�  B�  B�  B�{B�  B��B�  B�{B��B��B��B�  B�  B��B��B�{B�(�B�{B�{B�  B�  B�(�B�  B��C 
=C  C  C��C��C
  C
=C
=C  C  C  C  C{C
=C
=C�C   C!��C#��C%��C(  C*  C,  C.  C0  C2  C4  C5��C8  C:  C;��C>  C?��CB  CD
=CF  CG��CJ  CL  CN  CP  CR  CT  CV
=CX
=CZ  C\  C]��C`  Cb  Cd
=Cf
=Ch
=Cj  Ck��Cm��Cp  Cq��Cs��Cv  Cw�Cy��C|  C~  C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C�C�  C�C�  C�  C�  C���C���C�  C�  C�C�C�C�C�
=C�C�  C�  C�C�C�  C�  C���C�C�
=C�  C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�  C���C���C���C�  C�  C�  C�C�C�C�  C���C���C�  C�C�C�C�C�C�
=C�  C�C�C���C���C�  C�
=C�
=C�C�C�C�  C�C�
=C�C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C���C���C���C�  C�  C�  C���C���C�C�C�  C�  C�  C�  C���C�  C�C�  D   D }qD  D}qD��D}qD  D}qD��D� D�D��D  D��D  D� D�D��D	�D	�D
�D
� D�D��D  D� D�qDz�D  D� D�D��D�qD� D  D� D�D��D  D� D  D}qD  D� D�D��D�qD}qD�D��D�qD� D�D� D  D}qD  D��D  D� D  D� D  D� D �D � D �qD!}qD"  D"��D#  D#}qD#�qD$� D%�D%��D&  D&z�D&�qD'� D'�qD(z�D(�qD)� D*�D*� D*�qD+� D,  D,��D-D-� D-�qD.� D/�D/� D/�qD0� D1�D1� D1�qD2��D3  D3� D3�qD4}qD4�qD5}qD5�qD6}qD7�D7��D8�D8��D8�qD9}qD:�D:��D;  D;��D<  D<� D=D=� D=�qD>� D?  D?z�D?�qD@� D@�qDA� DB  DBz�DB��DC� DD�DD��DEDE��DF�DF� DG  DG}qDG�qDH� DI  DI� DJ  DJ� DJ�qDK}qDK�qDL� DM  DM� DN�DN��DO�DO��DP  DP� DQ�DQ� DR�DR��DS�DS��DS�qDTz�DT�qDU��DV�DV� DV��DW}qDW�qDX� DX�qDY� DZ  DZ� D[�D[�D\D\� D\�qD]� D^  D^��D_  D_}qD`�D`� Da  Da� Db  Db��Dc�Dc}qDd  Dd��De  De}qDe�qDf� Dg�Dg� Dg�qDh� Dh�qDi}qDj  Dj��Dk�Dk� Dk�qDl}qDm  Dm� DnDn��Do�Do� Dp  Dp� Dq  Dq� Dr�Dr�DsDs��Dt�Dt��Du  Du}qDu��Dv}qDw  Dw� Dx�Dx� Dx�qDy� Dz�Dz��D{  D{� D|  D|� D|�qD}z�D}�qD~}qD  D��D�  D�>�D�~�D���D���D�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD��HD�  D�AHD�~�D�� D�  D�>�D�� D���D���D�@ D�~�D���D��qD�@ D��HD��HD���D�>�D��HD��HD�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�AHD�� D���D���D�AHD���D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD���D�� D�  D�AHD���D��HD�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�AHD�� D���D���D�@ D�� D�� D���D�@ D�� D�� D�HD�>�D�� D�� D���D�>�D�� D�� D�  D�B�D���D��HD�HD�B�D��HD��HD���D�@ D���D��HD���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�B�D��HD�� D�HD�B�D�� D���D���D�@ D��HD��HD���D�@ D��HD���D�  D�@ D�� D��HD�  D�>�D�� D��HD���D�=qD�}qD���D�HD�@ D�~�D��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��HD���D�=qD�~�D���D���D�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D��HD�� D���D�=qD��HD�� D���D�@ D���D�D���D�=qD�� D�� D�HD�B�D��HD��HD�HD�AHD��HD�� D��qD�>�D�~�D�� D���D�@ D��HD�� D���D�=qD�~�D�� D�HD�@ D�� D��HD�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD D¾�D�  D�@ DÀ D��HD��D�@ D�~�Dľ�D���D�>�Dŀ D�� D���D�@ Dƀ Dƾ�D�  D�@ D�~�D�� D�  D�AHDȁHDȾ�D���D�@ Dɀ Dɾ�D�  D�AHDʁHD�� D�  D�@ D�~�D˾�D���D�@ D�~�D̾�D�  D�B�D̀ D;�D�  D�AHD΁HD�� D�  D�AHDπ D��HD�HD�AHDЁHD��HD��D�@ D�~�D�� D���D�>�DҀ D��HD��D�AHDӀ D�D�HD�AHDԀ DԾ�D���D�>�D�~�Dվ�D�  D�>�D�~�D�� D�HD�@ D�~�D׾�D��qD�=qD�}qDؾ�D���D�>�D�~�D�� D�HD�>�Dڀ D�� D���D�@ DہHD�� D���D�@ D܀ D�� D�  D�AHD�~�D�� D�  D�=qD�~�D�� D���D�>�D߀ D�D�  D�@ D�� D�qD���D�@ D�HD��HD�  D�@ D� D�� D�HD�@ D�HD�� D�  D�@ D� D��HD�HD�@ D� D��HD�HD�B�D� D澸D�  D�>�D�~�D�� D�HD�AHD�HD�� D�  D�@ D� D�qD��qD�@ D�HD��HD�  D�@ D�HD뾸D�  D�AHD�~�D쾸D���D�@ D�HD��HD��D�AHD�~�D�� D�HD�@ D� DﾸD���D�@ D�� D��HD���D�>�D� D��HD��D�AHD�~�D�� D�HD�AHD�HD��HD�  D�AHD�HD��HD���D�@ D���D�D�  D�>�D�~�D���D�  D�AHD��HD��HD�  D�=qD�� D��HD�HD�8RD���?\)?B�\?k�?�z�?�Q�?���?�@\)@��@+�@:�H@J=q@\(�@h��@xQ�@�ff@�{@�@��H@��\@��@�33@���@�  @���@У�@�@�p�@��@�{@�@�p�A33AQ�A(�A\)A�
A��Ap�A!G�A%�A*=qA.�RA333A7
=A;�A@��AE�AI��AN�RAS33AX��A\��A`  Ae�Ai��Amp�AqG�Aw
=A|(�A\)A���A�(�A��RA���A��\A���A�\)A���A�33A�p�A��A��A�z�A�ffA���A��\A��A��A���A��A�A���A��HA���A�
=A���A��
A�A�  A�=qA��A�
=A�G�A�33A�A�Q�A�=qA�z�A�ffA���A��HA��A�\)AᙚA�(�A�ffA�Q�A�\A���A�\)A�G�A�33A�p�A�  A�=qA�z�A�ffB (�Bp�B�\B�Bz�B��B
=B(�B	�B
{B33Bz�B��B�RB�B��B�B\)BQ�Bp�B�\B�B�B=qB33BQ�B��B�HB (�B!G�B"ffB#�B$��B&=qB'\)B(z�B)B+
=B,Q�B-p�B.�\B/�
B1G�B2�\B3�B4��B6{B7�B8��B9�B;33B<(�B=��B?
=B@(�BAG�BBffBC�BD��BF=qBG
=BHQ�BIBJ�HBK�
BL��BM�BO33BPz�BQp�BRffBS�BT��BUBV�HBX  BX��BYBZ�HB\  B\��B]B^�RB_�B`��BaBb�\Bc�Bdz�Be��Bf�\Bg�BhQ�Bi�Bj{Bk33BlQ�Bm�Bn{Bn�HBp  Bp��Bq�Bs
=Bt  Bt��Bu��Bv�RBw�Bxz�ByG�By�Bz�\B{\)B|  B|Q�B|��B|��B}�B}p�B}p�B}p�B}G�B}p�B}B}B}��B}��B}B}�B}�B}B}B}B}B}�B}B}��B}��B}��B}��B}B}��B}��B}p�B}p�B}p�B}��B}p�B}p�B}G�B}�B}�B}p�B}G�B}�B|��B|��B|��B|��B|��B|��B|��B|Q�B|Q�B|(�B|(�B|(�B|  B|  B{�B{�B{\)B{\)B{\)B{\)B{
=B{
=Bz�HBz�RBz�\BzffBz�\Bz�\Bz�\BzffBz=qBz=qBzffBzffBz�\Bz�RBz�HBz�HB{
=B{
=B{33B{�B|  B|Q�B|��B|��B}G�B}��B}�B~ffB~�HB�B�(�B�ffB��RB�
=B�\)B���B��B�=qB��\B���B�\)B�B�(�B��\B��HB�G�B��B��B�Q�B���B�33B��B�{B�z�B��HB�\)B�B�(�B��\B���B�\)B�B�=qB���B��B���B�{B�z�B���B�p�B��
B�=qB���B�
=B�\)B��
B�(�B�z�B���B�33B���B�  B�ffB���B��B��B��B�=qB��\B���B�\)B��B�{B�z�B��HB�G�B�B�(�B���B��B��B��B�Q�B���B�G�B���B�  B�ffB��HB�G�B��B�(�B��\B���B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�\)B�B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B�B�=qB��\B���B�p�B��
B�ffB��RB�33B��B�{B�z�B�
=B�p�B��B�Q�B���B�G�B�B�=qB���B��B��B�{B�z�B���B�p�B�  B�Q�B���B�G�B�B�=qB���B��B���B�{B��\B�
=B��B�  B�z�B�
=B�p�B�  B��\B��B���B�(�B¸RB�G�B��B�ffB���Bř�B�(�BƸRB�G�B��
B�ffB�
=BɅB�{Bʣ�B�33B�B�=qB���B�G�B��B�z�B�
=BϮB�=qB��HBхB�{BҸRB�G�B�B�ffB��HB�p�B�{B֣�B�G�B��B؏\B�33B�B�ffB�
=Bۙ�B�=qBܸRB�\)B��Bޣ�B�33B��B�z�B��B�B�=qB��HB�p�B�  B�\B�33B��
B�z�B��B�B�(�B�RB�\)B��B�\B�G�B��B�\B�33B��
B�Q�B��HB�p�B�{B�RB�\)B�  B�\B�33B�B�=qB���B�\)B�  B���B�G�B��
B�z�B�
=B���B�(�B���B��B�{B��RB�\)B��
B�z�B��B�C 33C �C ��C�CffC�C  CG�C��C��CG�C��C�HC(�Cz�C�RC
=CQ�C��C  CQ�C��C�
C�CffC�RC  C\)C��C�C	33C	p�C	C

=C
ffC
�RC  CG�C�\C�HC(�C�C�
C(�Cp�C�RC  CQ�C��C��CG�C��C�HC�Cp�CC{CffC�RC  CG�C��C�HCG�C��C��C=qC�\C�
C(�C�C�
C33C�\C�
C(�Cp�C��C(�Cz�C�
C(�Cz�C��C�Cp�CC�Cp�C��C
=CQ�C��C�C(�Cz�CC  C33CffC�\C�RC�
C  C(�CQ�Cz�C�C��C�C 
=C 33C \)C �C �RC �HC ��C!�C!G�C!p�C!��C!C!�HC"  C"(�C"Q�C"�C"�C"�
C#  C#�C#G�C#p�C#��C#�
C$
=C$33C$Q�C$z�C$��C$��C%
=C%=qC%ffC%�\C%�RC%�HC&{C&G�C&z�C&�C&�HC'
=C'33C'ffC'�\C'��C(  C(=qC(p�C(�\C(C(�C)(�C)ffC)��C)��C)��C*(�C*\)C*��C*��C+
=C+=qC+ffC+��C+��C,
=C,=qC,z�C,�C,�HC-{C-G�C-p�C-��C-�HC.{C.Q�C.�C.�RC.�C/{C/Q�C/�\C/C0  C0=qC0ffC0��C0��C1  C1=qC1p�C1�C1�C2�C2\)C2�C2�RC2�C3�C3\)C3��C3�
C4  C433C4\)C4��C4�
C5{C5G�C5p�C5��C5�
C6
=C6Q�C6�C6�RC6�C7{C7G�C7�C7C8  C8(�C8\)C8�\C8�RC9  C933C9p�C9��C9�
C:  C:(�C:\)C:��C:��C;  C;(�C;\)C;�\C;C<  C<33C<ffC<��C<C<��C=(�C=Q�C=�C=C=��C>(�C>\)C>�\C>�RC>�HC?{C?Q�C?�C?�RC?�HC@
=C@=qC@ffC@��C@��CA
=CA=qCAp�CA��CACA�CB�CBQ�CB�\CBCB�CC{CC=qCCffCC��CC��CD  CD33CDffCD�\CD�RCD�HCE  CE33CEffCE��CE��CE��CF(�CFQ�CFp�CF��CF�
CG
=CG33CGffCG�CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  ?��@�\@@  @�G�@��\@��R@޸R@��RA\)A\)A+�A?\)A_\)A\)A�Q�A�Q�A�  A��A�  A�  A�A��B  B  B(�B   B(  B0  B8(�B@  BH(�BP  BW�
B_�
Bg�
Bp  BzffB}B�  B�{B�  B�{B�{B�  B�  B�  B�  B�  B�{B�  B��B�  B�{B��B��B��B�  B�  B��B��B�{B�(�B�{B�{B�  B�  B�(�B�  B��C 
=C  C  C��C��C
  C
=C
=C  C  C  C  C{C
=C
=C�C   C!��C#��C%��C(  C*  C,  C.  C0  C2  C4  C5��C8  C:  C;��C>  C?��CB  CD
=CF  CG��CJ  CL  CN  CP  CR  CT  CV
=CX
=CZ  C\  C]��C`  Cb  Cd
=Cf
=Ch
=Cj  Ck��Cm��Cp  Cq��Cs��Cv  Cw�Cy��C|  C~  C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C�C�  C�C�  C�  C�  C���C���C�  C�  C�C�C�C�C�
=C�C�  C�  C�C�C�  C�  C���C�C�
=C�  C�  C�C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C�  C���C���C���C�  C�  C�  C�C�C�C�  C���C���C�  C�C�C�C�C�C�
=C�  C�C�C���C���C�  C�
=C�
=C�C�C�C�  C�C�
=C�C�  C�  C�  C�  C�C�C���C���C�  C�  C�  C�  C�  C�  C�  C�C�  C���C�  C���C���C���C�  C�  C�  C���C���C�C�C�  C�  C�  C�  C���C�  C�C�  D   D }qD  D}qD��D}qD  D}qD��D� D�D��D  D��D  D� D�D��D	�D	�D
�D
� D�D��D  D� D�qDz�D  D� D�D��D�qD� D  D� D�D��D  D� D  D}qD  D� D�D��D�qD}qD�D��D�qD� D�D� D  D}qD  D��D  D� D  D� D  D� D �D � D �qD!}qD"  D"��D#  D#}qD#�qD$� D%�D%��D&  D&z�D&�qD'� D'�qD(z�D(�qD)� D*�D*� D*�qD+� D,  D,��D-D-� D-�qD.� D/�D/� D/�qD0� D1�D1� D1�qD2��D3  D3� D3�qD4}qD4�qD5}qD5�qD6}qD7�D7��D8�D8��D8�qD9}qD:�D:��D;  D;��D<  D<� D=D=� D=�qD>� D?  D?z�D?�qD@� D@�qDA� DB  DBz�DB��DC� DD�DD��DEDE��DF�DF� DG  DG}qDG�qDH� DI  DI� DJ  DJ� DJ�qDK}qDK�qDL� DM  DM� DN�DN��DO�DO��DP  DP� DQ�DQ� DR�DR��DS�DS��DS�qDTz�DT�qDU��DV�DV� DV��DW}qDW�qDX� DX�qDY� DZ  DZ� D[�D[�D\D\� D\�qD]� D^  D^��D_  D_}qD`�D`� Da  Da� Db  Db��Dc�Dc}qDd  Dd��De  De}qDe�qDf� Dg�Dg� Dg�qDh� Dh�qDi}qDj  Dj��Dk�Dk� Dk�qDl}qDm  Dm� DnDn��Do�Do� Dp  Dp� Dq  Dq� Dr�Dr�DsDs��Dt�Dt��Du  Du}qDu��Dv}qDw  Dw� Dx�Dx� Dx�qDy� Dz�Dz��D{  D{� D|  D|� D|�qD}z�D}�qD~}qD  D��D�  D�>�D�~�D���D���D�>�D�~�D��HD�  D�>�D��HD��HD�HD�AHD��HD��HD�  D�AHD�~�D�� D�  D�>�D�� D���D���D�@ D�~�D���D��qD�@ D��HD��HD���D�>�D��HD��HD�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�AHD�� D���D���D�AHD���D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�� D���D�>�D�~�D���D���D�>�D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD���D�� D�  D�AHD���D��HD�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�AHD�� D���D���D�@ D�� D�� D���D�@ D�� D�� D�HD�>�D�� D�� D���D�>�D�� D�� D�  D�B�D���D��HD�HD�B�D��HD��HD���D�@ D���D��HD���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�B�D��HD�� D�HD�B�D�� D���D���D�@ D��HD��HD���D�@ D��HD���D�  D�@ D�� D��HD�  D�>�D�� D��HD���D�=qD�}qD���D�HD�@ D�~�D��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D��HD���D�=qD�~�D���D���D�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D��HD�� D���D�=qD��HD�� D���D�@ D���D�D���D�=qD�� D�� D�HD�B�D��HD��HD�HD�AHD��HD�� D��qD�>�D�~�D�� D���D�@ D��HD�� D���D�=qD�~�D�� D�HD�@ D�� D��HD�  D�>�D�� D�� D���D�>�D�� D��HD�  D�@ D�� D�� D�HD�AHD D¾�D�  D�@ DÀ D��HD��D�@ D�~�Dľ�D���D�>�Dŀ D�� D���D�@ Dƀ Dƾ�D�  D�@ D�~�D�� D�  D�AHDȁHDȾ�D���D�@ Dɀ Dɾ�D�  D�AHDʁHD�� D�  D�@ D�~�D˾�D���D�@ D�~�D̾�D�  D�B�D̀ D;�D�  D�AHD΁HD�� D�  D�AHDπ D��HD�HD�AHDЁHD��HD��D�@ D�~�D�� D���D�>�DҀ D��HD��D�AHDӀ D�D�HD�AHDԀ DԾ�D���D�>�D�~�Dվ�D�  D�>�D�~�D�� D�HD�@ D�~�D׾�D��qD�=qD�}qDؾ�D���D�>�D�~�D�� D�HD�>�Dڀ D�� D���D�@ DہHD�� D���D�@ D܀ D�� D�  D�AHD�~�D�� D�  D�=qD�~�D�� D���D�>�D߀ D�D�  D�@ D�� D�qD���D�@ D�HD��HD�  D�@ D� D�� D�HD�@ D�HD�� D�  D�@ D� D��HD�HD�@ D� D��HD�HD�B�D� D澸D�  D�>�D�~�D�� D�HD�AHD�HD�� D�  D�@ D� D�qD��qD�@ D�HD��HD�  D�@ D�HD뾸D�  D�AHD�~�D쾸D���D�@ D�HD��HD��D�AHD�~�D�� D�HD�@ D� DﾸD���D�@ D�� D��HD���D�>�D� D��HD��D�AHD�~�D�� D�HD�AHD�HD��HD�  D�AHD�HD��HD���D�@ D���D�D�  D�>�D�~�D���D�  D�AHD��HD��HD�  D�=qD�� D��HD�HD�8RG�O�?\)?B�\?k�?�z�?�Q�?���?�@\)@��@+�@:�H@J=q@\(�@h��@xQ�@�ff@�{@�@��H@��\@��@�33@���@�  @���@У�@�@�p�@��@�{@�@�p�A33AQ�A(�A\)A�
A��Ap�A!G�A%�A*=qA.�RA333A7
=A;�A@��AE�AI��AN�RAS33AX��A\��A`  Ae�Ai��Amp�AqG�Aw
=A|(�A\)A���A�(�A��RA���A��\A���A�\)A���A�33A�p�A��A��A�z�A�ffA���A��\A��A��A���A��A�A���A��HA���A�
=A���A��
A�A�  A�=qA��A�
=A�G�A�33A�A�Q�A�=qA�z�A�ffA���A��HA��A�\)AᙚA�(�A�ffA�Q�A�\A���A�\)A�G�A�33A�p�A�  A�=qA�z�A�ffB (�Bp�B�\B�Bz�B��B
=B(�B	�B
{B33Bz�B��B�RB�B��B�B\)BQ�Bp�B�\B�B�B=qB33BQ�B��B�HB (�B!G�B"ffB#�B$��B&=qB'\)B(z�B)B+
=B,Q�B-p�B.�\B/�
B1G�B2�\B3�B4��B6{B7�B8��B9�B;33B<(�B=��B?
=B@(�BAG�BBffBC�BD��BF=qBG
=BHQ�BIBJ�HBK�
BL��BM�BO33BPz�BQp�BRffBS�BT��BUBV�HBX  BX��BYBZ�HB\  B\��B]B^�RB_�B`��BaBb�\Bc�Bdz�Be��Bf�\Bg�BhQ�Bi�Bj{Bk33BlQ�Bm�Bn{Bn�HBp  Bp��Bq�Bs
=Bt  Bt��Bu��Bv�RBw�Bxz�ByG�By�Bz�\B{\)B|  B|Q�B|��B|��B}�B}p�B}p�B}p�B}G�B}p�B}B}B}��B}��B}B}�B}�B}B}B}B}B}�B}B}��B}��B}��B}��B}B}��B}��B}p�B}p�B}p�B}��B}p�B}p�B}G�B}�B}�B}p�B}G�B}�B|��B|��B|��B|��B|��B|��B|��B|Q�B|Q�B|(�B|(�B|(�B|  B|  B{�B{�B{\)B{\)B{\)B{\)B{
=B{
=Bz�HBz�RBz�\BzffBz�\Bz�\Bz�\BzffBz=qBz=qBzffBzffBz�\Bz�RBz�HBz�HB{
=B{
=B{33B{�B|  B|Q�B|��B|��B}G�B}��B}�B~ffB~�HB�B�(�B�ffB��RB�
=B�\)B���B��B�=qB��\B���B�\)B�B�(�B��\B��HB�G�B��B��B�Q�B���B�33B��B�{B�z�B��HB�\)B�B�(�B��\B���B�\)B�B�=qB���B��B���B�{B�z�B���B�p�B��
B�=qB���B�
=B�\)B��
B�(�B�z�B���B�33B���B�  B�ffB���B��B��B��B�=qB��\B���B�\)B��B�{B�z�B��HB�G�B�B�(�B���B��B��B��B�Q�B���B�G�B���B�  B�ffB��HB�G�B��B�(�B��\B���B�p�B��
B�=qB��RB��B���B�  B�z�B��HB�\)B�B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�ffB���B�G�B�B�=qB��\B���B�p�B��
B�ffB��RB�33B��B�{B�z�B�
=B�p�B��B�Q�B���B�G�B�B�=qB���B��B��B�{B�z�B���B�p�B�  B�Q�B���B�G�B�B�=qB���B��B���B�{B��\B�
=B��B�  B�z�B�
=B�p�B�  B��\B��B���B�(�B¸RB�G�B��B�ffB���Bř�B�(�BƸRB�G�B��
B�ffB�
=BɅB�{Bʣ�B�33B�B�=qB���B�G�B��B�z�B�
=BϮB�=qB��HBхB�{BҸRB�G�B�B�ffB��HB�p�B�{B֣�B�G�B��B؏\B�33B�B�ffB�
=Bۙ�B�=qBܸRB�\)B��Bޣ�B�33B��B�z�B��B�B�=qB��HB�p�B�  B�\B�33B��
B�z�B��B�B�(�B�RB�\)B��B�\B�G�B��B�\B�33B��
B�Q�B��HB�p�B�{B�RB�\)B�  B�\B�33B�B�=qB���B�\)B�  B���B�G�B��
B�z�B�
=B���B�(�B���B��B�{B��RB�\)B��
B�z�B��B�C 33C �C ��C�CffC�C  CG�C��C��CG�C��C�HC(�Cz�C�RC
=CQ�C��C  CQ�C��C�
C�CffC�RC  C\)C��C�C	33C	p�C	C

=C
ffC
�RC  CG�C�\C�HC(�C�C�
C(�Cp�C�RC  CQ�C��C��CG�C��C�HC�Cp�CC{CffC�RC  CG�C��C�HCG�C��C��C=qC�\C�
C(�C�C�
C33C�\C�
C(�Cp�C��C(�Cz�C�
C(�Cz�C��C�Cp�CC�Cp�C��C
=CQ�C��C�C(�Cz�CC  C33CffC�\C�RC�
C  C(�CQ�Cz�C�C��C�C 
=C 33C \)C �C �RC �HC ��C!�C!G�C!p�C!��C!C!�HC"  C"(�C"Q�C"�C"�C"�
C#  C#�C#G�C#p�C#��C#�
C$
=C$33C$Q�C$z�C$��C$��C%
=C%=qC%ffC%�\C%�RC%�HC&{C&G�C&z�C&�C&�HC'
=C'33C'ffC'�\C'��C(  C(=qC(p�C(�\C(C(�C)(�C)ffC)��C)��C)��C*(�C*\)C*��C*��C+
=C+=qC+ffC+��C+��C,
=C,=qC,z�C,�C,�HC-{C-G�C-p�C-��C-�HC.{C.Q�C.�C.�RC.�C/{C/Q�C/�\C/C0  C0=qC0ffC0��C0��C1  C1=qC1p�C1�C1�C2�C2\)C2�C2�RC2�C3�C3\)C3��C3�
C4  C433C4\)C4��C4�
C5{C5G�C5p�C5��C5�
C6
=C6Q�C6�C6�RC6�C7{C7G�C7�C7C8  C8(�C8\)C8�\C8�RC9  C933C9p�C9��C9�
C:  C:(�C:\)C:��C:��C;  C;(�C;\)C;�\C;C<  C<33C<ffC<��C<C<��C=(�C=Q�C=�C=C=��C>(�C>\)C>�\C>�RC>�HC?{C?Q�C?�C?�RC?�HC@
=C@=qC@ffC@��C@��CA
=CA=qCAp�CA��CACA�CB�CBQ�CB�\CBCB�CC{CC=qCCffCC��CC��CD  CD33CDffCD�\CD�RCD�HCE  CE33CEffCE��CE��CE��CF(�CFQ�CFp�CF��CF�
CG
=CG33CGffCG�CG�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�A�A�A�A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�A�A�%A�A�A�A�A�A�A�
=A�JA�JA�  A��HA��yA��
A�ȴA���A�ƨAړuA��AΟ�A��hA��A�x�A��A��A���A�|�A�E�A�+A��^A���A�|�A��-A��7A�ĜA��9A���A�p�A�`BA���A|1Aw"�Ar��Ao�AnJAln�AioAhJAg
=AdA^��AYƨAXȴAWx�AV�+AV1AQ�hAM�-AK�AKp�AIS�ADv�ABjAA��AA�ABbAA�hA@�DA@5?A>�HA<��A:�+A9\)A8v�A7��A6��A6r�A61'A5A5"�A4��A4r�A3�hA2�yA2�A1�A1`BA0  A/��A/O�A.r�A-C�A+�#A+;dA*  A)�
A)G�A(bA&�HA&Q�A%�A$��A$ĜA$~�A$1'A#��A"��A"��A"5?A!hsA �yA jA (�A�;AK�AĜA5?A  A��A��Al�AK�A�yA �A�-A\)A%A�`A�A��A�^AA�uAbNA�A�mA�
A��AVA�RA�AQ�AAƨA��A|�AdZA33A��AE�AA;dA��AM�A�AA�;A��A�A�AbAl�A�!A9XA�A��A|�A%A�jA��A��A�uA^5A  A�PAoA
�A
��A
�A
��A
ffA
I�A
�A	��A	hsA	�A�!AM�A�A�wA�Ap�AO�AA��A1'AƨA�hAt�AC�A�/AA�A�TA|�AS�A33A�A�yA�jA��A9XA�-AK�AA ��A E�@��w@���@�-@��@�O�@��`@�I�@�S�@�v�@�{@�V@�Z@�t�@��T@��/@�Z@�S�@��H@�v�@���@�7@�O�@��/@�r�@���@�S�@���@�V@�@��/@�j@�D@�I�@�
=@�E�@�V@�^5@��@陚@���@�w@��@柾@�`B@䛦@�33@�v�@�$�@�p�@��@��@�z�@ߕ�@ޟ�@�E�@��@�@ݡ�@݁@�p�@�hs@�O�@�V@۝�@�ȴ@٩�@ف@�G�@�z�@�I�@�  @ץ�@�@�V@�&�@�(�@ӝ�@��@�E�@�@Ѳ-@�/@���@Ь@�9X@ϥ�@��@�ff@�5?@���@�hs@���@�r�@��m@˥�@�o@ʗ�@�M�@���@�O�@��@ț�@�I�@ǶF@�
=@Ɵ�@�-@��T@�`B@Ĵ9@�  @�K�@�o@���@\@�=q@��@�/@��D@���@�t�@��y@�E�@��^@�G�@��@�1@�dZ@�ȴ@�$�@��#@�@�x�@��/@��u@�z�@�j@��@��
@�|�@�"�@��@�n�@��@��h@�?}@���@���@�j@��@�|�@�S�@�C�@�
=@�ȴ@�$�@�x�@�?}@���@��u@�Z@�1'@��@���@�l�@�o@���@�5?@���@��7@�p�@�V@�Ĝ@�Z@�1@��F@�S�@��@��y@��\@�ff@�J@��T@�hs@���@��@��F@�t�@���@��+@��@��T@��-@�`B@�7L@���@�A�@�1'@��@�\)@�
=@��@��+@��@��T@�hs@��`@��@���@���@�j@��@���@�"�@��!@�M�@��@��@���@��^@��@���@�j@��@�\)@�
=@���@�E�@��@���@���@�x�@�V@���@�Q�@���@��
@�|�@���@��\@�=q@��@��@�@�hs@�7L@�%@��@���@��u@�Q�@�1@�ƨ@���@��P@�S�@�o@��@�ȴ@���@��+@�5?@�{@��T@��-@��@��@�(�@���@��F@��@��@��H@���@��\@�E�@�J@��^@���@��7@�X@�G�@�O�@�?}@��@���@��u@�Z@� �@��@���@��P@�S�@�33@�"�@���@��@��@���@�$�@��@�p�@�G�@��@�z�@�Z@� �@�ƨ@�|�@�\)@�;d@�"�@�@���@�n�@�M�@��@��#@���@��7@�O�@�7L@��@�Ĝ@�A�@��
@�|�@�S�@��@�
=@��y@���@�5?@��@��@��@���@��h@�X@�G�@�?}@�/@��@��@���@��j@��j@���@�j@�9X@�(�@� �@�@\)@�@~$�@}�T@}�@|��@|�D@|�@{��@z�@z�\@z=q@y��@yhs@yG�@x��@w�;@v�@u�@t�j@t�@s��@s33@r�!@r^5@q�#@q�^@q��@qx�@q&�@pr�@o�w@o+@n��@n��@n�+@nv�@nV@n$�@m@m�@l�@l�@l�D@lj@lI�@k��@kdZ@j�@j�\@j�@i��@iX@hĜ@hb@g��@g\)@f��@e@eO�@e�@d��@d��@d�/@d��@dj@c��@b��@b�!@bM�@`�`@`�@`Q�@`b@_�w@_l�@^��@^�R@^�R@^��@^v�@^5?@]�-@\�@\�@\j@\9X@[�F@[33@[@Z��@Y�@Y%@X�@XA�@W��@W|�@Wl�@W|�@V�+@U��@U`B@T��@T��@TI�@S��@S�F@St�@R�@Rn�@R-@Q�^@Qhs@QG�@Q�@P�u@Pb@O|�@O;d@N��@N��@N{@M��@L��@LI�@Kƨ@K��@KS�@K33@J�H@J��@J^5@J�@I�#@I��@Ihs@I&�@H�9@Hb@G��@G��@G;d@G
=@F��@F$�@E�@E��@E�@D��@Dj@D1@C�@CS�@C"�@C@B�@B��@B��@B-@A��@A&�@A%@@�`@@��@@�@@ �@?��@?\)@>�+@>5?@=��@=?}@<��@<�/@<��@<j@<(�@;��@;�F@;"�@;@:��@:M�@9�#@9�^@9��@9x�@9G�@9�@8��@8�u@8A�@8 �@7�@7K�@7;d@6��@6��@6{@5��@5?}@4�/@4�@4�D@4�@3C�@3@2��@1��@1&�@0Ĝ@0�9@0�9@0�9@0�9@0��@0�u@0bN@0  @/�P@/\)@.�@.5?@.@-�@-@-p�@,�@,�@,�D@,z�@,Z@,�@+ƨ@+�@+C�@+33@+o@*�H@*�H@*�H@*��@*�\@*^5@*=q@*J@)�#@)�7@)&�@(�`@(Ĝ@(r�@(1'@(b@'�;@'�w@'��@';d@&�y@&��@&�+@&V@&{@&@%�T@%�h@%`B@%V@$��@$��@$j@$9X@$1@#�m@#ƨ@#��@#o@"�\@"-@"�@!�@!��@ ��@ r�@  �@��@|�@K�@;d@
=@ȴ@��@��@�@�h@O�@�@��@�/@�D@9X@(�@��@�m@ƨ@��@��@�@C�@�@��@~�@~�@~�@-@�@�#@�^@��@hs@G�@G�@�`@�@bN@A�@ �@b@  @��@��@K�@�@
=@�y@ȴ@��@E�@5?@5?@{@�-@�h@p�@`B@`B@/@�@�@�@V@��@�@��@�D@j@j@Z@9X@��@��@��@dZ@S�@C�@C�@33@33@"�@@��@�\@~�@=q@J@��@��@x�@G�@�@�`@��@�@bN@Q�@A�@A�@ �@b@�@��@�w@�P@K�@�y@�@��@��@�+@v�@ff@V@{@��@��@p�@O�@/@�/@z�@Z@Z@Z@9X@1@��@��A�1A�A�A�1A�%A�  A�%A�1A�A�  A�A�1A�A�  A�A�A�A�A�A�%A�%A�A�A�%A�1A�A�A�A�1A�%A�A�A�%A�%A�A�A�A�%A�A�A�A�1A�%A�A�A�1A�1A�A�A�%A�1A�1A�A�A�
=A�1A�A�%A�
=A�A�A�A�1A�1A�%A�1A�
=A�
=A�
=A�%A�1A�
=A�JA�1A�%A�%A�
=A�
=A�
=A�%A�A�%A�%A�A�A�%A�%A�A�  A�A�A�%A�A�A�A�%A�%A�A�A�1A�1A�A�A�A�%A�%A�A�  A�A�%A�A�A�A�A�A�A���A�  A�A�%A�A�  A�A�A�A�A�  A�  A�A�A�A�  A�  A�A�A�A�  A�A�A�%A�%A�A�A�A�
=A�JA�JA�1A�
=A�JA�VA�JA�
=A�
=A�VA�bA�JA�JA�JA�bA�JA�JA�JA�
=A�VA�bA�1A��yA��HA��TA��`A��HA��/A��/A��;A��A��mA��A��A��A��HA��yA��yA��TA��;A��/A���A���A���A���A���A���A���A�A���A�ĜA�ĜA�ĜA��/A��`A���A���A�Aں^A�ĜA�ȴA��A��
A���A�ȴAں^AڸRAھwAھwA�AڼjAڸRAڶFAڸRAڥ�Aڟ�A�M�AٮA�=qA���A׍PA�I�A�A�|�A�+Aԏ\A�`BA�-A���A�%A�XA�?}A��A��yA��A�"�A� �A���AѬAя\Aљ�Aѣ�AѮAщ7A�|�A�p�A�hsA�G�A�;dA��Aк^AХ�AЗ�A�l�A�C�A���A���A�ȴA�ȴA���Aχ+A�l�AύPA�v�A�I�A�JA���A���A�  A�
=A�VA�JA�A��A���A���A��A��mA��A���A���A���A���A�
=A�oA��A� �A�/A�?}A�M�A�S�A�^5A�S�A�VA�hsAσAϋDAρAχ+AυA�^5A�VA�;dA�&�A�(�A�bA��A��mA���A�7LA�oA���AͲ-A̙�A�-A���A�p�A�33A�9XA�M�AȰ!AǁA�^5A�?}A�dZA�r�A��A�
=A��wA�v�A�ȴA�1'A�jA�r�A�z�A�bNA�ƨA�~�A�VA�A�A�7LA�-A��A�A��;A�ĜA���A�x�A�S�A�(�A��A���A��hA�XA�1'A��mA���A�S�A���A�p�A�1'A���A���A��jA��!A���A���A��\A�z�A�Q�A�{A���A���A��A�l�A�\)A���A��FA���A���A�^5A��A�x�A���A�\)A�VA��9A�ffA��A�K�A�{A���A���A��A��^A���A��A�n�A�5?A���A�+A��`A�A�hsA�O�A�-A� �A���A�ĜA�ZA�=qA�9XA�A��`A��DA�p�A�^5A�M�A��A���A��;A��jA�|�A�^5A�oA���A��A��\A��A�|�A�M�A�VA�O�A��A��A���A��wA���A��PA�~�A�l�A�O�A�+A�A���A��DA��A�1A�VA��A�
=A�-A�S�A���A�?}A���A�E�A�+A��A���A���A�n�A�?}A��A��wA�&�A�p�A�ƨA��A���A���A��+A��
A�v�A�  A�t�A��#A�{A��A�n�A�$�A��A��HA�ĜA��A���A�l�A�^5A�C�A��A���A�\)A�%A�ȴA��PA�A�A���A�ĜA�G�A��9A���A���A�+A��\AhsA}��A|�RA|��A|z�A{�A{"�Az��AzffAy�#Ayp�Aw��Au�Ast�AsXAsG�As�ArȴArffArI�Ar9XAq�mAqG�Ap�RAp(�Ao�Ao
=An��An�9An�+AnZAn{Am�FAm�7AmdZAm�AmoAl�yAljAl �Ak�TAjĜAi�Ai`BAi/Ah�Ah��AhbNAhM�Ah �AhJAg��Ag�Ag�Ag�Ag�Ag�TAg�7Af�`Afn�Af�Ae�TAet�Ad��AdbNAc�FAb�\Aa�Aa��A`�A_�A]��A]%A\�A[��A[G�AZz�AY�TAY|�AX�jAX��AX�9AX�AY"�AY+AXȴAXr�AX{AW�
AW�-AW�PAWl�AWC�AW�AV�AV�jAV�\AVn�AVZAVVAVVAVM�AVA�AV9XAV1'AVbAU�
AUATbNARQ�AQ��AQ+AP��AP$�AN��ANbNAN-AN�AM��AM+AL�RALZAL(�AL1AK�AK�#AKAK�-AK��AK��AK�AKx�AKXAK�AJ�jAJQ�AIAI\)AH��AH�+AGp�AFbAE�hAD�!AC�wACAB��AB�AB�ABn�AB^5ABM�AB=qAB�ABJAA�AAAA;dA@��A@�RA@��A@�yA@��AAVAA��AA��AA��AA�;AA��AB-ABn�AB�AB�\AB~�ABE�AA�mAAhsAA
=A@�HA@��A@��A@�9A@�A@��A@��A@��A@��A@�\A@�+A@~�A@v�A@n�A@jA@ffA@bNA@VA@M�A@I�A@=qA@9XA@5?A@5?A@-A@-A@ �A@�A@ �A@{A?�A?�hA?l�A?
=A>��A>�A>��A>�\A>n�A> �A=�TA=�FA=t�A=O�A=�A<��A<�A<��A<��A<Q�A<1'A;��A;�^A;XA:�HA:��A:z�A:bNA:A�A:5?A:(�A:A9�;A9��A9�wA9�FA9x�A9K�A9/A9oA8��A8�A8�HA8�/A8ȴA8�A8~�A8Q�A8A�A89XA8-A8 �A8bA7��A7�TA7��A7��A7�A7l�A7`BA733A7oA7%A6��A6�`A6�A6��A6ĜA6�RA6��A6�DA6~�A6v�A6r�A6r�A6v�A6r�A6r�A6r�A6jA6^5A6Q�A6A�A6=qA69XA65?A6-A6 �A6�A6{A5��A5�mA5�;A5��A5A5�FA5�A5��A5��A5��A5�A5x�A5`BA5C�A5/A5
=A4�A4��A4��A4�RA4�!A4�A4��A4��A4��A4��A4��A4��A4��A4��A4��A4�uA4�+A4z�A4r�A4jA4ffA4^5A4M�A4-A4bA3�A3�wA3��A3|�A3p�A3\)A3O�A3K�A3;dA333A3/A3&�A3"�A3�A3�A2��A2��A2ffA2A�A25?A2-A2(�A2(�A2$�A2�A2�A2JA2JA2A2A2  A1��A1��A1�A1�A1�A1�mA1�mA1�mA1�TA1�#A1�
A1��A1ƨA1�wA1�-A1��A1p�A1"�A0��A0�A0�\A0ffA0=qA0�A0JA0A/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  A�%A�A�A�A�A�A�A�A�A�A�%A�%A�%A�%A�1A�1A�A�A�%A�A�A�A�A�A�A�
=A�JA�JA�  A��HA��yA��
A�ȴA���A�ƨAړuA��AΟ�A��hA��A�x�A��A��A���A�|�A�E�A�+A��^A���A�|�A��-A��7A�ĜA��9A���A�p�A�`BA���A|1Aw"�Ar��Ao�AnJAln�AioAhJAg
=AdA^��AYƨAXȴAWx�AV�+AV1AQ�hAM�-AK�AKp�AIS�ADv�ABjAA��AA�ABbAA�hA@�DA@5?A>�HA<��A:�+A9\)A8v�A7��A6��A6r�A61'A5A5"�A4��A4r�A3�hA2�yA2�A1�A1`BA0  A/��A/O�A.r�A-C�A+�#A+;dA*  A)�
A)G�A(bA&�HA&Q�A%�A$��A$ĜA$~�A$1'A#��A"��A"��A"5?A!hsA �yA jA (�A�;AK�AĜA5?A  A��A��Al�AK�A�yA �A�-A\)A%A�`A�A��A�^AA�uAbNA�A�mA�
A��AVA�RA�AQ�AAƨA��A|�AdZA33A��AE�AA;dA��AM�A�AA�;A��A�A�AbAl�A�!A9XA�A��A|�A%A�jA��A��A�uA^5A  A�PAoA
�A
��A
�A
��A
ffA
I�A
�A	��A	hsA	�A�!AM�A�A�wA�Ap�AO�AA��A1'AƨA�hAt�AC�A�/AA�A�TA|�AS�A33A�A�yA�jA��A9XA�-AK�AA ��A E�@��w@���@�-@��@�O�@��`@�I�@�S�@�v�@�{@�V@�Z@�t�@��T@��/@�Z@�S�@��H@�v�@���@�7@�O�@��/@�r�@���@�S�@���@�V@�@��/@�j@�D@�I�@�
=@�E�@�V@�^5@��@陚@���@�w@��@柾@�`B@䛦@�33@�v�@�$�@�p�@��@��@�z�@ߕ�@ޟ�@�E�@��@�@ݡ�@݁@�p�@�hs@�O�@�V@۝�@�ȴ@٩�@ف@�G�@�z�@�I�@�  @ץ�@�@�V@�&�@�(�@ӝ�@��@�E�@�@Ѳ-@�/@���@Ь@�9X@ϥ�@��@�ff@�5?@���@�hs@���@�r�@��m@˥�@�o@ʗ�@�M�@���@�O�@��@ț�@�I�@ǶF@�
=@Ɵ�@�-@��T@�`B@Ĵ9@�  @�K�@�o@���@\@�=q@��@�/@��D@���@�t�@��y@�E�@��^@�G�@��@�1@�dZ@�ȴ@�$�@��#@�@�x�@��/@��u@�z�@�j@��@��
@�|�@�"�@��@�n�@��@��h@�?}@���@���@�j@��@�|�@�S�@�C�@�
=@�ȴ@�$�@�x�@�?}@���@��u@�Z@�1'@��@���@�l�@�o@���@�5?@���@��7@�p�@�V@�Ĝ@�Z@�1@��F@�S�@��@��y@��\@�ff@�J@��T@�hs@���@��@��F@�t�@���@��+@��@��T@��-@�`B@�7L@���@�A�@�1'@��@�\)@�
=@��@��+@��@��T@�hs@��`@��@���@���@�j@��@���@�"�@��!@�M�@��@��@���@��^@��@���@�j@��@�\)@�
=@���@�E�@��@���@���@�x�@�V@���@�Q�@���@��
@�|�@���@��\@�=q@��@��@�@�hs@�7L@�%@��@���@��u@�Q�@�1@�ƨ@���@��P@�S�@�o@��@�ȴ@���@��+@�5?@�{@��T@��-@��@��@�(�@���@��F@��@��@��H@���@��\@�E�@�J@��^@���@��7@�X@�G�@�O�@�?}@��@���@��u@�Z@� �@��@���@��P@�S�@�33@�"�@���@��@��@���@�$�@��@�p�@�G�@��@�z�@�Z@� �@�ƨ@�|�@�\)@�;d@�"�@�@���@�n�@�M�@��@��#@���@��7@�O�@�7L@��@�Ĝ@�A�@��
@�|�@�S�@��@�
=@��y@���@�5?@��@��@��@���@��h@�X@�G�@�?}@�/@��@��@���@��j@��j@���@�j@�9X@�(�@� �@�@\)@�@~$�@}�T@}�@|��@|�D@|�@{��@z�@z�\@z=q@y��@yhs@yG�@x��@w�;@v�@u�@t�j@t�@s��@s33@r�!@r^5@q�#@q�^@q��@qx�@q&�@pr�@o�w@o+@n��@n��@n�+@nv�@nV@n$�@m@m�@l�@l�@l�D@lj@lI�@k��@kdZ@j�@j�\@j�@i��@iX@hĜ@hb@g��@g\)@f��@e@eO�@e�@d��@d��@d�/@d��@dj@c��@b��@b�!@bM�@`�`@`�@`Q�@`b@_�w@_l�@^��@^�R@^�R@^��@^v�@^5?@]�-@\�@\�@\j@\9X@[�F@[33@[@Z��@Y�@Y%@X�@XA�@W��@W|�@Wl�@W|�@V�+@U��@U`B@T��@T��@TI�@S��@S�F@St�@R�@Rn�@R-@Q�^@Qhs@QG�@Q�@P�u@Pb@O|�@O;d@N��@N��@N{@M��@L��@LI�@Kƨ@K��@KS�@K33@J�H@J��@J^5@J�@I�#@I��@Ihs@I&�@H�9@Hb@G��@G��@G;d@G
=@F��@F$�@E�@E��@E�@D��@Dj@D1@C�@CS�@C"�@C@B�@B��@B��@B-@A��@A&�@A%@@�`@@��@@�@@ �@?��@?\)@>�+@>5?@=��@=?}@<��@<�/@<��@<j@<(�@;��@;�F@;"�@;@:��@:M�@9�#@9�^@9��@9x�@9G�@9�@8��@8�u@8A�@8 �@7�@7K�@7;d@6��@6��@6{@5��@5?}@4�/@4�@4�D@4�@3C�@3@2��@1��@1&�@0Ĝ@0�9@0�9@0�9@0�9@0��@0�u@0bN@0  @/�P@/\)@.�@.5?@.@-�@-@-p�@,�@,�@,�D@,z�@,Z@,�@+ƨ@+�@+C�@+33@+o@*�H@*�H@*�H@*��@*�\@*^5@*=q@*J@)�#@)�7@)&�@(�`@(Ĝ@(r�@(1'@(b@'�;@'�w@'��@';d@&�y@&��@&�+@&V@&{@&@%�T@%�h@%`B@%V@$��@$��@$j@$9X@$1@#�m@#ƨ@#��@#o@"�\@"-@"�@!�@!��@ ��@ r�@  �@��@|�@K�@;d@
=@ȴ@��@��@�@�h@O�@�@��@�/@�D@9X@(�@��@�m@ƨ@��@��@�@C�@�@��@~�@~�@~�@-@�@�#@�^@��@hs@G�@G�@�`@�@bN@A�@ �@b@  @��@��@K�@�@
=@�y@ȴ@��@E�@5?@5?@{@�-@�h@p�@`B@`B@/@�@�@�@V@��@�@��@�D@j@j@Z@9X@��@��@��@dZ@S�@C�@C�@33@33@"�@@��@�\@~�@=q@J@��@��@x�@G�@�@�`@��@�@bN@Q�@A�@A�@ �@b@�@��@�w@�P@K�@�y@�@��@��@�+@v�@ff@V@{@��@��@p�@O�@/@�/@z�@Z@Z@Z@9X@1@��G�O�A�1A�A�A�1A�%A�  A�%A�1A�A�  A�A�1A�A�  A�A�A�A�A�A�%A�%A�A�A�%A�1A�A�A�A�1A�%A�A�A�%A�%A�A�A�A�%A�A�A�A�1A�%A�A�A�1A�1A�A�A�%A�1A�1A�A�A�
=A�1A�A�%A�
=A�A�A�A�1A�1A�%A�1A�
=A�
=A�
=A�%A�1A�
=A�JA�1A�%A�%A�
=A�
=A�
=A�%A�A�%A�%A�A�A�%A�%A�A�  A�A�A�%A�A�A�A�%A�%A�A�A�1A�1A�A�A�A�%A�%A�A�  A�A�%A�A�A�A�A�A�A���A�  A�A�%A�A�  A�A�A�A�A�  A�  A�A�A�A�  A�  A�A�A�A�  A�A�A�%A�%A�A�A�A�
=A�JA�JA�1A�
=A�JA�VA�JA�
=A�
=A�VA�bA�JA�JA�JA�bA�JA�JA�JA�
=A�VA�bA�1A��yA��HA��TA��`A��HA��/A��/A��;A��A��mA��A��A��A��HA��yA��yA��TA��;A��/A���A���A���A���A���A���A���A�A���A�ĜA�ĜA�ĜA��/A��`A���A���A�Aں^A�ĜA�ȴA��A��
A���A�ȴAں^AڸRAھwAھwA�AڼjAڸRAڶFAڸRAڥ�Aڟ�A�M�AٮA�=qA���A׍PA�I�A�A�|�A�+Aԏ\A�`BA�-A���A�%A�XA�?}A��A��yA��A�"�A� �A���AѬAя\Aљ�Aѣ�AѮAщ7A�|�A�p�A�hsA�G�A�;dA��Aк^AХ�AЗ�A�l�A�C�A���A���A�ȴA�ȴA���Aχ+A�l�AύPA�v�A�I�A�JA���A���A�  A�
=A�VA�JA�A��A���A���A��A��mA��A���A���A���A���A�
=A�oA��A� �A�/A�?}A�M�A�S�A�^5A�S�A�VA�hsAσAϋDAρAχ+AυA�^5A�VA�;dA�&�A�(�A�bA��A��mA���A�7LA�oA���AͲ-A̙�A�-A���A�p�A�33A�9XA�M�AȰ!AǁA�^5A�?}A�dZA�r�A��A�
=A��wA�v�A�ȴA�1'A�jA�r�A�z�A�bNA�ƨA�~�A�VA�A�A�7LA�-A��A�A��;A�ĜA���A�x�A�S�A�(�A��A���A��hA�XA�1'A��mA���A�S�A���A�p�A�1'A���A���A��jA��!A���A���A��\A�z�A�Q�A�{A���A���A��A�l�A�\)A���A��FA���A���A�^5A��A�x�A���A�\)A�VA��9A�ffA��A�K�A�{A���A���A��A��^A���A��A�n�A�5?A���A�+A��`A�A�hsA�O�A�-A� �A���A�ĜA�ZA�=qA�9XA�A��`A��DA�p�A�^5A�M�A��A���A��;A��jA�|�A�^5A�oA���A��A��\A��A�|�A�M�A�VA�O�A��A��A���A��wA���A��PA�~�A�l�A�O�A�+A�A���A��DA��A�1A�VA��A�
=A�-A�S�A���A�?}A���A�E�A�+A��A���A���A�n�A�?}A��A��wA�&�A�p�A�ƨA��A���A���A��+A��
A�v�A�  A�t�A��#A�{A��A�n�A�$�A��A��HA�ĜA��A���A�l�A�^5A�C�A��A���A�\)A�%A�ȴA��PA�A�A���A�ĜA�G�A��9A���A���A�+A��\AhsA}��A|�RA|��A|z�A{�A{"�Az��AzffAy�#Ayp�Aw��Au�Ast�AsXAsG�As�ArȴArffArI�Ar9XAq�mAqG�Ap�RAp(�Ao�Ao
=An��An�9An�+AnZAn{Am�FAm�7AmdZAm�AmoAl�yAljAl �Ak�TAjĜAi�Ai`BAi/Ah�Ah��AhbNAhM�Ah �AhJAg��Ag�Ag�Ag�Ag�Ag�TAg�7Af�`Afn�Af�Ae�TAet�Ad��AdbNAc�FAb�\Aa�Aa��A`�A_�A]��A]%A\�A[��A[G�AZz�AY�TAY|�AX�jAX��AX�9AX�AY"�AY+AXȴAXr�AX{AW�
AW�-AW�PAWl�AWC�AW�AV�AV�jAV�\AVn�AVZAVVAVVAVM�AVA�AV9XAV1'AVbAU�
AUATbNARQ�AQ��AQ+AP��AP$�AN��ANbNAN-AN�AM��AM+AL�RALZAL(�AL1AK�AK�#AKAK�-AK��AK��AK�AKx�AKXAK�AJ�jAJQ�AIAI\)AH��AH�+AGp�AFbAE�hAD�!AC�wACAB��AB�AB�ABn�AB^5ABM�AB=qAB�ABJAA�AAAA;dA@��A@�RA@��A@�yA@��AAVAA��AA��AA��AA�;AA��AB-ABn�AB�AB�\AB~�ABE�AA�mAAhsAA
=A@�HA@��A@��A@�9A@�A@��A@��A@��A@��A@�\A@�+A@~�A@v�A@n�A@jA@ffA@bNA@VA@M�A@I�A@=qA@9XA@5?A@5?A@-A@-A@ �A@�A@ �A@{A?�A?�hA?l�A?
=A>��A>�A>��A>�\A>n�A> �A=�TA=�FA=t�A=O�A=�A<��A<�A<��A<��A<Q�A<1'A;��A;�^A;XA:�HA:��A:z�A:bNA:A�A:5?A:(�A:A9�;A9��A9�wA9�FA9x�A9K�A9/A9oA8��A8�A8�HA8�/A8ȴA8�A8~�A8Q�A8A�A89XA8-A8 �A8bA7��A7�TA7��A7��A7�A7l�A7`BA733A7oA7%A6��A6�`A6�A6��A6ĜA6�RA6��A6�DA6~�A6v�A6r�A6r�A6v�A6r�A6r�A6r�A6jA6^5A6Q�A6A�A6=qA69XA65?A6-A6 �A6�A6{A5��A5�mA5�;A5��A5A5�FA5�A5��A5��A5��A5�A5x�A5`BA5C�A5/A5
=A4�A4��A4��A4�RA4�!A4�A4��A4��A4��A4��A4��A4��A4��A4��A4��A4�uA4�+A4z�A4r�A4jA4ffA4^5A4M�A4-A4bA3�A3�wA3��A3|�A3p�A3\)A3O�A3K�A3;dA333A3/A3&�A3"�A3�A3�A2��A2��A2ffA2A�A25?A2-A2(�A2(�A2$�A2�A2�A2JA2JA2A2A2  A1��A1��A1�A1�A1�A1�mA1�mA1�mA1�TA1�#A1�
A1��A1ƨA1�wA1�-A1��A1p�A1"�A0��A0�A0�\A0ffA0=qA0�A0JA0A/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	P}B	P�B	P�B	P}B	PHB	P}B	P�B	P}B	P}B	PHB	PHB	PHB	PHB	P}B	PHB	P}B	PHB	PHB	PHB	PB	P}B	PHB	P}B	P}B	P}B	PHB	PHB	PB	QNB	Q�B	QNB	R�B	R�B	QNB	O�B	N�B	�_B	��B
<B
;�B
G�B
\)B
T�B
WsB
��B(�B#�BB
��B
�B
�KB
�:B
}"B
2�B	��B	�gB	�3B	��B	oiB	d�B	D3B	?HB	1[B	*�B	&�B	�B	!�B	�B	�B��B	�B	!�B	CB	�B	DB�oB�B�HB��B��BѷB��B��B	hB	IRB	\�B	^B	h>B	poB	�4B	�DB	��B	�$B	��B	��B	�bB	��B	��B	�'B	�9B	�B	B	�B	�B	ΥB	�)B	��B	�yB	��B
B
	�B
~B
�B
�B
oB
!B
"�B
%�B
)�B
,B
-�B
/�B
0�B
2�B
0�B
1�B
6FB
5B
4B
3�B
49B
5?B
4nB
6B
5�B
5B
4�B
4�B
4�B
4�B
6FB
7LB
7�B
7�B
7�B
6�B
7�B
9�B
9�B
=B
=B
=<B
>B
>BB
>BB
?}B
C-B
CaB
D3B
D�B
F?B
F?B
F?B
EmB
D�B
D3B
D�B
H�B
I�B
JXB
K)B
K�B
K�B
K^B
K�B
K�B
J�B
L0B
MB
K�B
K^B
J�B
K)B
K^B
K�B
K�B
J#B
I�B
IRB
H�B
IB
HB
H�B
GEB
F�B
GB
F�B
F�B
F�B
E�B
FB
FB
D�B
D�B
D�B
C-B
B'B
B�B
@�B
@�B
A B
@�B
@�B
?}B
?HB
>BB
>B
=<B
<�B
:�B
:^B
:*B
9�B
9�B
9$B
8�B
8B
7�B
7�B
7B
5�B
4�B
49B
2�B
2-B
1'B
0�B
0!B
/�B
.�B
-�B
.B
,=B
+�B
,B
)�B
*0B
(�B
&�B
&B
%zB
$B
$B
#:B
"�B
"4B
!�B
 �B
 \B
!B
�B
�B
�B
IB
B
xB
�B
xB
�B
�B
�B
B
�B
�B
�B
�B
�B
$B
�B
SB
�B
B
FB
uB
�B
B
�B
�B
B
�B
�B
uB
@B
�B
oB
�B
�B
oB
hB
�B
�B
.B
bB
�B
bB
�B
.B
�B
\B
�B
VB
"B
VB
PB
�B
�B
PB
�B
�B
�B
�B
B
�B
�B
�B
B
�B
B
DB
B
JB
~B
�B
�B
�B
�B
PB
�B
PB
�B
�B
�B
�B
�B
(B
VB
"B
�B
�B
VB
"B
�B
�B
VB
�B
B
B
�B
�B
�B
�B
VB
�B
�B
"B
"B
�B
(B
�B
�B
�B
\B
�B
�B
�B
�B
.B
�B
 B
.B
�B
 B
�B
hB
oB
�B
�B
�B
�B
oB
�B
oB
@B
�B
B
B
B
�B
�B
�B
�B
FB
FB
FB
FB
{B
�B
B
B
�B
�B
�B
B
�B
�B
�B
�B
$B
�B
_B
+B
_B
1B
1B
�B
�B
eB
�B
eB
kB
�B
kB
�B
=B
qB
=B
�B
B
B
�B
�B
�B
�B
xB
CB
CB
B
B
B
B
B
�B
�B
�B
OB
�B
�B
OB
VB
VB
!B
!B
�B
!B
�B
�B
�B
�B
�B
 \B
�B
 \B
 �B
!bB
!�B
!�B
!�B
"�B
"�B
"�B
#B
#:B
#B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&LB
&�B
&�B
'B
'�B
)�B
)*B
)_B
)_B
*eB
*�B
+B
+B
+6B
+kB
+6B
+kB
+6B
+6B
,B
+�B
,B
+�B
+�B
+�B
+�B
,B
,=B
,=B
,�B
,�B
,�B
-CB
-CB
-�B
.�B
.�B
/B
/B
/OB
/B
/B
/�B
0!B
0!B
0�B
0!B
0�B
1'B
1'B
1[B
2-B
2aB
2aB
2-B
2-B
1�B
2-B
2-B
1�B
1�B
2-B
2aB
2�B
2�B
2aB
2�B
2�B
33B
2�B
33B
33B
3�B
3�B
3�B
4B
4�B
5tB
7LB
7�B
8B
8B
8RB
8B
8B
8RB
8RB
8RB
8�B
8B
8B
7�B
7�B
7�B
7�B
7�B
8B
7�B
7�B
8�B
8�B
9XB
9�B
:*B
:�B
:*B
;dB
:�B
;0B
;�B
;0B
:�B
;0B
<6B
;�B
=qB
=�B
>BB
>BB
?B
>�B
?B
?}B
?HB
?HB
?HB
?}B
@B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
AUB
B[B
B'B
B'B
B'B
A�B
B'B
B�B
B�B
B�B
CaB
C�B
CaB
C�B
C�B
C�B
C�B
EB
EB
EB
E9B
EB
EB
EB
EB
D�B
F?B
E�B
EmB
FtB
GEB
F�B
F�B
F�B
GB
GEB
GzB
G�B
GEB
GEB
GzB
G�B
HKB
IB
I�B
J#B
JXB
J�B
K)B
J�B
J�B
K)B
K�B
K^B
K�B
K^B
K^B
K^B
K)B
LdB
K^B
K�B
K^B
L0B
K�B
LdB
LdB
LdB
L�B
M6B
M6B
M�B
NB
M�B
N<B
N<B
N�B
OvB
OBB
O�B
O�B
PB
P�B
QNB
RTB
R�B
R�B
S[B
S[B
S�B
T,B
T�B
T�B
T�B
U2B
UgB
U�B
V9B
W
B
V�B
W
B
W
B
WsB
W�B
W�B
WsB
W�B
W�B
XB
XB
XEB
X�B
X�B
X�B
YB
YB
YB
YKB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[�B
\]B
\]B
\]B
\�B
\�B
\�B
]/B
]dB
]dB
]dB
]�B
^5B
^B
^jB
^�B
_;B
_B
_;B
_;B
_pB
_�B
_�B
_�B
`B
`BB
`vB
`vB
`vB
`�B
aHB
a|B
a�B
bNB
bNB
b�B
bNB
b�B
cTB
b�B
c�B
d&B
d�B
e,B
e,B
e,B
e,B
d�B
e,B
d�B
e`B
e�B
e�B
e�B
f2B
gB
gB
gB
g8B
g�B
h>B
h>B
h�B
h�B
h�B
h�B
iDB
iyB
jB
jB
i�B
jKB
jB
jB
jKB
jKB
j�B
jB
j�B
j�B
j�B
j�B
kB
kB
k�B
k�B
l"B
lWB
l"B
l�B
l�B
m)B
m]B
l�B
m]B
m]B
m)B
m]B
m�B
m�B
n/B
n�B
n�B
n�B
o5B
o�B
o�B
pB
o�B
p;B
p�B
poB
p;B
p;B
poB
qvB
qvB
rB
rGB
r�B
r�B
r�B
r�B
s�B
tB
s�B
s�B
s�B
tB
t�B
t�B
u%B
u�B
u�B
u�B
v�B
v+B
v�B
v�B
v�B
w2B
w�B
w�B
x�B
xB
x8B
w�B
y>B
y	B
y	B
yrB
y	B
zB
y�B
y>B
zDB
{JB
zxB
z�B
{B
z�B
{B
{�B
{�B
{B
{B
{B
{JB
{�B
|B
|�B
|PB
{�B
{B
{�B
|B
}"B
}"B
|�B
|�B
}VB
}VB
|�B
|�B
}�B
}VB
}VB
}�B
~(B
}�B
~(B
~(B
~]B
~]B
~]B
.B
~�B
.B
~�B
.B
~�B
.B
.B
cB
cB
cB
cB
.B
�B
�B
�B
cB
� B
�iB
��B
�B
��B
�B
�B
�B
�;B
�;B
�;B
�oB
�oB
�oB
�B
�uB
��B
��B
��B
�GB
�B
�B
�GB
��B
�B
��B
��B
��B
��B
�B
��B
��B
��B
�SB
��B
��B
�%B
�B	PB	R B	P�B	OvB	P�B	Q�B	PB	O�B	P}B	Q�B	P�B	O�B	P}B	P�B	P}B	PB	OvB	QB	QB	OvB	OB	Q�B	QNB	O�B	OBB	QNB	Q�B	P�B	OBB	PB	QB	QB	O�B	O�B	QB	QNB	P�B	OBB	P�B	QB	QB	OBB	PB	QNB	QNB	OvB	OvB	QNB	P�B	P�B	OvB	O�B	Q�B	QB	N�B	PB	Q�B	PHB	OBB	P�B	Q�B	P�B	OvB	O�B	QB	P�B	O�B	OBB	PB	Q�B	QNB	O�B	OvB	P}B	QNB	QB	PB	O�B	PB	Q�B	P�B	OBB	OvB	P�B	PHB	O�B	OBB	P}B	QB	P�B	O�B	OB	P}B	R�B	P}B	OBB	PB	QNB	P}B	N�B	O�B	P}B	QNB	PHB	OvB	OvB	PHB	QNB	P}B	OB	P�B	QB	P}B	O�B	OvB	P}B	QNB	QNB	PHB	OB	O�B	QNB	QB	PB	PHB	P�B	QB	QB	O�B	PHB	O�B	Q�B	QNB	P�B	P�B	O�B	QB	QB	P�B	OBB	O�B	P�B	QNB	Q�B	NpB	OBB	O�B	QNB	QB	PB	OB	PB	QNB	QB	OBB	OB	PB	PHB	P�B	O�B	PB	OvB	PHB	QB	OvB	NpB	P�B	T�B	TaB	R�B	QNB	R B	Q�B	R�B	R B	OBB	S&B	R�B	P�B	QNB	P�B	QNB	QNB	T�B	Q�B	P�B	T,B	S[B	S�B	Q�B	Q�B	P�B	QB	U�B	TaB	Q�B	Q�B	S�B	PB	N�B	WsB	N�B	S�B	T,B	P�B	T,B	MB	MjB	Q�B	PB	Q�B	P�B	OB	M6B	M�B	MB	MB	L0B	J�B	IRB	G�B	P�B	n�B	�rB	l�B	o�B	q�B	r�B	|�B	�zB	��B	��B	��B	��B	��B	�qB	�nB	��B	�=B	��B	��B	��B	�B	�B	�0B	�@B	��B	�4B	�B	��B	�kB	�6B	��B	��B	��B	��B	��B	�UB	��B	�FB	�-B	�B	��B	��B	�B	�XB	��B	��B	�BB	��B	�jB	��B	ȴB	��B	��B	��B	��B	B	ƨB	�}B	��B	��B	��B	�B	�OB	��B	� B	��B	�B	�qB	��B	��B	��B	��B	�B	�RB	�B	��B	��B	��B	��B	��B	�tB	�B	�hB	�qB	��B	�B	��B	��B	�LB	�nB	�B	�nB	�LB	�=B	�B	��B	�RB	�)B	�vB	�BB	�B	��B	�KB	��B
\B
PB
SB
)�B
�B
xB
6�B
F?B
+kB
;dB
#�B
3hB
>BB
D�B
B'B
=B
>�B
<jB
9�B
:^B
9�B
8B
8RB
=B
:^B
=�B
?�B
AUB
B�B
C�B
OBB
B�B
J#B
E�B
M�B
N�B
R�B
]/B
`�B
_B
a�B
]�B
Z�B
Z�B
Y�B
YKB
WsB
XEB
Z�B
Z�B
W�B
R�B
S�B
P�B
J�B
a�B
L�B
J�B
L0B
EB
d�B
K�B
O�B
�AB
l�B
TaB
Q�B
I�B
o�B
}�B
�lB
�B
�dB
��B
�B
��B
�iB
��BGB$BJXBMjB<B0�B1�B+B1�B+�B,�B'RB	B&�B#�B%�BqB�BuBFB�B�B�BGBJB�B�B
��B
�B
��B
�|B
��B$B
�JB
��B
�WB
�B
�fB
�TB
� B
��B
�B
��B
�)B
��B
ٴB
��B
�EB
��B
��B
��B
�B
ѷB
ŢB
�jB
�B
��B
�$B
��B
�1B
��B
��B
�~B
��B
}�B
��B
cB
v�B
rB
rB
�B
D3B
S�B
:�B
B
0UB
1B
VB
�B
(B
�B	��B	�B	�B	��B	��B
SB	�EB	�B	��B	�9B	�6B	��B	�^B	��B	�[B	�B	��B	��B	��B	��B	��B	�zB	�_B	��B	��B	�.B	w�B	g8B	g�B	o�B	m�B	dZB	c B	[#B	b�B	uZB	u�B	[�B	E�B	DgB	E�B	IB	EB	?B	?B	C�B	JXB	B'B	>�B	C-B	6zB	4�B	2aB	1[B	1[B	5tB	+kB	+�B	/�B	+kB	%�B	'�B	,�B	%�B	$@B	>BB	(XB	VB	$�B	&LB	&B	#�B	�B	 �B	 'B	�B	�B	OB	B	CB	�B	&B	(XB	#nB	!�B	!B	B	OB	B	 �B	 �B	�B	�B	PB	qB	�B��B�cB��B� B��B�B�AB�B�5B�GB�B��B	MB	(�B	'�B	*eB	#�B	"�B	 �B	�B	!-B	�B	�B	�B	�B	CB	=B	+B	�B	$B	�B	�B	uB	�B	B	�B	+B	8�B	B	 iB��B	�B��B��B��B��B��B�B�B�]B��B��B�,B�`B��B��B�B�vB�vB�;B�B��B�,B��B�TB�jB��B�TB�	BخBںB��B�pBܒBѷB��B҉B�TB��B�HB��BуB��B��B��B�vB�B�B�B�B�)B��B��B	xB		7B	�B	xB	~B	#:B	"�B	1'B	5tB	9XB	B�B	NpB	W
B	YB	Z�B	[�B	\]B	\�B	\�B	[�B	\)B	\]B	\]B	]dB	]�B	^jB	^B	]dB	]dB	\�B	^jB	^�B	^�B	^�B	^jB	]�B	]�B	^5B	]�B	_B	^5B	\�B	]�B	_pB	dZB	dZB	lWB	kQB	g8B	g8B	g�B	kB	o B	n�B	o�B	o�B	ncB	p�B	m�B	ncB	n/B	tTB	tB	n�B	q�B	v`B	|�B	~]B	�B	��B	�B	��B	~�B	�B	�MB	�7B	�B	��B	��B	��B	�PB	��B	��B	��B	��B	�\B	�B	��B	�VB	��B	�B	�.B	��B	��B	��B	�4B	��B	�B	��B	��B	�_B	��B	��B	��B	��B	��B	�=B	��B	�xB	��B	�~B	�~B	��B	��B	��B	�\B	��B	�'B	��B	��B	��B	��B	��B	��B	�-B	�4B	�bB	��B	�\B	�\B	�4B	��B	�nB	�tB	�B	�nB	��B	�B	��B	��B	�LB	��B	��B	��B	�$B	�B	��B	��B	�CB	��B	�B	�OB	��B	��B	�!B	�UB	�UB	�[B	��B	��B	�aB	��B	��B	��B	��B	��B	�nB	�nB	��B	�9B	�9B	�B	��B	��B	�dB	�B	�qB	�jB	�6B	��B	��B	��B	�qB	��B	�B	��B	��B	�qB	�B	��B	��B	��B	��B	�B	ɆB	ȴB	�KB	ǮB	ɆB	�RB	��B	ɆB	�B	�KB	��B	ȴB	��B	�RB	ɆB	ɆB	ɺB	�RB	ȀB	ȀB	��B	�B	ɆB	��B	�XB	��B	�)B	�pB	ӏB	�HB	�?B	�KB	�B	�WB	�#B	�WB	�WB	�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  B	P�B	P�B	P�B	P}B	PbB	P�B	P�B	P�B	P}B	PHB	PbB	PbB	PbB	P}B	PbB	P�B	PbB	PHB	PbB	P.B	P�B	PbB	P�B	P}B	PbB	PHB	PbB	PbB	Q�B	Q�B	Q�B	R�B	R�B	Q�B	TFB	x�B	��B
BB
PB
E�B
UgB
e�B
lWB
|jB
�?B5B-B�B
	B
��B
��B
�`B
�hB
MPB
DB	�B	˒B	�B	� B	r-B	MPB	E�B	7fB	5%B	*B	#�B	-B	/�B	�B��B	�B	%B	;B	$�B	�B��B�>B�_B�B�B�aBڠB�QB	[B	L�B	^�B	b�B	oiB	xB	�3B	�pB	�B	��B	�B	��B	�B	�
B	��B	�B	�2B	�wB	�9B	��B	�^B	�@B	�~B	�4B	�B	��B
�B
B
}B
�B
:B
�B
#:B
$�B
(�B
+kB
,�B
.�B
1B
2�B
5?B
1�B
49B
8�B
6�B
5�B
4�B
5ZB
7LB
6`B
8B
6�B
5�B
5tB
5?B
5?B
6`B
9	B
8�B
8�B
9	B
8B
7�B
:*B
:�B
<B
>�B
=�B
>(B
>�B
>�B
?HB
AoB
DMB
D3B
D�B
E�B
GB
F�B
F�B
E�B
E�B
E9B
GB
J�B
KxB
K�B
L�B
L�B
K�B
K�B
LdB
L0B
L�B
O\B
OvB
N�B
MB
K�B
K�B
L�B
M6B
L�B
JrB
I�B
I�B
I�B
J�B
I�B
J�B
G�B
G_B
G�B
GB
G_B
G+B
F�B
G�B
F�B
FB
FB
E�B
C�B
C{B
CaB
AUB
AoB
BAB
B�B
B'B
@�B
@ B
>�B
>�B
>�B
>�B
<jB
;�B
:�B
:DB
:*B
9�B
9�B
8�B
9>B
9�B
8�B
6�B
6`B
5tB
4TB
3�B
2�B
1B
1AB
0�B
/�B
/�B
/�B
-B
-�B
-]B
+�B
-B
*eB
(
B
'�B
&LB
$�B
%FB
#�B
# B
#B
"hB
!�B
!HB
�B
�B
 B
�B
�B
~B
B
�B
�B
�B
�B
WB
B
dB
~B
	B
�B
7B
�B
1B
�B
gB
gB
�B
�B
�B
�B
gB
�B
�B
�B
�B
�B
[B
�B
�B
�B
[B
B
[B
B
B
�B
�B
�B
B
4B
�B
 B
B
�B
�B
(B
�B
B
�B
<B
�B
�B
�B
B
�B
~B
�B
~B
~B
B
�B
�B
dB
B
�B
�B
�B
<B
VB
�B
pB
B
"B
�B
�B
�B
(B
�B
bB
�B
�B
BB
\B
�B
�B
.B
�B
B
�B
VB
"B
�B
�B
�B
(B
vB
�B
BB
VB
�B
�B
�B
(B
�B
(B
�B
�B
bB
B
}B
�B
 B
�B
�B
NB
�B
4B
�B
�B
�B
 B
:B
B
�B
[B
@B
�B
@B
[B
�B
�B
,B
aB
aB
�B
MB
�B
�B
�B
B
�B
�B
�B
9B
mB
�B
�B
�B
sB
$B
�B
EB
eB
�B
�B
�B
�B
B
B
B
B
B
B
�B
�B
�B
�B
�B
�B
�B
xB
�B
�B
�B
IB
�B
�B
�B
�B
IB
B
�B
�B
�B
5B
B
B
jB
VB
�B
pB
pB
�B
�B
 B
�B
;B
;B
�B
 vB
 B
 �B
 �B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
"hB
#:B
#B
#:B
#:B
#nB
#�B
$ZB
$tB
%,B
$�B
%B
%FB
&2B
&�B
&�B
'8B
'mB
(XB
)�B
)�B
)�B
*B
+�B
+�B
+kB
+�B
+�B
,=B
+�B
+�B
+kB
+�B
,�B
,qB
,WB
,B
,=B
+�B
+�B
,=B
,�B
,�B
-]B
-)B
-CB
-�B
-�B
.cB
/B
/5B
/OB
/iB
/�B
/5B
/�B
0oB
0�B
1B
0�B
0�B
1�B
1vB
1�B
2B
2�B
2�B
2�B
2aB
2|B
2|B
2�B
2|B
2-B
2|B
2�B
2�B
2�B
2�B
2�B
3MB
3�B
4B
3�B
3�B
3�B
3�B
3�B
4nB
4�B
5B
5tB
7�B
88B
8�B
8�B
8lB
88B
88B
8�B
8�B
8�B
8�B
8B
8RB
8RB
8RB
7�B
7�B
8B
8�B
8B
8lB
9>B
8�B
9�B
:DB
:�B
;B
:�B
;�B
;JB
;�B
;�B
;dB
;dB
<PB
=<B
=B
>(B
>wB
>�B
>�B
?�B
?.B
?�B
?�B
?cB
?}B
?�B
@4B
@�B
AB
@�B
A B
AB
AB
@�B
@�B
AUB
AoB
A�B
B�B
BAB
BAB
B[B
BB
B�B
C-B
B�B
C{B
C�B
C�B
C�B
DMB
DgB
DMB
DgB
E�B
EmB
E9B
ESB
EB
EB
ESB
ESB
E�B
F�B
E�B
E�B
G�B
G�B
GB
G+B
G+B
G_B
G�B
G�B
G�B
G_B
GzB
G�B
H1B
IB
IlB
I�B
JXB
J�B
KxB
K^B
K)B
K�B
K�B
L~B
K�B
K�B
K�B
KxB
K^B
LB
MB
K�B
K�B
K�B
L~B
LJB
L�B
L�B
L�B
MPB
M�B
M�B
M�B
N"B
NB
N�B
N�B
O\B
O�B
O�B
O�B
PHB
P�B
QNB
RB
R�B
R�B
S@B
S�B
S�B
TFB
T{B
T�B
UB
UB
UgB
U�B
VB
V�B
WYB
W
B
WsB
W?B
W�B
XEB
W�B
W�B
W�B
X+B
X�B
XyB
X�B
X�B
YB
X�B
Y1B
Y1B
YKB
Y�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[#B
[�B
[�B
\�B
\�B
\�B
\�B
]IB
]/B
]IB
]dB
]�B
]�B
]�B
^B
^jB
^OB
^�B
_;B
_VB
_!B
_pB
_pB
_�B
_�B
_�B
`'B
`BB
`�B
`�B
`�B
`�B
aB
a�B
a�B
bNB
b�B
b�B
b�B
b�B
c�B
c�B
c:B
d@B
d�B
e`B
eFB
e,B
e,B
e,B
eB
eFB
e,B
e�B
f2B
e�B
fB
f�B
g8B
gB
g8B
g�B
hXB
h�B
hXB
h�B
h�B
h�B
i*B
i�B
i�B
j0B
j0B
jB
jKB
jB
j0B
jB
jB
j�B
j�B
j�B
kB
kQB
k6B
kQB
kkB
k�B
lB
lWB
lqB
lWB
l�B
mCB
mwB
mwB
m)B
m�B
mwB
m]B
m�B
m�B
m�B
n}B
n�B
n�B
o B
oiB
o�B
o�B
p;B
p!B
p�B
qB
p�B
poB
p�B
qB
q�B
q�B
raB
r�B
sB
r�B
sB
s3B
s�B
t9B
t�B
tTB
t9B
tTB
t�B
t�B
utB
u�B
vB
v+B
v�B
v`B
v�B
wB
v�B
w�B
w�B
xB
x�B
xB
x8B
xB
yrB
y$B
y$B
y�B
y>B
z*B
y�B
y�B
z�B
{dB
z�B
z�B
{�B
z�B
{JB
{�B
|B
{�B
{�B
{�B
{B
{�B
|�B
|�B
|PB
|B
{�B
|B
|6B
}<B
}"B
|�B
|�B
}VB
}VB
|�B
}<B
~B
}qB
}qB
}�B
~(B
}�B
~BB
~]B
~]B
~�B
~�B
HB
~�B
.B
~�B
.B
B
HB
cB
�B
}B
�B
�B
HB
�B
�B
� B
�B
�4B
��B
��B
� B
��B
� B
�B
� B
�UB
�UB
�UB
��B
��B
��B
�uB
��B
��B
��B
��B
�aB
�-B
�-B
��B
��B
�MB
��B
��B
��B
�9B
��B
��B
��B
��B
�mB
�%B
��B
�%G�O�B	PB	R B	P�B	OvB	P�B	Q�B	PB	O�B	P}B	Q�B	P�B	O�B	P}B	P�B	P}B	PB	OvB	QB	QB	OvB	OB	Q�B	QNB	O�B	OBB	QNB	Q�B	P�B	OBB	PB	QB	QB	O�B	O�B	QB	QNB	P�B	OBB	P�B	QB	QB	OBB	PB	QNB	QNB	OvB	OvB	QNB	P�B	P�B	OvB	O�B	Q�B	QB	N�B	PB	Q�B	PHB	OBB	P�B	Q�B	P�B	OvB	O�B	QB	P�B	O�B	OBB	PB	Q�B	QNB	O�B	OvB	P}B	QNB	QB	PB	O�B	PB	Q�B	P�B	OBB	OvB	P�B	PHB	O�B	OBB	P}B	QB	P�B	O�B	OB	P}B	R�B	P}B	OBB	PB	QNB	P}B	N�B	O�B	P}B	QNB	PHB	OvB	OvB	PHB	QNB	P}B	OB	P�B	QB	P}B	O�B	OvB	P}B	QNB	QNB	PHB	OB	O�B	QNB	QB	PB	PHB	P�B	QB	QB	O�B	PHB	O�B	Q�B	QNB	P�B	P�B	O�B	QB	QB	P�B	OBB	O�B	P�B	QNB	Q�B	NpB	OBB	O�B	QNB	QB	PB	OB	PB	QNB	QB	OBB	OB	PB	PHB	P�B	O�B	PB	OvB	PHB	QB	OvB	NpB	P�B	T�B	TaB	R�B	QNB	R B	Q�B	R�B	R B	OBB	S&B	R�B	P�B	QNB	P�B	QNB	QNB	T�B	Q�B	P�B	T,B	S[B	S�B	Q�B	Q�B	P�B	QB	U�B	TaB	Q�B	Q�B	S�B	PB	N�B	WsB	N�B	S�B	T,B	P�B	T,B	MB	MjB	Q�B	PB	Q�B	P�B	OB	M6B	M�B	MB	MB	L0B	J�B	IRB	G�B	P�B	n�B	�rB	l�B	o�B	q�B	r�B	|�B	�zB	��B	��B	��B	��B	��B	�qB	�nB	��B	�=B	��B	��B	��B	�B	�B	�0B	�@B	��B	�4B	�B	��B	�kB	�6B	��B	��B	��B	��B	��B	�UB	��B	�FB	�-B	�B	��B	��B	�B	�XB	��B	��B	�BB	��B	�jB	��B	ȴB	��B	��B	��B	��B	B	ƨB	�}B	��B	��B	��B	�B	�OB	��B	� B	��B	�B	�qB	��B	��B	��B	��B	�B	�RB	�B	��B	��B	��B	��B	��B	�tB	�B	�hB	�qB	��B	�B	��B	��B	�LB	�nB	�B	�nB	�LB	�=B	�B	��B	�RB	�)B	�vB	�BB	�B	��B	�KB	��B
\B
PB
SB
)�B
�B
xB
6�B
F?B
+kB
;dB
#�B
3hB
>BB
D�B
B'B
=B
>�B
<jB
9�B
:^B
9�B
8B
8RB
=B
:^B
=�B
?�B
AUB
B�B
C�B
OBB
B�B
J#B
E�B
M�B
N�B
R�B
]/B
`�B
_B
a�B
]�B
Z�B
Z�B
Y�B
YKB
WsB
XEB
Z�B
Z�B
W�B
R�B
S�B
P�B
J�B
a�B
L�B
J�B
L0B
EB
d�B
K�B
O�B
�AB
l�B
TaB
Q�B
I�B
o�B
}�B
�lB
�B
�dB
��B
�B
��B
�iB
��BGB$BJXBMjB<B0�B1�B+B1�B+�B,�B'RB	B&�B#�B%�BqB�BuBFB�B�B�BGBJB�B�B
��B
�B
��B
�|B
��B$B
�JB
��B
�WB
�B
�fB
�TB
� B
��B
�B
��B
�)B
��B
ٴB
��B
�EB
��B
��B
��B
�B
ѷB
ŢB
�jB
�B
��B
�$B
��B
�1B
��B
��B
�~B
��B
}�B
��B
cB
v�B
rB
rB
�B
D3B
S�B
:�B
B
0UB
1B
VB
�B
(B
�B	��B	�B	�B	��B	��B
SB	�EB	�B	��B	�9B	�6B	��B	�^B	��B	�[B	�B	��B	��B	��B	��B	��B	�zB	�_B	��B	��B	�.B	w�B	g8B	g�B	o�B	m�B	dZB	c B	[#B	b�B	uZB	u�B	[�B	E�B	DgB	E�B	IB	EB	?B	?B	C�B	JXB	B'B	>�B	C-B	6zB	4�B	2aB	1[B	1[B	5tB	+kB	+�B	/�B	+kB	%�B	'�B	,�B	%�B	$@B	>BB	(XB	VB	$�B	&LB	&B	#�B	�B	 �B	 'B	�B	�B	OB	B	CB	�B	&B	(XB	#nB	!�B	!B	B	OB	B	 �B	 �B	�B	�B	PB	qB	�B��B�cB��B� B��B�B�AB�B�5B�GB�B��B	MB	(�B	'�B	*eB	#�B	"�B	 �B	�B	!-B	�B	�B	�B	�B	CB	=B	+B	�B	$B	�B	�B	uB	�B	B	�B	+B	8�B	B	 iB��B	�B��B��B��B��B��B�B�B�]B��B��B�,B�`B��B��B�B�vB�vB�;B�B��B�,B��B�TB�jB��B�TB�	BخBںB��B�pBܒBѷB��B҉B�TB��B�HB��BуB��B��B��B�vB�B�B�B�B�)B��B��B	xB		7B	�B	xB	~B	#:B	"�B	1'B	5tB	9XB	B�B	NpB	W
B	YB	Z�B	[�B	\]B	\�B	\�B	[�B	\)B	\]B	\]B	]dB	]�B	^jB	^B	]dB	]dB	\�B	^jB	^�B	^�B	^�B	^jB	]�B	]�B	^5B	]�B	_B	^5B	\�B	]�B	_pB	dZB	dZB	lWB	kQB	g8B	g8B	g�B	kB	o B	n�B	o�B	o�B	ncB	p�B	m�B	ncB	n/B	tTB	tB	n�B	q�B	v`B	|�B	~]B	�B	��B	�B	��B	~�B	�B	�MB	�7B	�B	��B	��B	��B	�PB	��B	��B	��B	��B	�\B	�B	��B	�VB	��B	�B	�.B	��B	��B	��B	�4B	��B	�B	��B	��B	�_B	��B	��B	��B	��B	��B	�=B	��B	�xB	��B	�~B	�~B	��B	��B	��B	�\B	��B	�'B	��B	��B	��B	��B	��B	��B	�-B	�4B	�bB	��B	�\B	�\B	�4B	��B	�nB	�tB	�B	�nB	��B	�B	��B	��B	�LB	��B	��B	��B	�$B	�B	��B	��B	�CB	��B	�B	�OB	��B	��B	�!B	�UB	�UB	�[B	��B	��B	�aB	��B	��B	��B	��B	��B	�nB	�nB	��B	�9B	�9B	�B	��B	��B	�dB	�B	�qB	�jB	�6B	��B	��B	��B	�qB	��B	�B	��B	��B	�qB	�B	��B	��B	��B	��B	�B	ɆB	ȴB	�KB	ǮB	ɆB	�RB	��B	ɆB	�B	�KB	��B	ȴB	��B	�RB	ɆB	ɆB	ɺB	�RB	ȀB	ȀB	��B	�B	ɆB	��B	�XB	��B	�)B	�pB	ӏB	�HB	�?B	�KB	�B	�WB	�#B	�WB	�WB	�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
='�7=>zd=��<�^H<(g�<_��<#�
<��}=�b<b��<L�3<#�
<S	<iU"<�y<���<��=
�<�߳<���<l/{<�S@<��c<�ź<`��<#�
<#�
<#�
<1WX<#�
<#�
<99�<���<r�r<#�
<#�
<#�
<#�
<j�L<L?�<#�
<#�
<#�
<�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018120723290320181207232903IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010420190106200104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551220190521075512IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                