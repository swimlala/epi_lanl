CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-15T15:31:00Z creation; 2021-10-15T19:29:27Z DMQC;      
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
_FillValue        G�O�     �  c<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �p   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ɛ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � W�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210315153100  20211015173717  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               k   kAA  AOAO7824_008764_107                 7824_008764_107                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�e�N&Ԁ@�e�N&Ԁ11  @�e���v�@�e���v�@7!1eәp@7!1eәp�d�ѷX��d�ѷX�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?�@@  @�G�@�G�@�  @�  @��RA\)A\)A*�HA@  A`  A�  A�  A��A�  A���A�  A�  A�Q�B   B�
B  B  B   B((�B0  B8  B@  BG�
BO�
BW�
B`(�Bh(�Bp  Bw�
B�  B��B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B�{B�  B�  B��B��B�  B�  B�  B�  B�  B�{B�{B�(�B�  B�  B�  B�{B�(�B�(�B�(�C {C{C
=C
=C
=C

=C�C  C��C��C  C  C  C  C  C��C��C"  C$
=C&
=C(
=C*  C,  C-��C0  C2
=C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI��CL  CN
=CP
=CR  CS��CV  CX
=CZ  C\  C^  C_��Ca��Cd
=Cf  Cg��Ci��Ck��Cm�Cp  Cr  Cs��Cv  Cx  Cz  C{��C}��C�  C�C�C�C�C�C�  C���C���C�C�C���C���C�  C���C�  C�C�  C�C�  C���C�  C�  C�C�  C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�C�  C�  C�  C���C�C�  C�  C���C�  C�
=C�
=C���C�  C���C���C�C�C�C�C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C���C�  C�C�C�C�C�  C�  C�C�C�  C�  C�C�  C�  C�C�C�C�  C�C�  C���C�C�C���C���C���C�  C�  C���C���C�  C�C�C�  C�  C���C���C���C�C�
=D   D � D�D}qD�qD��D�D}qD�qD}qD�qD��D�D� D  D}qD  D��D	  D	z�D	�qD
� D  D� D  D� D  D}qD�qD��D�D� D  D� D  D� D�D� D�qD}qD  D��D�D� D  D� D  D}qD  D� D  D� D  D� D  D� D  D}qD  D��D  D� D�qD� D �D � D!  D!� D"D"��D#�D#� D$�D$}qD$��D%� D&�D&��D'  D'}qD'�qD(� D)�D)��D*D*��D+�D+��D,  D,��D-  D-}qD-�qD.}qD/�D/� D0  D0� D1  D1� D2  D2��D3�D3��D4  D4� D4�qD5}qD5�qD6}qD6�qD7��D8�D8��D9�D9� D:  D:}qD;  D;��D<  D<}qD=  D=��D>�D>��D?  D?� D@�D@��DA  DA� DB  DB� DC�DC}qDC�qDD}qDE  DE� DF�DF��DG�DG��DH  DH}qDH�qDI� DJ�DJ��DK  DK� DK�qDL}qDL�qDM� DN�DN� DO  DO��DP�DP�DQ�DQ� DR  DR}qDS�DS��DT�DT��DU�DU�DV  DV��DW�DW� DW�qDX� DY  DY��DZ�DZ� DZ�qD[z�D\  D\��D]  D]}qD^  D^�D_�D_� D_�qD`� Da  Da� Db  Db� Dc  Dc��Dd�Dd� Dd��De}qDf  Df}qDg  Dg� Dh  Dh� Di�Di� Di�qDj� Dk�Dk��Dl�Dl� Dm  Dm� Dn  Dn}qDo  Do� Dp  Dp� Dp�qDq� Dr  Dr��Ds  Ds}qDt�Dt}qDu  Du}qDv  Dv� Dw  Dw� Dx  Dx}qDy  Dy� Dz�Dz� Dz�qD{� D|  D|� D}  D}� D}�qD~}qD~�qD� D�HD�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D��HD�HD�>�D�� D�� D���D�>�D��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�� D�D��D�AHD��HD�� D���D�>�D�~�D���D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�=qD�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D���D�@ D��HD�D�  D�>�D�� D�� D���D�AHD�� D�� D�  D�=qD�~�D��HD�  D�@ D��HD���D��qD�@ D��HD�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D�HD�B�D��HD�� D���D�@ D�� D���D��qD�@ D��HD�� D�  D�AHD��HD�� D���D�>�D�� D��qD���D�@ D�~�D��qD���D�AHD�� D���D���D�@ D��HD�� D���D�=qD�~�D�� D���D�>�D�� D��HD�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�>�D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D���D�>�D�}qD��qD�  D�B�D��HD��HD�HD�AHD��HD�D��D�B�D�� D���D�  D�>�D�~�D���D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�}qD¾�D�HD�@ DÀ D�� D�HD�AHDĀ D�� D�  D�>�D�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D�� D�HD�AHDȀ D�� D�HD�AHDɀ Dɾ�D�  D�>�D�~�D�� D�HD�@ D�~�D˾�D���D�>�D�~�D�� D���D�>�D̀ D�� D���D�>�D΀ D�� D���D�=qD�~�DϾ�D�  D�>�DЀ D�� D�HD�@ D�~�D�� D�HD�@ D�~�DҾ�D��qD�@ DӀ DӾ�D�  D�AHDԀ D��HD�  D�>�D�~�D�� D�HD�AHDր D�� D�  D�@ D�~�D�� D�HD�@ D؁HD�� D���D�>�Dـ Dپ�D���D�@ Dڀ Dھ�D�  D�AHDہHD��HD�HD�@ D�~�D��HD�HD�>�D݀ D�� D��qD�>�D�~�D�� D�HD�>�D߀ D��HD�  D�>�D��HD��HD�  D�>�D� D��HD�  D�@ D�~�D�qD��qD�@ D�HD�� D�  D�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�~�D澸D�  D�AHD�HD�� D���D�>�D�~�D辸D�  D�@ D�~�D龸D�  D�B�D� D�qD���D�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�~�D��HD�  D�>�D� D�� D���D�@ D� D�� D��?#�
?L��?�  ?��R?Ǯ?�(�@   @��@&ff@333@:�H@L��@^�R@p��@�G�@�=q@�33@��R@�ff@�{@��H@�ff@��@�(�@���@��A   AA�A��AffA(�A"�\A(Q�A.{A3�
A:=qA@  AFffAL��AQ�AXQ�A^{Adz�Aj=qAp��AvffA|��A�G�A�z�A��A��\A�A���A��A�
=A��A���A�\)A��\A�p�A�Q�A�33A�{A���A��
A�{A�G�A��
A��RA�G�A��
A�ffA�G�A�(�AθRA�G�A�(�AָRA�G�A��
A�ffA�G�A��
A�RA陚A��
A�
=A�A�(�A�
=A���A��
A��RB ��BB�HB(�BG�B=qB33B(�B	�B
=qB33B  B�B�B�HB�
B��Bp�BffB\)B(�B��B�B�HB�B��B��BffB\)BQ�B��BB�RB�B Q�B!G�B"=qB#
=B$  B$��B%�B&�HB'�B(��B)B*�\B+\)B,Q�B-G�B.=qB/33B0Q�B1�B2=qB3
=B4  B5�B6{B7
=B8(�B8��B:{B;
=B;�
B<��B=B>�RB?�B@��BABB�\BC�BD��BEp�BF�\BG�BHz�BI��BJ�\BK�BL��BMG�BN=qBO33BP  BP��BQBR�HBS�BT��BU��BVffBW33BXQ�BYG�BZ=qB[33B\(�B]G�B^=qB_33B`(�B`��Bb{Bc33Bd  Be�Be�Bg
=Bh  Bi�Bj=qBk33Bl(�Bm�Bn=qBo
=Bp(�BqG�Br{Bs33Bt  Bu�Bv{Bw33Bx  ByG�Bz=qB{
=B|(�B}G�B~ffB33B�(�B���B��B��B�{B���B��B��B�(�B���B�33B�B�Q�B��RB�\)B��
B�Q�B���B�p�B��B�ffB���B�p�B��B�z�B���B��B�{B��\B�
=B��B�{B���B�33B��B�=qB��RB�G�B��
B�Q�B��HB�p�B��B�z�B���B��B�{B�z�B��B���B�(�B���B�G�B�B�=qB��RB�\)B��B�ffB��HB��B�  B��\B�
=B��B�(�B��RB�G�B�B�Q�B��HB�G�B��B�z�B���B�p�B��B��\B�
=B���B�{B���B�33B�B�=qB���B�G�B��B�ffB���B��B�  B���B��B��B�=qB��RB�\)B��B�ffB���B���B�  B���B��B�B�=qB���B�G�B��B�ffB���B��B�  B��\B�33B��B�Q�B���B�p�B�  B���B�33B�B�ffB���BÅB�{BĸRB�\)B��BƏ\B��B�B�Q�B��HBɅB�{Bʣ�B�G�B��B�ffB�
=B͙�B�(�B���B�p�B�  BУ�B�G�B�B�Q�B���Bә�B�(�BԸRB�\)B�  B֏\B��B׮B�Q�B���BمB�(�BڸRB�\)B�  B�z�B��B�B�Q�B���B߅B�(�B�RB�\)B��
B�\B��B�B�Q�B��HB�p�B�{B�RB�G�B��
B�ffB�
=B�B�Q�B��HB�B�(�B���B�B�{B���B�p�B�{B�RB�G�B�  B��B�G�B�  B�\B�\)B��B���B�\)B�  B���B�33B�  B���B�G�B�  B���B�G�B��B��\B�G�C   C Q�C ��C ��CQ�C��C  CQ�C�C  CQ�C��C��CG�C��C��CQ�C��C  CQ�C��C��CG�C��C  CG�C��C�C	G�C	�\C	�HC
33C
�C
��C{C\)C�C�C(�CffC��C��C  C33C\)C�\CC  C�C\)C�\CC��C�C\)C�C�C�C
=C=qCp�C��C��C  C(�C\)C�\C�RC�C�CQ�Cz�C�C�C
=CG�Cz�C��C�
C
=C33Cp�C��C��C
=C33CffC��C��C  C(�CffC��CC��C(�CffC�\CC��C�CQ�C�C�RC�C�CQ�C�C�RC�C�C\)C�\C�RC��C(�C\)C�\C�RC  C(�C\)C�CC�C(�CQ�C�C�C�C�CQ�Cz�C�RC�HC{CG�Cz�C�C�
C {C =qC p�C ��C �
C!
=C!=qC!p�C!��C!�
C"  C"=qC"ffC"��C"��C#  C#(�C#\)C#�\C#C#��C$�C$Q�C$�\C$�RC$�C%{C%G�C%z�C%��C%�
C&
=C&=qC&ffC&��C&��C&��C'33C'\)C'�\C'�RC'��C(�C(Q�C(z�C(�C(�HC){C)=qC)p�C)��C)�
C*  C*33C*\)C*��C*��C*��C+(�C+Q�C+�C+�RC+�C,(�C,Q�C,�C,C,�C-�C-Q�C-�\C-�RC-��C.(�C.\)C.��C.C/  C/33C/ffC/��C/�
C0
=C0=qC0z�C0�C0�HC1�C1Q�C1�\C1C1��C2=qC2\)C2��C2�
C3
=C3G�C3p�C3�RC3�C4{C4\)C4�\C4�RC5  C533C5ffC5��C5�
C6{C6Q�C6z�C6�RC6��C733C7ffC7��C7�
C8
=C8G�C8z�C8C9  C933C9ffC9��C9�HC:�C:\)C:��C:��C;
=C;G�C;�C;C<  C<33C<z�C<�RC<��C=33C=ffC=��C=�C>�C>\)C>��C>�
C?{C?G�C?�C?C@  C@=qC@z�C@�C@�CA(�CAffCA��CA�HCB�CB\)CB��CB�
CC�CCQ�CC�\CC��CD
=CDQ�CD�\CD��CE
=CEG�CE�CECE��CF33CFz�CF�RCG  CG=qCGp�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                        ?��?�@@  @�G�@�G�@�  @�  @��RA\)A\)A*�HA@  A`  A�  A�  A��A�  A���A�  A�  A�Q�B   B�
B  B  B   B((�B0  B8  B@  BG�
BO�
BW�
B`(�Bh(�Bp  Bw�
B�  B��B�  B��B�  B�{B�  B�  B�  B�  B�  B�  B�{B�  B�  B��B��B�  B�  B�  B�  B�  B�{B�{B�(�B�  B�  B�  B�{B�(�B�(�B�(�C {C{C
=C
=C
=C

=C�C  C��C��C  C  C  C  C  C��C��C"  C$
=C&
=C(
=C*  C,  C-��C0  C2
=C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI��CL  CN
=CP
=CR  CS��CV  CX
=CZ  C\  C^  C_��Ca��Cd
=Cf  Cg��Ci��Ck��Cm�Cp  Cr  Cs��Cv  Cx  Cz  C{��C}��C�  C�C�C�C�C�C�  C���C���C�C�C���C���C�  C���C�  C�C�  C�C�  C���C�  C�  C�C�  C�  C���C���C�  C�C�C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�C�  C�  C�  C���C�C�  C�  C���C�  C�
=C�
=C���C�  C���C���C�C�C�C�C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C���C�  C�C�C�C�C�  C�  C�C�C�  C�  C�C�  C�  C�C�C�C�  C�C�  C���C�C�C���C���C���C�  C�  C���C���C�  C�C�C�  C�  C���C���C���C�C�
=D   D � D�D}qD�qD��D�D}qD�qD}qD�qD��D�D� D  D}qD  D��D	  D	z�D	�qD
� D  D� D  D� D  D}qD�qD��D�D� D  D� D  D� D�D� D�qD}qD  D��D�D� D  D� D  D}qD  D� D  D� D  D� D  D� D  D}qD  D��D  D� D�qD� D �D � D!  D!� D"D"��D#�D#� D$�D$}qD$��D%� D&�D&��D'  D'}qD'�qD(� D)�D)��D*D*��D+�D+��D,  D,��D-  D-}qD-�qD.}qD/�D/� D0  D0� D1  D1� D2  D2��D3�D3��D4  D4� D4�qD5}qD5�qD6}qD6�qD7��D8�D8��D9�D9� D:  D:}qD;  D;��D<  D<}qD=  D=��D>�D>��D?  D?� D@�D@��DA  DA� DB  DB� DC�DC}qDC�qDD}qDE  DE� DF�DF��DG�DG��DH  DH}qDH�qDI� DJ�DJ��DK  DK� DK�qDL}qDL�qDM� DN�DN� DO  DO��DP�DP�DQ�DQ� DR  DR}qDS�DS��DT�DT��DU�DU�DV  DV��DW�DW� DW�qDX� DY  DY��DZ�DZ� DZ�qD[z�D\  D\��D]  D]}qD^  D^�D_�D_� D_�qD`� Da  Da� Db  Db� Dc  Dc��Dd�Dd� Dd��De}qDf  Df}qDg  Dg� Dh  Dh� Di�Di� Di�qDj� Dk�Dk��Dl�Dl� Dm  Dm� Dn  Dn}qDo  Do� Dp  Dp� Dp�qDq� Dr  Dr��Ds  Ds}qDt�Dt}qDu  Du}qDv  Dv� Dw  Dw� Dx  Dx}qDy  Dy� Dz�Dz� Dz�qD{� D|  D|� D}  D}� D}�qD~}qD~�qD� D�HD�@ D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD�� D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D��HD��HD�HD�AHD��HD��HD�HD�>�D�� D�� D�  D�AHD�� D���D�  D�@ D�~�D��HD�HD�>�D�� D�� D���D�>�D��HD��HD�HD�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D�  D�AHD��HD��HD�  D�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D�� D�  D�>�D�� D�D��D�AHD��HD�� D���D�>�D�~�D���D�  D�@ D�~�D�� D���D�>�D�� D�� D���D�=qD�� D��HD�HD�AHD�� D�� D�  D�@ D��HD�� D���D�@ D��HD�D�  D�>�D�� D�� D���D�AHD�� D�� D�  D�=qD�~�D��HD�  D�@ D��HD���D��qD�@ D��HD�� D�  D�AHD��HD�� D���D�>�D�� D�� D�  D�@ D�� D�� D�HD�B�D��HD�� D���D�@ D�� D���D��qD�@ D��HD�� D�  D�AHD��HD�� D���D�>�D�� D��qD���D�@ D�~�D��qD���D�AHD�� D���D���D�@ D��HD�� D���D�=qD�~�D�� D���D�>�D�� D��HD�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D��HD�� D���D�>�D��HD��HD�  D�>�D�}qD���D�  D�@ D�� D���D���D�>�D�}qD��qD�  D�B�D��HD��HD�HD�AHD��HD�D��D�B�D�� D���D�  D�>�D�~�D���D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�� D�� D�  D�>�D�}qD¾�D�HD�@ DÀ D�� D�HD�AHDĀ D�� D�  D�>�D�~�D�� D�HD�AHDƀ D�� D�  D�@ Dǀ D�� D�HD�AHDȀ D�� D�HD�AHDɀ Dɾ�D�  D�>�D�~�D�� D�HD�@ D�~�D˾�D���D�>�D�~�D�� D���D�>�D̀ D�� D���D�>�D΀ D�� D���D�=qD�~�DϾ�D�  D�>�DЀ D�� D�HD�@ D�~�D�� D�HD�@ D�~�DҾ�D��qD�@ DӀ DӾ�D�  D�AHDԀ D��HD�  D�>�D�~�D�� D�HD�AHDր D�� D�  D�@ D�~�D�� D�HD�@ D؁HD�� D���D�>�Dـ Dپ�D���D�@ Dڀ Dھ�D�  D�AHDہHD��HD�HD�@ D�~�D��HD�HD�>�D݀ D�� D��qD�>�D�~�D�� D�HD�>�D߀ D��HD�  D�>�D��HD��HD�  D�>�D� D��HD�  D�@ D�~�D�qD��qD�@ D�HD�� D�  D�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�~�D澸D�  D�AHD�HD�� D���D�>�D�~�D辸D�  D�@ D�~�D龸D�  D�B�D� D�qD���D�>�D�~�D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�~�D��HD�  D�>�D� D�� D���D�@ D� D�� G�O�?#�
?L��?�  ?��R?Ǯ?�(�@   @��@&ff@333@:�H@L��@^�R@p��@�G�@�=q@�33@��R@�ff@�{@��H@�ff@��@�(�@���@��A   AA�A��AffA(�A"�\A(Q�A.{A3�
A:=qA@  AFffAL��AQ�AXQ�A^{Adz�Aj=qAp��AvffA|��A�G�A�z�A��A��\A�A���A��A�
=A��A���A�\)A��\A�p�A�Q�A�33A�{A���A��
A�{A�G�A��
A��RA�G�A��
A�ffA�G�A�(�AθRA�G�A�(�AָRA�G�A��
A�ffA�G�A��
A�RA陚A��
A�
=A�A�(�A�
=A���A��
A��RB ��BB�HB(�BG�B=qB33B(�B	�B
=qB33B  B�B�B�HB�
B��Bp�BffB\)B(�B��B�B�HB�B��B��BffB\)BQ�B��BB�RB�B Q�B!G�B"=qB#
=B$  B$��B%�B&�HB'�B(��B)B*�\B+\)B,Q�B-G�B.=qB/33B0Q�B1�B2=qB3
=B4  B5�B6{B7
=B8(�B8��B:{B;
=B;�
B<��B=B>�RB?�B@��BABB�\BC�BD��BEp�BF�\BG�BHz�BI��BJ�\BK�BL��BMG�BN=qBO33BP  BP��BQBR�HBS�BT��BU��BVffBW33BXQ�BYG�BZ=qB[33B\(�B]G�B^=qB_33B`(�B`��Bb{Bc33Bd  Be�Be�Bg
=Bh  Bi�Bj=qBk33Bl(�Bm�Bn=qBo
=Bp(�BqG�Br{Bs33Bt  Bu�Bv{Bw33Bx  ByG�Bz=qB{
=B|(�B}G�B~ffB33B�(�B���B��B��B�{B���B��B��B�(�B���B�33B�B�Q�B��RB�\)B��
B�Q�B���B�p�B��B�ffB���B�p�B��B�z�B���B��B�{B��\B�
=B��B�{B���B�33B��B�=qB��RB�G�B��
B�Q�B��HB�p�B��B�z�B���B��B�{B�z�B��B���B�(�B���B�G�B�B�=qB��RB�\)B��B�ffB��HB��B�  B��\B�
=B��B�(�B��RB�G�B�B�Q�B��HB�G�B��B�z�B���B�p�B��B��\B�
=B���B�{B���B�33B�B�=qB���B�G�B��B�ffB���B��B�  B���B��B��B�=qB��RB�\)B��B�ffB���B���B�  B���B��B�B�=qB���B�G�B��B�ffB���B��B�  B��\B�33B��B�Q�B���B�p�B�  B���B�33B�B�ffB���BÅB�{BĸRB�\)B��BƏ\B��B�B�Q�B��HBɅB�{Bʣ�B�G�B��B�ffB�
=B͙�B�(�B���B�p�B�  BУ�B�G�B�B�Q�B���Bә�B�(�BԸRB�\)B�  B֏\B��B׮B�Q�B���BمB�(�BڸRB�\)B�  B�z�B��B�B�Q�B���B߅B�(�B�RB�\)B��
B�\B��B�B�Q�B��HB�p�B�{B�RB�G�B��
B�ffB�
=B�B�Q�B��HB�B�(�B���B�B�{B���B�p�B�{B�RB�G�B�  B��B�G�B�  B�\B�\)B��B���B�\)B�  B���B�33B�  B���B�G�B�  B���B�G�B��B��\B�G�C   C Q�C ��C ��CQ�C��C  CQ�C�C  CQ�C��C��CG�C��C��CQ�C��C  CQ�C��C��CG�C��C  CG�C��C�C	G�C	�\C	�HC
33C
�C
��C{C\)C�C�C(�CffC��C��C  C33C\)C�\CC  C�C\)C�\CC��C�C\)C�C�C�C
=C=qCp�C��C��C  C(�C\)C�\C�RC�C�CQ�Cz�C�C�C
=CG�Cz�C��C�
C
=C33Cp�C��C��C
=C33CffC��C��C  C(�CffC��CC��C(�CffC�\CC��C�CQ�C�C�RC�C�CQ�C�C�RC�C�C\)C�\C�RC��C(�C\)C�\C�RC  C(�C\)C�CC�C(�CQ�C�C�C�C�CQ�Cz�C�RC�HC{CG�Cz�C�C�
C {C =qC p�C ��C �
C!
=C!=qC!p�C!��C!�
C"  C"=qC"ffC"��C"��C#  C#(�C#\)C#�\C#C#��C$�C$Q�C$�\C$�RC$�C%{C%G�C%z�C%��C%�
C&
=C&=qC&ffC&��C&��C&��C'33C'\)C'�\C'�RC'��C(�C(Q�C(z�C(�C(�HC){C)=qC)p�C)��C)�
C*  C*33C*\)C*��C*��C*��C+(�C+Q�C+�C+�RC+�C,(�C,Q�C,�C,C,�C-�C-Q�C-�\C-�RC-��C.(�C.\)C.��C.C/  C/33C/ffC/��C/�
C0
=C0=qC0z�C0�C0�HC1�C1Q�C1�\C1C1��C2=qC2\)C2��C2�
C3
=C3G�C3p�C3�RC3�C4{C4\)C4�\C4�RC5  C533C5ffC5��C5�
C6{C6Q�C6z�C6�RC6��C733C7ffC7��C7�
C8
=C8G�C8z�C8C9  C933C9ffC9��C9�HC:�C:\)C:��C:��C;
=C;G�C;�C;C<  C<33C<z�C<�RC<��C=33C=ffC=��C=�C>�C>\)C>��C>�
C?{C?G�C?�C?C@  C@=qC@z�C@�C@�CA(�CAffCA��CA�HCB�CB\)CB��CB�
CC�CCQ�CC�\CC��CD
=CDQ�CD�\CD��CE
=CEG�CE�CECE��CF33CFz�CF�RCG  CG=qCGp�CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                        @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A���A��A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��!A��-A��-A��FA��FA��9A��-A��!A��!A��A���A�/A��A�9XA��hA�&�A��#A�7LA���A��A�n�A��A���A�bNA�VA�E�A��A��yA�v�A��
A�l�A�C�A�1'A��!A��jA��A��A���A�JA���A��A���A�;dA�1'A���A��
A��A��A��A�  A��RA���A�hsA�C�A��A���A�Q�A�r�A�bNA���A�JA�hsA��A��;A�l�A���A�9XA��A���A��hA��^A�7LA�^5A���A�G�A�hsA���A�M�A�+A��A�
=A�1A�$�A��!A��A�=qA��`A�7LA��+A�#AS�A}��A{��Ax�yAw?}Au&�Arr�Ao�
An�+Am��Al��AjQ�Ah��Ag�Af��Aex�AcC�A`n�A^E�A]�A\�AZ��AX�!AWx�AVQ�AT1'AR~�AQVAN�HAM�AMt�AL^5AK;dAJ�AH=qAG��AF�DAE|�AEC�AD��AC�A@�uA?��A=�A=��A=?}A<��A<��A;�A;"�A9�FA8  A7S�A6��A6��A6(�A3��A3/A2Q�A0��A/�A/\)A/%A.JA-�A,ĜA+�^A*ȴA*(�A)�A)�FA)K�A(��A'�A&n�A%K�A%%A$ȴA$z�A#�7A#33A"��A"�A"jA!�A!O�A ��A (�A�wAS�A(�AC�AAt�A��A`BA�`A5?A|�AZAS�A$�A�A��A��AM�A�-AVA
��A
�A	��A�/A�#A^5AG�A�9A-A�mA�7A"�A9XA��A`BA;dA�A �A �A �!A ff@��F@�|�@�33@���@�ff@�-@���@��-@��7@�hs@��@�  @�33@�r�@�
=@�V@�@�/@�r�@�F@���@�@�p�@��@�o@홚@��@�V@�Ĝ@��@�t�@�"�@���@�\@�@�Ĝ@��;@��@� �@�&�@�o@�=q@�@�X@ܴ9@��m@�
=@ڗ�@�hs@��
@ם�@�dZ@�~�@�ƨ@��/@�j@�I�@�"�@��/@���@ɺ^@���@�1@��@ư!@Ƈ+@�n�@�M�@�{@ũ�@�hs@���@Ĵ9@�A�@þw@�ff@���@��-@���@�O�@�Ĝ@�z�@�9X@�  @�l�@�V@���@�p�@���@���@�Ĝ@�z�@���@���@�^5@���@�7L@��@���@�l�@���@�-@�p�@��j@�I�@�I�@�9X@��@���@��@�33@�
=@���@�j@���@�dZ@�o@���@�E�@�J@��@��-@�x�@��`@�o@���@�^5@�E�@�E�@�=q@�@��7@��@��/@��@���@��D@�r�@�j@�Z@�A�@�(�@��@���@�K�@�ȴ@��@�hs@���@��@�33@���@�v�@�^5@�J@�J@���@��@��@���@���@�bN@�Q�@�Q�@�Z@���@��D@��@�j@� �@�\)@���@���@���@��!@���@���@���@���@���@��\@���@���@��@�O�@��@���@�1'@�  @�ƨ@��@�C�@��H@���@�  @�S�@�K�@�+@�o@���@�n�@�$�@�5?@��#@�hs@�j@��P@�~�@�x�@���@�Z@�1'@�  @��@�\)@�"�@�n�@�v�@�n�@�$�@�V@�^5@�V@�E�@�@�X@���@�l�@�;d@�"�@���@�n�@��@��7@��@��@���@�z�@�(�@�  @��;@���@�|�@�
=@���@�n�@�M�@�@��@��^@�`B@��@�V@�%@���@��@���@���@�Ĝ@��@��@���@���@��D@���@�|�@��@��@��y@���@��!@���@�-@��#@���@��7@�p�@�O�@�7L@���@�A�@�w@l�@K�@;d@;d@;d@�@~�y@~�y@~�@~ȴ@~ȴ@~�R@~��@~��@~�+@~�+@~ff@~V@}��@}`B@}�@|�/@|�j@|z�@|I�@|I�@{ƨ@{C�@z^5@yhs@x�`@x��@xbN@w\)@v@u@u��@u�@up�@u`B@u/@t�@t��@t�D@t�D@t�D@tz�@tI�@tI�@tI�@t1@s@r=q@q��@p��@p�9@p�@p�@pbN@pbN@pb@ol�@o;d@n�@nV@m�@m�h@l��@lz�@l(�@kƨ@kdZ@j�H@j�\@jn�@jM�@j-@i�#@i�7@i�7@iX@h��@h�u@h1'@g��@gl�@gK�@f�R@fv�@e�-@e�@eV@d��@d�@c�@ct�@ct�@b��@a��@a��@a��@a�7@`�`@`��@`Q�@_��@_
=@^�R@^��@^��@^��@^5?@]`B@\�/@\��@[�m@[ƨ@[dZ@Y��@X��@XĜ@X�9@XĜ@X�9@X��@XbN@Xb@W�w@W
=@Vv�@VE�@V{@U�-@T�j@S�@S@R��@R�!@R^5@Qx�@PĜ@PĜ@P��@P�u@O�@O+@O�@O|�@O�@N��@N5?@M@M�-@M�h@MO�@MV@L�/@L��@Lz�@Lj@LZ@L9X@L�@L1@K�m@K��@Kt�@K�@K33@K@J�\@I�@I��@I�@H�`@HĜ@HĜ@H�u@HA�@G�;@G��@G\)@G
=@F�@F��@F�+@FV@F5?@F@E�T@E@E�-@E�h@E�@E`B@Dj@C��@C�m@Cƨ@C�F@CC�@B��@B�\@Bn�@B^5@B^5@BM�@B�@A�#@A��@A��@A��@A�^@A��@A��@A%@@�9@@�@?�@?��@?K�@>��@>��@>V@=��@=�h@=p�@=`B@=`B@=�@<�@<��@<�D@<z�@<(�@;dZ@;o@:�H@:�\@9�@9x�@9&�@8��@8��@8Ĝ@8�@8  @7��@7;d@7
=@6ȴ@6�+@6V@6$�@5��@5��@5V@4I�@3t�@2�@2�!@2�\@2��@2��@2~�@1x�@0��@0Ĝ@0�9@0��@0��@0A�@0  @/��@/l�@/\)@/+@.ȴ@.��@.��@.v�@.V@.5?@.$�@.@-��@-��@,��@,9X@+�
@+�@+C�@*��@*�\@*^5@*=q@*M�@*M�@)�#@)��@)�7@)x�@)hs@)hs@)hs@)X@)%@(1'@'�P@'|�@'|�@'\)@';d@'
=@&��@%�@%�@$�/@$�@$�D@$j@$9X@#��@#S�@#@"��@"��@"��@"��@"��@"��@"��@"�!@"�!@"��@!��@!�#@!��@!��@!X@!7L@ ��@ A�@�@�@��@�P@|�@|�@l�@;d@�@V@E�@5?@@�@�T@�T@�T@�T@�T@��@�-@��@��@�h@�h@�@`B@?}@��@z�@Z@I�@1@�F@t�@o@��@n�@=q@�@7L@Ĝ@Ĝ@��@r�@Q�@A�@A�@A�@1'@1'@ �@�@��@l�@K�@�y@{@�T@p�@V@�@�@9X@1@��@��@1@ƨ@t�@S�@33@�H@��@^5@=q@�@�@�@�@J@J@J@��@��@�#@��@x�@7L@7L@�@��@Q�@A�@1'@b@  @��@�P@\)@K�@K�@+@�R@��@�+@V@E�@5?@$�@{@{@A��A��PA��A��+A��hA��A�|�A��A���A���A���A��hA��PA��A��A��A��\A��PA��uA��uA���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��-A��!A��!A��A���A��A���A��A��!A��!A��A��!A��!A��!A��A��!A��A��-A��-A��-A��-A��-A��!A��!A��-A��-A��-A��-A��9A��FA��FA��9A��-A��9A��FA��FA��RA��FA��9A��9A��FA��^A��FA��9A��RA��^A��RA��9A��FA��RA��9A��FA��-A��!A��-A��9A��A��9A��-A��!A��9A��!A��FA��-A��9A��-A��-A��-A��A��-A��!A��A��A��-A��-A��A��A��!A��A��A��-A��A��A��A��!A��A��A���A���A���A��A��-A��!A���A���A���A���A���A��\A�Q�A�?}A�5?A�+A�A���A��A��HA��A���A��jA��A���A��7A�~�A�p�A�r�A�x�A�\)A�33A�-A�bA��/A��/A�ƨA��A���A��hA�p�A�ffA�XA�C�A�A�A�;dA�1'A�&�A��A�bA�A���A��A��A��yA��/A��A�A��FA���A���A���A�|�A�
=A��;A��jA��!A���A���A���A���A���A���A���A��7A��A��A��A��A�~�A�|�A�~�A�~�A��A�|�A�z�A�x�A�hsA�bNA�G�A�A�A�;dA�1'A�(�A��A�  A��;A���A��^A��9A���A���A��hA��A�p�A�jA�jA�jA�bNA�`BA�`BA�\)A�XA�VA�VA�VA�VA�XA�XA�VA�S�A�S�A�Q�A�O�A�G�A�;dA�33A�33A�$�A��A��A��A��A�oA�  A���A���A��A��yA��`A��TA��HA��;A���A���A���A��A�\)A�5?A��A��A�A��mA��/A���A��RA���A��PA�x�A�p�A�n�A�hsA�dZA�`BA�`BA�`BA�XA�Q�A�I�A�A�A�&�A�1A���A�ƨA�|�A�JA��RA��DA�ZA�9XA��A���A���A��A��A���A�&�A��A���A���A�|�A�`BA�G�A�=qA�&�A�JA�A�A�A���A��A��A��A��yA��yA��A��A��yA��mA���A��wA���A��hA�VA�?}A�7LA�JA��;A���A�O�A�A��HA���A�ƨA��uA�|�A�t�A�?}A�bA���A��9A��-A��-A��A���A���A���A���A��\A�l�A�S�A�K�A�;dA�7LA�+A�1A���A��mA���A�C�A��wA� �A���A�\)A�A���A��DA�\)A�33A� �A�VA��A���A��7A�M�A��A�  A���A��mA���A�ĜA��RA���A��uA�x�A�G�A�-A��A�1A���A��A���A���A�bNA��A���A��mA�ƨA���A�p�A�G�A��A�Q�A���A��RA�G�A��!A���A�|�A�A�A�bA���A���A�dZA�?}A�(�A��/A���A��+A�v�A�^5A�$�A���A�-A�bA�  A��yA��mA��#A���A��#A���A��jA��^A��wA��^A��^A���A���A��7A�t�A�bNA�\)A�O�A�S�A�O�A�S�A�O�A�Q�A�O�A�M�A�O�A�M�A�O�A�K�A�M�A�G�A� �A���A��A�jA�$�A��#A��hA�I�A�%A�~�A�E�A��A�%A���A��A��^A���A��A�dZA�A�bNA�E�A�C�A�=qA�;dA�9XA�7LA�/A� �A���A��A��jA���A��A�n�A�`BA�O�A�?}A�&�A�ȴA��A�VA���A���A�$�A���A���A�l�A�5?A�/A�&�A��A�JA�%A���A���A���A���A��A��A��A��A��A��;A���A��9A��\A�bNA�C�A�A���A�O�A�JA�A�A���A�ƨA���A��DA�VA�%A��A��`A��#A���A���A���A��+A��A��A�ffA�$�A��A��A��A��A�{A�{A�oA�JA�
=A���A��yA��;A�ȴA��-A���A���A���A���A���A���A���A���A���A���A��hA��\A�x�A�/A� �A�bA���A��A��RA���A�VA���A���A�A��9A�ȴA���A���A��
A���A��wA��!A��uA�~�A�VA�=qA�K�A�^5A�O�A�G�A�=qA�5?A���A�r�A�9XA��yA���A�S�A�VA��A��mA��HA���A���A��jA��-A���A���A���A���A��PA��A��A�r�A�p�A�dZA�\)A�S�A�9XA�JA�  A�A�  A�  A���A��wA�ffA���A��mA���A�z�A���A��;A���A��A���A�x�A�bNA�bNA�hsA�l�A�S�A�XA�M�A�C�A�G�A�A�A�?}A�9XA�=qA�9XA�33A�+A�&�A��A�{A�{A��A��A��A��A��A��A�oA��A��A�VA�
=A�VA�bA�bA�VA���A��A��;A�t�A�A�A�&�A�A�ƨA�~�A�9XA� �A���A�VA���A��;A���A���A���A��jA��wA��RA��FA��-A���A���A���A���A���A��PA��7A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                        A��A��A���A��A��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��!A��-A��-A��FA��FA��9A��-A��!A��!A��A���A�/A��A�9XA��hA�&�A��#A�7LA���A��A�n�A��A���A�bNA�VA�E�A��A��yA�v�A��
A�l�A�C�A�1'A��!A��jA��A��A���A�JA���A��A���A�;dA�1'A���A��
A��A��A��A�  A��RA���A�hsA�C�A��A���A�Q�A�r�A�bNA���A�JA�hsA��A��;A�l�A���A�9XA��A���A��hA��^A�7LA�^5A���A�G�A�hsA���A�M�A�+A��A�
=A�1A�$�A��!A��A�=qA��`A�7LA��+A�#AS�A}��A{��Ax�yAw?}Au&�Arr�Ao�
An�+Am��Al��AjQ�Ah��Ag�Af��Aex�AcC�A`n�A^E�A]�A\�AZ��AX�!AWx�AVQ�AT1'AR~�AQVAN�HAM�AMt�AL^5AK;dAJ�AH=qAG��AF�DAE|�AEC�AD��AC�A@�uA?��A=�A=��A=?}A<��A<��A;�A;"�A9�FA8  A7S�A6��A6��A6(�A3��A3/A2Q�A0��A/�A/\)A/%A.JA-�A,ĜA+�^A*ȴA*(�A)�A)�FA)K�A(��A'�A&n�A%K�A%%A$ȴA$z�A#�7A#33A"��A"�A"jA!�A!O�A ��A (�A�wAS�A(�AC�AAt�A��A`BA�`A5?A|�AZAS�A$�A�A��A��AM�A�-AVA
��A
�A	��A�/A�#A^5AG�A�9A-A�mA�7A"�A9XA��A`BA;dA�A �A �A �!A ff@��F@�|�@�33@���@�ff@�-@���@��-@��7@�hs@��@�  @�33@�r�@�
=@�V@�@�/@�r�@�F@���@�@�p�@��@�o@홚@��@�V@�Ĝ@��@�t�@�"�@���@�\@�@�Ĝ@��;@��@� �@�&�@�o@�=q@�@�X@ܴ9@��m@�
=@ڗ�@�hs@��
@ם�@�dZ@�~�@�ƨ@��/@�j@�I�@�"�@��/@���@ɺ^@���@�1@��@ư!@Ƈ+@�n�@�M�@�{@ũ�@�hs@���@Ĵ9@�A�@þw@�ff@���@��-@���@�O�@�Ĝ@�z�@�9X@�  @�l�@�V@���@�p�@���@���@�Ĝ@�z�@���@���@�^5@���@�7L@��@���@�l�@���@�-@�p�@��j@�I�@�I�@�9X@��@���@��@�33@�
=@���@�j@���@�dZ@�o@���@�E�@�J@��@��-@�x�@��`@�o@���@�^5@�E�@�E�@�=q@�@��7@��@��/@��@���@��D@�r�@�j@�Z@�A�@�(�@��@���@�K�@�ȴ@��@�hs@���@��@�33@���@�v�@�^5@�J@�J@���@��@��@���@���@�bN@�Q�@�Q�@�Z@���@��D@��@�j@� �@�\)@���@���@���@��!@���@���@���@���@���@��\@���@���@��@�O�@��@���@�1'@�  @�ƨ@��@�C�@��H@���@�  @�S�@�K�@�+@�o@���@�n�@�$�@�5?@��#@�hs@�j@��P@�~�@�x�@���@�Z@�1'@�  @��@�\)@�"�@�n�@�v�@�n�@�$�@�V@�^5@�V@�E�@�@�X@���@�l�@�;d@�"�@���@�n�@��@��7@��@��@���@�z�@�(�@�  @��;@���@�|�@�
=@���@�n�@�M�@�@��@��^@�`B@��@�V@�%@���@��@���@���@�Ĝ@��@��@���@���@��D@���@�|�@��@��@��y@���@��!@���@�-@��#@���@��7@�p�@�O�@�7L@���@�A�@�w@l�@K�@;d@;d@;d@�@~�y@~�y@~�@~ȴ@~ȴ@~�R@~��@~��@~�+@~�+@~ff@~V@}��@}`B@}�@|�/@|�j@|z�@|I�@|I�@{ƨ@{C�@z^5@yhs@x�`@x��@xbN@w\)@v@u@u��@u�@up�@u`B@u/@t�@t��@t�D@t�D@t�D@tz�@tI�@tI�@tI�@t1@s@r=q@q��@p��@p�9@p�@p�@pbN@pbN@pb@ol�@o;d@n�@nV@m�@m�h@l��@lz�@l(�@kƨ@kdZ@j�H@j�\@jn�@jM�@j-@i�#@i�7@i�7@iX@h��@h�u@h1'@g��@gl�@gK�@f�R@fv�@e�-@e�@eV@d��@d�@c�@ct�@ct�@b��@a��@a��@a��@a�7@`�`@`��@`Q�@_��@_
=@^�R@^��@^��@^��@^5?@]`B@\�/@\��@[�m@[ƨ@[dZ@Y��@X��@XĜ@X�9@XĜ@X�9@X��@XbN@Xb@W�w@W
=@Vv�@VE�@V{@U�-@T�j@S�@S@R��@R�!@R^5@Qx�@PĜ@PĜ@P��@P�u@O�@O+@O�@O|�@O�@N��@N5?@M@M�-@M�h@MO�@MV@L�/@L��@Lz�@Lj@LZ@L9X@L�@L1@K�m@K��@Kt�@K�@K33@K@J�\@I�@I��@I�@H�`@HĜ@HĜ@H�u@HA�@G�;@G��@G\)@G
=@F�@F��@F�+@FV@F5?@F@E�T@E@E�-@E�h@E�@E`B@Dj@C��@C�m@Cƨ@C�F@CC�@B��@B�\@Bn�@B^5@B^5@BM�@B�@A�#@A��@A��@A��@A�^@A��@A��@A%@@�9@@�@?�@?��@?K�@>��@>��@>V@=��@=�h@=p�@=`B@=`B@=�@<�@<��@<�D@<z�@<(�@;dZ@;o@:�H@:�\@9�@9x�@9&�@8��@8��@8Ĝ@8�@8  @7��@7;d@7
=@6ȴ@6�+@6V@6$�@5��@5��@5V@4I�@3t�@2�@2�!@2�\@2��@2��@2~�@1x�@0��@0Ĝ@0�9@0��@0��@0A�@0  @/��@/l�@/\)@/+@.ȴ@.��@.��@.v�@.V@.5?@.$�@.@-��@-��@,��@,9X@+�
@+�@+C�@*��@*�\@*^5@*=q@*M�@*M�@)�#@)��@)�7@)x�@)hs@)hs@)hs@)X@)%@(1'@'�P@'|�@'|�@'\)@';d@'
=@&��@%�@%�@$�/@$�@$�D@$j@$9X@#��@#S�@#@"��@"��@"��@"��@"��@"��@"��@"�!@"�!@"��@!��@!�#@!��@!��@!X@!7L@ ��@ A�@�@�@��@�P@|�@|�@l�@;d@�@V@E�@5?@@�@�T@�T@�T@�T@�T@��@�-@��@��@�h@�h@�@`B@?}@��@z�@Z@I�@1@�F@t�@o@��@n�@=q@�@7L@Ĝ@Ĝ@��@r�@Q�@A�@A�@A�@1'@1'@ �@�@��@l�@K�@�y@{@�T@p�@V@�@�@9X@1@��@��@1@ƨ@t�@S�@33@�H@��@^5@=q@�@�@�@�@J@J@J@��@��@�#@��@x�@7L@7L@�@��@Q�@A�@1'@b@  @��@�P@\)@K�@K�@+@�R@��@�+@V@E�@5?@$�@{@{G�O�A��A��PA��A��+A��hA��A�|�A��A���A���A���A��hA��PA��A��A��A��\A��PA��uA��uA���A���A���A���A���A���A���A��A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��-A��!A��!A��A���A��A���A��A��!A��!A��A��!A��!A��!A��A��!A��A��-A��-A��-A��-A��-A��!A��!A��-A��-A��-A��-A��9A��FA��FA��9A��-A��9A��FA��FA��RA��FA��9A��9A��FA��^A��FA��9A��RA��^A��RA��9A��FA��RA��9A��FA��-A��!A��-A��9A��A��9A��-A��!A��9A��!A��FA��-A��9A��-A��-A��-A��A��-A��!A��A��A��-A��-A��A��A��!A��A��A��-A��A��A��A��!A��A��A���A���A���A��A��-A��!A���A���A���A���A���A��\A�Q�A�?}A�5?A�+A�A���A��A��HA��A���A��jA��A���A��7A�~�A�p�A�r�A�x�A�\)A�33A�-A�bA��/A��/A�ƨA��A���A��hA�p�A�ffA�XA�C�A�A�A�;dA�1'A�&�A��A�bA�A���A��A��A��yA��/A��A�A��FA���A���A���A�|�A�
=A��;A��jA��!A���A���A���A���A���A���A���A��7A��A��A��A��A�~�A�|�A�~�A�~�A��A�|�A�z�A�x�A�hsA�bNA�G�A�A�A�;dA�1'A�(�A��A�  A��;A���A��^A��9A���A���A��hA��A�p�A�jA�jA�jA�bNA�`BA�`BA�\)A�XA�VA�VA�VA�VA�XA�XA�VA�S�A�S�A�Q�A�O�A�G�A�;dA�33A�33A�$�A��A��A��A��A�oA�  A���A���A��A��yA��`A��TA��HA��;A���A���A���A��A�\)A�5?A��A��A�A��mA��/A���A��RA���A��PA�x�A�p�A�n�A�hsA�dZA�`BA�`BA�`BA�XA�Q�A�I�A�A�A�&�A�1A���A�ƨA�|�A�JA��RA��DA�ZA�9XA��A���A���A��A��A���A�&�A��A���A���A�|�A�`BA�G�A�=qA�&�A�JA�A�A�A���A��A��A��A��yA��yA��A��A��yA��mA���A��wA���A��hA�VA�?}A�7LA�JA��;A���A�O�A�A��HA���A�ƨA��uA�|�A�t�A�?}A�bA���A��9A��-A��-A��A���A���A���A���A��\A�l�A�S�A�K�A�;dA�7LA�+A�1A���A��mA���A�C�A��wA� �A���A�\)A�A���A��DA�\)A�33A� �A�VA��A���A��7A�M�A��A�  A���A��mA���A�ĜA��RA���A��uA�x�A�G�A�-A��A�1A���A��A���A���A�bNA��A���A��mA�ƨA���A�p�A�G�A��A�Q�A���A��RA�G�A��!A���A�|�A�A�A�bA���A���A�dZA�?}A�(�A��/A���A��+A�v�A�^5A�$�A���A�-A�bA�  A��yA��mA��#A���A��#A���A��jA��^A��wA��^A��^A���A���A��7A�t�A�bNA�\)A�O�A�S�A�O�A�S�A�O�A�Q�A�O�A�M�A�O�A�M�A�O�A�K�A�M�A�G�A� �A���A��A�jA�$�A��#A��hA�I�A�%A�~�A�E�A��A�%A���A��A��^A���A��A�dZA�A�bNA�E�A�C�A�=qA�;dA�9XA�7LA�/A� �A���A��A��jA���A��A�n�A�`BA�O�A�?}A�&�A�ȴA��A�VA���A���A�$�A���A���A�l�A�5?A�/A�&�A��A�JA�%A���A���A���A���A��A��A��A��A��A��;A���A��9A��\A�bNA�C�A�A���A�O�A�JA�A�A���A�ƨA���A��DA�VA�%A��A��`A��#A���A���A���A��+A��A��A�ffA�$�A��A��A��A��A�{A�{A�oA�JA�
=A���A��yA��;A�ȴA��-A���A���A���A���A���A���A���A���A���A���A��hA��\A�x�A�/A� �A�bA���A��A��RA���A�VA���A���A�A��9A�ȴA���A���A��
A���A��wA��!A��uA�~�A�VA�=qA�K�A�^5A�O�A�G�A�=qA�5?A���A�r�A�9XA��yA���A�S�A�VA��A��mA��HA���A���A��jA��-A���A���A���A���A��PA��A��A�r�A�p�A�dZA�\)A�S�A�9XA�JA�  A�A�  A�  A���A��wA�ffA���A��mA���A�z�A���A��;A���A��A���A�x�A�bNA�bNA�hsA�l�A�S�A�XA�M�A�C�A�G�A�A�A�?}A�9XA�=qA�9XA�33A�+A�&�A��A�{A�{A��A��A��A��A��A��A�oA��A��A�VA�
=A�VA�bA�bA�VA���A��A��;A�t�A�A�A�&�A�A�ƨA�~�A�9XA� �A���A�VA���A��;A���A���A���A��jA��wA��RA��FA��-A���A���A���A���A���A��PA��7A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL0BL�BJ�BL�BK^BJ�BI�BI�BJ�BJ�BJ#BJXBJXBI�BI�BI�BJXBI�BJ#BI�BJ#BJXBJ�BK)BJ�BK)BJ�BJ�BH�BH�BJ�BMBiB�B�qB��BҽB�2B 4B%B	B
	BB�B�BxB�BBBPB
	B_B�B	B��B�B�pB�B�B�B�)B�vBȴB�tB�HBɺB�HB��B��B��B��B�OB��B�{B}"Bm�BjBd�B`BE�B1�B 'B	7B��B�dB��B��B��B�B��B�qB��B��BtBe�B`BXEB@�B8B3�B0UB-wB$@B
=B��B�B��B� B�B��B��B�?B�$B�!B�DB�GBs�Be�BO�BH�B@OB<�B/OB"hBBBDB  B
��B
�B
�)B
��B
�<B
��B
��B
�OB
��B
��B
�uB
�+B
��B
��B
��B
��B
�@B
�B
��B
��B
~]B
{JB
xB
x�B
e�B
aHB
[�B
V�B
U�B
RTB
P}B
N<B
H�B
EmB
;�B
6�B
4�B
1[B
0�B
&�B
!bB
"�B
�B
{B
�B
�B
�B
�B
�B
�B	�(B	�B	�>B	��B	��B	�B	�B	�cB	�8B	��B	�ZB	��B	�|B	�B	��B	�/B	��B	�QB	�B	�2B	� B	бB	͟B	��B	�EB	� B	�B	�<B	��B	�B	��B	��B	��B	��B	�B	�hB	��B	��B	�~B	�"B	�rB	��B	�B	��B	��B	�fB	�JB	��B	�=B	�xB	�B	��B	��B	��B	��B	��B	��B	�B	�%B	�SB	��B	��B	��B	��B	�7B	��B	�fB	��B	�1B	��B	��B	��B	��B	��B	�%B	��B	�B	��B	��B	�B	��B	�B	��B	��B	�SB	��B	�	B	�7B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�~B	��B	��B	�DB	��B	�B	�SB	�fB	�DB	��B	�.B	��B	�MB	��B	�B	� B	�B	�(B	�.B	��B	�4B	��B	��B	��B	�B	��B	�UB	��B	��B	�?B	�zB	��B	��B	�OB	ŢB	�EB	��B	ӏB	��B	��B	�5B	��B	�pB	�ZB	�B	�B	��B	��B	�iB	��B	�|B	�`B	��B	�xB	��B
B
�B
�B

�B
B

=B
xB
�B
�B
B
_B
�B
	B
�B
CB
B
�B
�B
"hB
#B
+�B
,=B
/�B
1�B
3�B
7B
9XB
:^B
;0B
<B
<jB
?�B
HB
I�B
JXB
J�B
J�B
J�B
K�B
NpB
O�B
Q�B
R�B
S�B
TaB
UgB
V9B
V�B
WsB
X�B
]�B
`�B
c�B
c�B
c B
c�B
gB
l�B
n/B
sMB
t�B
w2B
.B
�B
��B
��B
�=B
��B
�B
��B
��B
�hB
�oB
��B
��B
��B
�eB
��B
��B
��B
�-B
��B
�4B
�hB
�B
�nB
�@B
�tB
�B
�qB
�wB
��B
��B
��B
��B
�B
��B
��B
�tB
�9B
�aB
��B
��B
��B
��B
��B
��B
��B
�*B
�0B
�qB
�BB
�qB
�B
�B
��B
��B
B
�aB
�-B
�[B
��B
��B
�zB
��B
�6B
ϫB
�TB
�mB
�yB
��B
ںB
�WB
�#B
��B
�]B
�dB
�jB
�BB
��B
��B
�QB
��B
�5B
�vB
�|B
��B
�%B
��B
��B
�B
��B
�VB
��B  BABBABMB�B�B�B_BfB	B�B	�B
=B
�BB
�B
�B�B�B�B�BBB�B�B�B=B=BBB�BBIB �B!�B"�B#nB#�B#�B$@B%FB&B&B&B&�B&�B&�B'RB'B'�B'�B'�B'�B)�B)�B+6B+6B+6B,B-B,=B,�B-�B.�B/�B0UB0UB0UB1�B33B3hB3hB3hB3�B3�B5tB5�B5�B5�B6B6FB6�B8B8B8�B8B9�B7�B8B7�B8�B9�B9�B<B<B<B<6B<jB;�B;�B<jB<�B=�B?�BAUBB[BB�BD3BDgBD�BD�BE�BE�BF�BF?BF?BGBG�BH�BH�BHBIBJ#BI�BK�BK^BK�BK�BJ�BM�BMBLdBOBOBOvBOvBPHBR�BR BR�BS�BU2BV9BVmBVBV�BXBW?BVmBV�BV�BV9BWsBYBY�BZBY�BYBYBY�BZ�BZQBZQB[�B\)B[�B[�B\]B]dB^�B]�B^jB]�B]�B_�B`vB`BB`BBaHBa�Bb�Bd�Bf�Bg�BhsBi�BjBjKBjKBkBk�Bk�Bl"BlWBl"Bl"BlWBl�Bl"Bl�Bm]Bm]Bl�Bm]Bl�Bm]Bm�Bl�Bl�Bm)Bm)Bm)Bm]Bm�Bm�Bn/Bn�Bo Bo Bo Bo5BoiBoiBo�Bo�BpBo�BpBpBp;Bp;Bo5Bo Bo5BoiBp�BqvBqBq�Bq�Bq�Bq�Bq�Bq�Bq�BrBr|Bq�BqvBq�BrBrGBrGBq�BrBrBr�BsMBsMBs�Bs�Bs�BsBsBs�BtBs�Bs�Bs�Bt�Bu�Bu�Bu�Bv+Bv�Bv+Bv�Bv�Bv�Bv+Bw2Bw2Bw2BxBxBw�Bx�Bx�ByrBy�By�BzBzDBz�B{B{�B{�B{�B{�B{�B}"B}�B}�B}�B}�B}�B~�B~�B.B�BcB�B�4B��B�4B��B�iB��B��B��B��B�B�B�uB�B�B�GB��B�MB��B��B�MB��B�SB�B��B�SB��B�%B�SB�SB�%B��B��B��B��B��B�1B��B��B��B�=B�	B��B��B��B�B��B�JB�~B�B��B��B��B��B��B�JB��B�B��B��B��B��B��B��B�VB�VB�\B��B�.B�.B�.B��B��B��B�bB��B�hB�hB��B��B�:B�:B��B�:B��B�:B��B��B�oB��B�B�oB��B��B�@B��B�FB�FB�B��B��B��B�B�MB��B��B�SB��B�+B��B�_B�+B��B��B��B�_B��B�_B��B��B�1B�eB�eB�7B��B��B��B�xB��B�CB�IB�~B�B��B�B��B��B�~B��B��B��B�!B�!B��B��B��B��B��B�VB��B�\B�\B�\B�\B�bB��B��B��B��B��B�4B�4B��B�hB��B�:B�:B�B�:B��B��B�tB��B�tB��B�B�FB�FB��B��BK�BIRBMjBH�BH�BP}BM�BK^BFBK)BK)BK�BJ�BMjBM�BM6BI�BK�BJ�BL0BL�BI�BI�BJ�BI�BJ�BIBIRBJ�BI�BI�BLdBK^BIRBI�BJ�BK�BI�BK^BIRBH�BK�BK�BI�BH�BI�BJ�BJ�BJ�BH�BHKBI�BIRBJXBIBHKBIRBI�BIBI�BJXBJ�BJ�BI�BI�BK�BK)BI�BIBJXBI�BJ�BK)BI�BIRBIRBJXBJXBJ�BJ�BJ�BJ�BJXBIBIBI�BH�BI�BJ�BK�BIBK^BK)BJ�BK^BK^BK)BK)BJ�BK�BJXBJ�BIRBK�BK�BK^BL0BK�BK�BK�BK�BK)BJ�BJXBI�BJXBJ�BK�BK�BK^BJ�BJ#BJ�BK�BL0BJ�BI�BK�BK�BK)BJ#BJ�BMBJ�BI�BK)BK^BK^BK^BJ�BJXBK�BJXBJXBK^BI�BK�BI�BK^BI�BK)BJ�BGzBI�BIBE�BHKBH�BF�BIBH�BF?BF�BJXBK�BI�BJ�BK)BK^BJ#BH�BI�BL�BL�BN�BJ�BIRBI�BL�BMjBO�BR�BLdBS�Bd�Bd�Bg�Bi�BsBsMBv�BxlBzDB~�B�;B�AB��B�B�B�.B��B�xB�SB�xB��B��B��B��B�UB�3B�zB��B�[B��B�zB�0B�0B�B�B��B��B�B��BߤB�|B�BB��B��B�B�]B�B�%B��B��B��B�B�B_B�B+BB�B�B�B�BB�B�B�B�B1B	lB
	B	lB�B1B	7B	lB	�B~B
rB
�B
rBDB~B
rBB�B�B�B�BB�B�B�B�B�BBxB
=BBBxB~B~B~BJB�BDB
�B
�B
�B
rB
	B
	B	�BDB�B�BB�BBBB�BxB�BPB�BxB�B�BJB�B�B�B�B
=B�B�B�B�B�B�B�B1B�BB�B
�B�B+BYBYB�B�B�B�B�BGBuB{BSBfB�B4B�B�B{BB;B�"B��B�BB��B�B�B��B�5B�8B�mB� B��B�pB�B�TBޞB��B�B�jB��B�BB�5B�jB�B�pB�jB�jB�vB�B�B��B�B�fB�/B�DB�B�B�
B�;B�B�B�B��B՛B�;B��B��B՛B��B��B�BɆBʌB��B��B�EBȴB�RB�?B͟B�tB�9B�zB��B�9BɺB�3BB��B�vB�5B��B��B�TB�vB�-BĜB��B�HB��B�B��B��B�}B�'B��B�*B�B�RB�nB��B��B��B��B��B�FB�CB�B�=B��B��B��B�wB��B��B��B��B��B��B�qB��B�CB��B�@B�MB��B��B�lB�%B��B�YB��B��B��B|�B~�B��B{Bw�Bw�Bu�B~(B��Bt�Bl�Bm]Bp;Bl�Bm�Bm�BjKBjKBo Bj�Bh�BjBjBm�Bh�BiDBk�Bf�Bh
BgBe,BffBd&Be�BcTBc�Bc�Ba�Ba�B_�B_�B\�B\�Bd�BpBj�BcTBV�BR�BR BM�BPBPBB�BEmB<�B<�B<6B>wB7�B9XB8BA�B=B/OB)�B)�B&�B'B%�B"�B$�B9$B*0BIBB�B$B:BB�B(B�B/�B�B�(B�lBPB�B�B��B�B�B�B�ZB�&B��B�BߤB�B��B��B�/B��B�dB��B��B�)B�]B�/B�KB�B��B҉B�mB�BǮB�gB��B�,B��B�B��B�9B�0B�dB�XB��B��B�B��B��B��B��B��B��B��B�UB�}B��B�B�B��B�B��B�IB�6B�B�=B��B�RB�XB��B�B��B��B�LB��B�FB��B�4B��B��B��B�!B��B�B�oB��B��B�"B�PB��B�YB�7B��B��B��B�4B��B�JB��B�B��B��B� B�1B�SB��B�B��B��B�+B~]Bu�Bl�BwfBo Bm�Bh�BjBjKBi�Bi�BgBgBe�BdZBd�Be�Bc BaBc�B`�BbB`BB_�Bb�Ba�BZ�BXEBX�BWsBXBbB^jBV�BNpBM�B\�BM�BB�B=�BH�B?�B7LB>wB9�B8�B8RBB'B5?B:�B7LB49B6�B4�B4B5�B2�B5B2�B4B0�B1�B1�B/B33B0!B0�B0�B.�B1'B-�B.IB1'B-�B-�B,�B-CB+�B-wB*0B'�B-�B+B"�B"�B!�B�BFBB 'B�B�B�B  B�.B iB��B�(B��B�VB��B�VB�VB��B�B�B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202110151929172021101519291720211015192917202110151929172021101519291720211015192917SI  SI  ARFMARFM                                                                                                                                                2021031515310020210315153100IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040122013720210401220137QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021040122013720210401220137QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074620211014130746IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021101519292120211015192921IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                