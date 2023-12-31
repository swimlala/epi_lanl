CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-11-01T01:09:24Z creation; 2020-06-20T00:40:12Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191101010924  20210326170159  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7836_008777_018                 7836_008777_018                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @����r@����r11  @����@����@<���0@<���0�d�,'�7�d�,'�711  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@B�\@�  @�G�@�G�@�G�A�AG�A ��A,(�A@  A`  A���A�  A��A�  A�  AϮA�Q�A��B Q�B(�B  B  B Q�B((�B0(�B8(�B@  BHQ�BP(�BX  B_�
Bh  Bp(�Bx  B�
B�  B�{B��B��B��
B�  B��B�  B�(�B�(�B�(�B�{B��
B��
B�  B�(�B�  B�  B�{B�  B�  B��B�{B�{B��B�  B�{B��B��B�{B�{B��C�C��C  C�C
  C  C  C  C��C  C
=C  C��C
=C  C   C!��C$  C%��C'��C*  C+��C.
=C0  C1��C3��C5�C7�C:  C<{C>  C@  CB
=CD
=CE��CH  CJ{CL
=CN  CO��CQ��CT
=CV  CX{CZ{C\
=C^  C_��Cb  Cd
=Cf  Ch  Ci��Cl  Cn
=Cp  Cr
=Ct  Cv  Cx
=Cz  C|  C~  C�C�C���C�  C�  C�  C�C�C�C�C�C�C�C�  C�  C�C�  C��C���C���C�  C�C�
=C�
=C�C�C�  C�  C�
=C���C�  C�C�
=C�
=C�C�C�  C���C�  C���C�  C���C���C�C�C�C�  C���C���C���C���C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�  C�  C�  C�  C�C�  C���C�C�C���C���C��C���C���C�  C�C�C�  C�  C���C�  C���C���C�  C���C�  C�C�  C�  C���C��C���C�
=C�
=C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�
=C�\C�
=C�C�  C���C���C���C�C�\C�\C�  C���C���C�  C���C�C�  C�  C�  C���D }qD ��D� D��D� D�D��D�D� D�D��D�D}qD  D��D  Dz�D�qD	� D
  D
z�D
�qD}qD  D�D�D��D�D}qD��D}qD  D��D  Dz�D�qD� D  D� D  D��D  D� DD� D  D}qD�qD}qD�D}qD�qD��D  D� D  D��D  Dz�DD� D  D��D D ��D!  D!}qD"  D"}qD#  D#z�D#��D$� D$��D%}qD%�qD&}qD'  D'�D(D(��D)�D)��D*  D*� D*�qD+� D+�qD,� D-�D-�D.  D.}qD/  D/��D0  D0� D1  D1� D2  D2� D3�D3z�D3�qD4��D5D5��D6  D6��D7  D7}qD8  D8��D9  D9� D:�D:�D;�D;��D<�D<� D<�qD=}qD>�D>��D?  D?� D?�qD@}qDA�DA�DB�DB� DC  DC��DC�qDD}qDE  DE��DF�DF��DG�DG��DH�DH}qDI  DI}qDI�qDJ��DK�DK� DK�qDL}qDMDM� DM�qDN��DO�DO� DO��DPxRDP�RDQz�DQ��DRz�DS  DS� DS�qDT� DU�DU��DV�DV� DV�qDW}qDW�RDXz�DY  DY��DZ�DZ}qDZ��D[��D\�D\}qD\�qD]� D^�D^� D_  D_�D`  D`� DaDa�Db  Db}qDc  Dc��Dd  Dd� De�De��DfDf� Df�qDgz�Dg�qDh��Di  Di� Dj�Dj��Dk  Dkz�Dl  Dl��Dl�qDm� Dm�qDnz�Dn��Do� DpDp�DqDq��Dr�Dr��Dr�qDsxRDs�qDt}qDu  Du��DvDv� Dw  Dw� Dx�Dx��DyDy��Dz�Dz� D{�D{}qD{��D|}qD}  D}��D~D~��D  D}qD��D�>�D�~�D�� D�HD�@ D�~�D���D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�B�D���D�D�HD�AHD��HD���D��qD�@ D�� D�� D���D�@ D�� D�� D��D�@ D�}qD���D�  D�=qD�|)D��qD��D�B�D�� D�� D�HD�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD���D���D��D�AHD�� D���D�HD�AHD��HD��HD���D�@ D�� D�� D�  D�@ D�� D��HD��D�B�D�~�D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D���D�=qD�~�D�� D�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�>�D���D��HD���D�>�D�� D�� D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�AHD���D��HD�HD�@ D���D�D�  D�>�D�� D�D�HD�@ D�� D�� D�  D�@ D�� D��HD���D�>�D�~�D�� D�  D�>�D�� D���D���D�AHD�� D���D�HD�AHD�~�D�� D�  D�@ D��HD��HD��D�AHD��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�@ D�� D��qD�  D�AHD��HD�� D�  D�C�D���D�D�  D�@ D�� D���D��D�C�D���D��HD�HD�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D���D�=qD�~�D���D��qD�=qD�}qD���D�HD�AHD��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D��HD��D�AHD�~�D�� D�HD�@ D�}qD�� D�  D�@ D�� D��qD��qD�>�D��HD�D��D�AHD���D�� D���D�=qD�}qD�� D�HD�>�D�}qD���D�  D�@ D�� D��HD�HD�B�D��HD���D�  D�B�DHD�� D��D�AHD�}qD��HD�  D�=qDāHD�D�HD�@ D�~�D��HD�  D�@ D�~�D��HD��D�@ D�~�D��HD�HD�>�D�~�D��HD��D�@ D�~�D��HD�  D�@ Dʂ�D��HD�  D�=qD�}qD˾�D�  D�@ D̂�D�� D�  D�@ D̀ D��HD���D�@ D�~�D�D�  D�>�D�~�DϾ�D�  D�@ DЂ�D�� D�HD�AHDр D�D�  D�>�DҀ D�� D�HD�AHDӁHD�� D�  D�AHDԁHDԽqD���D�>�D�~�Dվ�D���D�@ DցHD��HD�HD�>�D׀ D�� D�HD�AHD؂�D�� D���D�@ D�~�Dپ�D�  D�@ Dڀ D�� D��qD�<)D�~�D��HD��D�@ D܀ Dܾ�D�  D�AHD݁HD�D�  D�>�DށHD�� D�  D�>�D߀ D��HD�HD�AHD��HD�� D�  D�=qD�~�D��HD��D�>�D� D�� D�  D�AHD�HD㾸D��)D�@ D�HD��HD�  D�@ D�HD��HD���D�>�D�~�D�� D�  D�=qD� D羸D���D�>�D�~�D��HD�  D�>�D�HD�� D���D�>�D� D�� D���D�@ D�~�D�� D�HD�B�D�HD쾸D��qD�>�D� D���D���D�@ D� D��HD�HD�B�D�HD�� D���D�@ D���D�D�HD�AHD�HD�� D���D�@ D�}qD�qD���D�@ D�HD�D��D�AHD� D���D�HD�@ D�}qD���D�HD�=qD�~�D���D���D�=qD�� D��HD���D�>�D��HD�� D���D�@ D�~�D��HD�  D�=qD�� D���>���?B�\?k�?�=q?���?�G�@�\@��@(�@.{@E�@W
=@aG�@n{@��\@���@�33@���@��
@�{@�@�p�@\@�{@�
=@�\@���@�\)@�Q�A�A�A
�HA{A�\A�Ap�A"�\A'
=A*=qA.�RA333A8Q�A?\)AC33AEAJ=qAP  AVffAZ�HA_\)Ac33Ag
=Al��As33Aw�A{�A�  A�33A�{A�  A��A�z�A��A��\A�z�A�
=A���A��A�ffA���A��
A�p�A�  A��HA�{A���A��\A�z�A�
=A�=qA���A��A�=qA�(�A�ffA���A�(�A�
=A���AӅA�A�Q�Aڏ\A�A���A�33A�p�A�A陚A�z�A�\)A�=qA��
A�A�\)A�=qA�z�A��RB Q�B ��BB�\B�
B��B�B�\B33B(�B	�B
=qB\)B�
B��Bp�B=qB33Bz�Bp�B{B�HB�Bz�B��BffB
=B�B��BB�HB�
Bz�BG�B�B�RB�B ��B!�B"�\B#33B#�
B$��B%�B&�HB(  B(z�B)G�B*{B*�RB+�B,z�B-��B.ffB/\)B0z�B1�B1��B2=qB2�HB3�
B4��B5B6�\B7
=B7�
B8z�B9G�B:=qB;33B<(�B<��B=B>�\B?\)B@(�B@��BABB�RBC�
BD��BEBF�HBG�
BH��BIBJ�RBK�BLz�BMp�BNffBO�BP(�BP��BQ�BR�RBS�BTz�BUG�BV{BV�HBW�
BX��BY��BZ�\B[�B\��B]��B^�\B_�B`z�Bap�Bb=qBc�Bdz�Be��Bf�\Bg�Bh��BiBj�RBk�
Bl��Bn{Bo
=BpQ�BqG�BrffBs�Bt��BuBw
=Bx  ByG�Bz=qB{�B|��B~{B33B�=qB���B�\)B�  B�ffB���B��B�  B���B�G�B��B��\B��B�B�ffB�
=B��B�=qB���B�\)B��B��\B��B���B�=qB��HB��B�(�B���B�p�B�(�B���B�\)B��B�z�B���B���B�Q�B���B��B�ffB���B��B�{B���B�G�B�  B��RB�p�B�(�B���B�\)B�  B��\B��B�B�z�B�33B��
B��\B�33B��
B�ffB�
=B��B�{B��HB��B�=qB���B��B�Q�B���B�p�B�  B���B�\)B�(�B��HB��B�{B���B�33B��B���B�\)B�(�B��RB�G�B��B��\B�\)B�  B��RB��B�{B���B�G�B��
B��\B�\)B�{B��RB�\)B��B�z�B�33B�  BĸRB�p�B�  B�z�B�\)B�(�B���B�\)B�  Bʏ\B�\)B�(�B��HBͅB�{BΣ�BυB�=qB�
=Bљ�B�{B��HBә�B�z�B�33B�B�Q�B���B�B؏\B�\)B��
B�Q�B�33B��BܸRB�\)B��Bޏ\B�\)B�(�B��HB�B�{B���B㙚B�ffB��B�B�=qB�
=B��
B�RB�G�B��B�\B�B�=qB���B�\)B�(�B��HB�B�=qB���B�B�ffB��B�B�Q�B�33B�  B��RB�\)B��B��RB��B�ffB���B��B�ffB��B�  B��\B��B��C \)C C{CQ�CC(�C�C�
C�C�C�CG�C��C�HCG�C��C{C\)C��C
=CffC�
C�Cp�CC	(�C	��C	�C
=qC
�\C
��C\)C�C��CQ�C�RC�CffC�C{C�C��C{Cp�C�HC=qCz�C��C33C��C�HC�C�\C��C=qCz�C�HCG�C��C�HC�C�C�HC33CffC��C�CG�Cp�C�\C�HC(�C\)C�C�RC  CG�Cp�C�\C�
C�C\)C�C�C��C=qCffC�C�
C�CG�CffC�C��C(�C=qC�CC  C33CQ�C��C�HC
=C(�Cz�C�RC�
C {C ffC �\C �C ��C!G�C!ffC!��C!�C"�C"=qC"z�C"��C#  C#(�C#Q�C#��C#�HC${C$=qC$ffC$�RC%  C%�C%=qC%�\C%�
C%��C&(�C&p�C&��C&C'
=C'\)C'z�C'�C(  C(�C(G�C(�\C(�
C(��C)�C)ffC)�C)�
C*  C*=qC*�C*��C*�
C+�C+\)C+�C+�RC,  C,=qC,ffC,�\C,�HC-{C-(�C-z�C-C-�HC.{C.\)C.�C.�C.��C/33C/=qC/��C/�
C/��C033C0�C0��C0��C1
=C1\)C1�C1��C1��C2=qC2ffC2�C2��C3{C3G�C3p�C3��C3�C433C4Q�C4�C4�
C5
=C5(�C5p�C5�C5�C6�C6=qC6�C6��C7
=C733C7Q�C7��C7�
C8(�C8Q�C8z�C8�RC9
=C9=qC9\)C9��C9��C:{C:G�C:��C:�
C:��C;=qC;�\C;�C;�C<=qC<ffC<��C<�C={C=G�C=�\C=��C=�C>33C>�C>��C>�HC?33C?ffC?�\C?�HC@{C@33C@�C@��C@�HCA33CAp�CA�\CA�HCB�CB=qCB�CB�
CC  CC(�CCp�CC�RCC�
CD�CDp�CD�\CD��CE�CEQ�CEp�CECF
=CF33CFffCF�CF��CG{CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         ?��@�\@B�\@�  @�G�@�G�@�G�A�AG�A ��A,(�A@  A`  A���A�  A��A�  A�  AϮA�Q�A��B Q�B(�B  B  B Q�B((�B0(�B8(�B@  BHQ�BP(�BX  B_�
Bh  Bp(�Bx  B�
B�  B�{B��B��B��
B�  B��B�  B�(�B�(�B�(�B�{B��
B��
B�  B�(�B�  B�  B�{B�  B�  B��B�{B�{B��B�  B�{B��B��B�{B�{B��C�C��C  C�C
  C  C  C  C��C  C
=C  C��C
=C  C   C!��C$  C%��C'��C*  C+��C.
=C0  C1��C3��C5�C7�C:  C<{C>  C@  CB
=CD
=CE��CH  CJ{CL
=CN  CO��CQ��CT
=CV  CX{CZ{C\
=C^  C_��Cb  Cd
=Cf  Ch  Ci��Cl  Cn
=Cp  Cr
=Ct  Cv  Cx
=Cz  C|  C~  C�C�C���C�  C�  C�  C�C�C�C�C�C�C�C�  C�  C�C�  C��C���C���C�  C�C�
=C�
=C�C�C�  C�  C�
=C���C�  C�C�
=C�
=C�C�C�  C���C�  C���C�  C���C���C�C�C�C�  C���C���C���C���C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�  C�  C�  C�  C�C�  C���C�C�C���C���C��C���C���C�  C�C�C�  C�  C���C�  C���C���C�  C���C�  C�C�  C�  C���C��C���C�
=C�
=C�  C�  C�  C���C���C�  C�  C�  C�  C���C�  C�
=C�\C�
=C�C�  C���C���C���C�C�\C�\C�  C���C���C�  C���C�C�  C�  C�  C���D }qD ��D� D��D� D�D��D�D� D�D��D�D}qD  D��D  Dz�D�qD	� D
  D
z�D
�qD}qD  D�D�D��D�D}qD��D}qD  D��D  Dz�D�qD� D  D� D  D��D  D� DD� D  D}qD�qD}qD�D}qD�qD��D  D� D  D��D  Dz�DD� D  D��D D ��D!  D!}qD"  D"}qD#  D#z�D#��D$� D$��D%}qD%�qD&}qD'  D'�D(D(��D)�D)��D*  D*� D*�qD+� D+�qD,� D-�D-�D.  D.}qD/  D/��D0  D0� D1  D1� D2  D2� D3�D3z�D3�qD4��D5D5��D6  D6��D7  D7}qD8  D8��D9  D9� D:�D:�D;�D;��D<�D<� D<�qD=}qD>�D>��D?  D?� D?�qD@}qDA�DA�DB�DB� DC  DC��DC�qDD}qDE  DE��DF�DF��DG�DG��DH�DH}qDI  DI}qDI�qDJ��DK�DK� DK�qDL}qDMDM� DM�qDN��DO�DO� DO��DPxRDP�RDQz�DQ��DRz�DS  DS� DS�qDT� DU�DU��DV�DV� DV�qDW}qDW�RDXz�DY  DY��DZ�DZ}qDZ��D[��D\�D\}qD\�qD]� D^�D^� D_  D_�D`  D`� DaDa�Db  Db}qDc  Dc��Dd  Dd� De�De��DfDf� Df�qDgz�Dg�qDh��Di  Di� Dj�Dj��Dk  Dkz�Dl  Dl��Dl�qDm� Dm�qDnz�Dn��Do� DpDp�DqDq��Dr�Dr��Dr�qDsxRDs�qDt}qDu  Du��DvDv� Dw  Dw� Dx�Dx��DyDy��Dz�Dz� D{�D{}qD{��D|}qD}  D}��D~D~��D  D}qD��D�>�D�~�D�� D�HD�@ D�~�D���D���D�>�D�� D�� D�  D�@ D�� D�� D�  D�B�D���D�D�HD�AHD��HD���D��qD�@ D�� D�� D���D�@ D�� D�� D��D�@ D�}qD���D�  D�=qD�|)D��qD��D�B�D�� D�� D�HD�>�D�� D��HD�  D�>�D�~�D�� D�  D�AHD���D���D��D�AHD�� D���D�HD�AHD��HD��HD���D�@ D�� D�� D�  D�@ D�� D��HD��D�B�D�~�D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D���D�=qD�~�D�� D�  D�>�D��HD��HD�  D�@ D�� D�� D�  D�>�D���D��HD���D�>�D�� D�� D��qD�>�D�� D�� D�HD�@ D�~�D�� D�  D�AHD���D��HD�HD�@ D���D�D�  D�>�D�� D�D�HD�@ D�� D�� D�  D�@ D�� D��HD���D�>�D�~�D�� D�  D�>�D�� D���D���D�AHD�� D���D�HD�AHD�~�D�� D�  D�@ D��HD��HD��D�AHD��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D���D���D�@ D�� D��qD�  D�AHD��HD�� D�  D�C�D���D�D�  D�@ D�� D���D��D�C�D���D��HD�HD�AHD�� D�� D�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D�� D���D�=qD�~�D���D��qD�=qD�}qD���D�HD�AHD��HD��HD�  D�>�D�� D�� D�HD�@ D�~�D��HD��D�AHD�~�D�� D�HD�@ D�}qD�� D�  D�@ D�� D��qD��qD�>�D��HD�D��D�AHD���D�� D���D�=qD�}qD�� D�HD�>�D�}qD���D�  D�@ D�� D��HD�HD�B�D��HD���D�  D�B�DHD�� D��D�AHD�}qD��HD�  D�=qDāHD�D�HD�@ D�~�D��HD�  D�@ D�~�D��HD��D�@ D�~�D��HD�HD�>�D�~�D��HD��D�@ D�~�D��HD�  D�@ Dʂ�D��HD�  D�=qD�}qD˾�D�  D�@ D̂�D�� D�  D�@ D̀ D��HD���D�@ D�~�D�D�  D�>�D�~�DϾ�D�  D�@ DЂ�D�� D�HD�AHDр D�D�  D�>�DҀ D�� D�HD�AHDӁHD�� D�  D�AHDԁHDԽqD���D�>�D�~�Dվ�D���D�@ DցHD��HD�HD�>�D׀ D�� D�HD�AHD؂�D�� D���D�@ D�~�Dپ�D�  D�@ Dڀ D�� D��qD�<)D�~�D��HD��D�@ D܀ Dܾ�D�  D�AHD݁HD�D�  D�>�DށHD�� D�  D�>�D߀ D��HD�HD�AHD��HD�� D�  D�=qD�~�D��HD��D�>�D� D�� D�  D�AHD�HD㾸D��)D�@ D�HD��HD�  D�@ D�HD��HD���D�>�D�~�D�� D�  D�=qD� D羸D���D�>�D�~�D��HD�  D�>�D�HD�� D���D�>�D� D�� D���D�@ D�~�D�� D�HD�B�D�HD쾸D��qD�>�D� D���D���D�@ D� D��HD�HD�B�D�HD�� D���D�@ D���D�D�HD�AHD�HD�� D���D�@ D�}qD�qD���D�@ D�HD�D��D�AHD� D���D�HD�@ D�}qD���D�HD�=qD�~�D���D���D�=qD�� D��HD���D�>�D��HD�� D���D�@ D�~�D��HD�  D�=qD�� G�O�>���?B�\?k�?�=q?���?�G�@�\@��@(�@.{@E�@W
=@aG�@n{@��\@���@�33@���@��
@�{@�@�p�@\@�{@�
=@�\@���@�\)@�Q�A�A�A
�HA{A�\A�Ap�A"�\A'
=A*=qA.�RA333A8Q�A?\)AC33AEAJ=qAP  AVffAZ�HA_\)Ac33Ag
=Al��As33Aw�A{�A�  A�33A�{A�  A��A�z�A��A��\A�z�A�
=A���A��A�ffA���A��
A�p�A�  A��HA�{A���A��\A�z�A�
=A�=qA���A��A�=qA�(�A�ffA���A�(�A�
=A���AӅA�A�Q�Aڏ\A�A���A�33A�p�A�A陚A�z�A�\)A�=qA��
A�A�\)A�=qA�z�A��RB Q�B ��BB�\B�
B��B�B�\B33B(�B	�B
=qB\)B�
B��Bp�B=qB33Bz�Bp�B{B�HB�Bz�B��BffB
=B�B��BB�HB�
Bz�BG�B�B�RB�B ��B!�B"�\B#33B#�
B$��B%�B&�HB(  B(z�B)G�B*{B*�RB+�B,z�B-��B.ffB/\)B0z�B1�B1��B2=qB2�HB3�
B4��B5B6�\B7
=B7�
B8z�B9G�B:=qB;33B<(�B<��B=B>�\B?\)B@(�B@��BABB�RBC�
BD��BEBF�HBG�
BH��BIBJ�RBK�BLz�BMp�BNffBO�BP(�BP��BQ�BR�RBS�BTz�BUG�BV{BV�HBW�
BX��BY��BZ�\B[�B\��B]��B^�\B_�B`z�Bap�Bb=qBc�Bdz�Be��Bf�\Bg�Bh��BiBj�RBk�
Bl��Bn{Bo
=BpQ�BqG�BrffBs�Bt��BuBw
=Bx  ByG�Bz=qB{�B|��B~{B33B�=qB���B�\)B�  B�ffB���B��B�  B���B�G�B��B��\B��B�B�ffB�
=B��B�=qB���B�\)B��B��\B��B���B�=qB��HB��B�(�B���B�p�B�(�B���B�\)B��B�z�B���B���B�Q�B���B��B�ffB���B��B�{B���B�G�B�  B��RB�p�B�(�B���B�\)B�  B��\B��B�B�z�B�33B��
B��\B�33B��
B�ffB�
=B��B�{B��HB��B�=qB���B��B�Q�B���B�p�B�  B���B�\)B�(�B��HB��B�{B���B�33B��B���B�\)B�(�B��RB�G�B��B��\B�\)B�  B��RB��B�{B���B�G�B��
B��\B�\)B�{B��RB�\)B��B�z�B�33B�  BĸRB�p�B�  B�z�B�\)B�(�B���B�\)B�  Bʏ\B�\)B�(�B��HBͅB�{BΣ�BυB�=qB�
=Bљ�B�{B��HBә�B�z�B�33B�B�Q�B���B�B؏\B�\)B��
B�Q�B�33B��BܸRB�\)B��Bޏ\B�\)B�(�B��HB�B�{B���B㙚B�ffB��B�B�=qB�
=B��
B�RB�G�B��B�\B�B�=qB���B�\)B�(�B��HB�B�=qB���B�B�ffB��B�B�Q�B�33B�  B��RB�\)B��B��RB��B�ffB���B��B�ffB��B�  B��\B��B��C \)C C{CQ�CC(�C�C�
C�C�C�CG�C��C�HCG�C��C{C\)C��C
=CffC�
C�Cp�CC	(�C	��C	�C
=qC
�\C
��C\)C�C��CQ�C�RC�CffC�C{C�C��C{Cp�C�HC=qCz�C��C33C��C�HC�C�\C��C=qCz�C�HCG�C��C�HC�C�C�HC33CffC��C�CG�Cp�C�\C�HC(�C\)C�C�RC  CG�Cp�C�\C�
C�C\)C�C�C��C=qCffC�C�
C�CG�CffC�C��C(�C=qC�CC  C33CQ�C��C�HC
=C(�Cz�C�RC�
C {C ffC �\C �C ��C!G�C!ffC!��C!�C"�C"=qC"z�C"��C#  C#(�C#Q�C#��C#�HC${C$=qC$ffC$�RC%  C%�C%=qC%�\C%�
C%��C&(�C&p�C&��C&C'
=C'\)C'z�C'�C(  C(�C(G�C(�\C(�
C(��C)�C)ffC)�C)�
C*  C*=qC*�C*��C*�
C+�C+\)C+�C+�RC,  C,=qC,ffC,�\C,�HC-{C-(�C-z�C-C-�HC.{C.\)C.�C.�C.��C/33C/=qC/��C/�
C/��C033C0�C0��C0��C1
=C1\)C1�C1��C1��C2=qC2ffC2�C2��C3{C3G�C3p�C3��C3�C433C4Q�C4�C4�
C5
=C5(�C5p�C5�C5�C6�C6=qC6�C6��C7
=C733C7Q�C7��C7�
C8(�C8Q�C8z�C8�RC9
=C9=qC9\)C9��C9��C:{C:G�C:��C:�
C:��C;=qC;�\C;�C;�C<=qC<ffC<��C<�C={C=G�C=�\C=��C=�C>33C>�C>��C>�HC?33C?ffC?�\C?�HC@{C@33C@�C@��C@�HCA33CAp�CA�\CA�HCB�CB=qCB�CB�
CC  CC(�CCp�CC�RCC�
CD�CDp�CD�\CD��CE�CEQ�CEp�CECF
=CF33CFffCF�CF��CG{CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�-@�}G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�Aɛ�A�=qA�+A�JA���A��A���A�AȺ^Aȩ�Aȝ�AȑhAȇ+A�~�A�x�A�v�A�t�A�p�A�l�A�n�A�l�A�jA�hsA�+A��A���A��AöFA���A��^A�M�A��A���A�/A��A�VA�?}A�=qA���A�n�A���A�5?A�JA�Q�A��jA��-A�oA���A�{A��hA��mA��PA�^5A��A�JA�n�A���A��!A�9XA�hsA���A�1A�n�A�$�A�x�A��A��A���A���A�Q�A�wA~(�A}�A|VAy��Ax�+Ax�Av1Aul�At�As�As+Ar��Aq��ApQ�Ao
=AnĜAn��Ao"�An=qAlAk�AkhsAkC�Aj�RAh�/Ag�Af9XAeC�Ae�AdĜAdffAcp�Ab�DAa|�A`Q�A_��A^v�A^9XA]�A\r�A[K�AZ�AZ��AZ1'AYAX��AXA�AV�AU�AU7LAT��AS��ARv�AP��AP=qAO;dAN�AM�AL�AK�#AKC�AJVAIS�AH�AH(�AG�TAF��AF�AE��AD(�AC\)AC
=ABffAA�wA@�jA?XA>ȴA>A�A=�7A<��A<�jA;�A:�uA9��A9��A9�^A9&�A7�^A6^5A5A4M�A3\)A2�RA2A1O�A0~�A/�wA/33A.~�A-��A-�A,z�A+�A*��A*=qA)p�A({A&�HA& �A$��A$A#�hA#?}A"z�A!hsA!A�#A7LA�TAA�A��A�HA�wA/A9XA�^AC�A��AE�A�wA%AbNA��A��AI�AhsA�DA��A��A;dAn�A��A�A��AjAJAt�AoA
�9A
E�A	��A	A�`A�AZA��A�A5?A��A�#A�A�DAJA+A �A �A ��A I�A b@�
=@�ff@��^@�A�@�=q@�/@��@�\)@�E�@�O�@���@��m@��@��/@�b@�w@�o@��@���@�bN@� �@�@�p�@�j@���@�C�@�M�@�x�@��/@���@�@�j@�\)@�$�@݁@ܛ�@�S�@���@��@��@���@�|�@�@Ь@�l�@�@���@˅@ʸR@�^5@Ɂ@�7L@��`@ȃ@�1@�K�@���@�~�@őh@��`@�z�@�1'@��
@�C�@�E�@�X@�A�@�dZ@��@���@�(�@�|�@�M�@�%@���@��@�|�@�K�@���@��D@��F@�S�@���@��7@�Ĝ@�  @�;d@�^5@��7@���@�(�@���@�C�@�v�@���@��@�I�@�C�@�E�@���@�O�@��@��/@���@��\@���@�G�@��/@���@�  @�l�@��+@�J@�x�@��@�(�@�+@��!@��^@�?}@�(�@�\)@�@���@�M�@��^@�G�@���@��m@�dZ@���@�ȴ@���@��\@�-@���@��h@�O�@��/@�b@�l�@�dZ@�33@��@��!@���@�~�@�E�@��@��@��@�r�@�  @��m@��;@�ƨ@���@�dZ@�@���@���@���@�@��@��+@�v�@�^5@�=q@��@�p�@��@��9@�bN@��m@��P@��@�l�@��@���@�ff@�M�@�=q@�=q@�E�@�=q@�5?@�$�@���@��@���@�`B@�7L@�Ĝ@��u@�r�@�I�@�(�@��@���@�S�@�o@���@�^5@�E�@�$�@�J@��@��^@��@�V@�@�@~�y@~�@~��@~�+@~5?@}@}`B@}V@|��@|��@|j@|9X@|(�@{��@{ƨ@{��@{��@{��@{dZ@{"�@zn�@yhs@x��@x1'@wl�@vȴ@vv�@v$�@u��@u�h@up�@u/@t�@t��@t(�@st�@s33@r��@r-@qx�@qG�@q%@pbN@o��@ol�@o\)@oK�@o;d@o
=@n��@n�R@m��@m?}@l�/@l(�@kC�@j��@jM�@j�@j�@jJ@jJ@i��@i��@i�@i��@i��@ix�@iG�@i%@hĜ@hbN@h �@g�;@g�P@g;d@g
=@f�@fV@f@e�-@e�h@e�h@e�h@e`B@d9X@cƨ@c�F@c��@c�@c��@cdZ@cC�@co@b��@b~�@bM�@b=q@b-@b�@a�@a��@a�7@aG�@`��@`��@`Q�@_�;@_\)@_�@^��@^�y@^�@^ȴ@^�R@^��@^�+@^V@^$�@]�T@]��@]p�@]?}@\��@\�@[�m@[ƨ@[�@[dZ@[33@[o@Z�H@Z��@Z=q@Y�^@Y��@Y�@Xr�@XbN@XA�@Xb@W��@Wl�@W;d@W+@W�@V�@V5?@U��@U�h@UO�@U/@U�@T�/@Tz�@TI�@T(�@S�
@S"�@RJ@Q�#@Q��@Qx�@Q%@P�`@P��@PbN@P1'@P �@P �@Nȴ@M@MO�@L�@L1@K�
@Kƨ@K��@K�@KC�@Ko@J��@J~�@J=q@JJ@I��@I��@Ix�@IX@I%@H��@H�`@H�u@G�@G|�@G;d@G�@G
=@F�y@F�+@F@E��@E�-@Ep�@EO�@EO�@EV@D��@D��@D�D@DZ@D�@C�m@C�@Ct�@C"�@B^5@A��@A��@A��@A�7@Ahs@AX@A&�@@��@@��@@�9@@�@@bN@@bN@@bN@?��@?\)@?;d@>�@>�+@>V@=�@=p�@=�@<�j@<z�@<1@;�@;33@;@:�!@:~�@:-@9�#@9hs@9X@9G�@9&�@8��@8�`@8�9@8�u@8Q�@7�@7�@7\)@7+@7+@7
=@6�@6�R@6�R@6��@65?@5�T@5�h@5O�@4�/@4z�@4j@4I�@4(�@3��@3��@3C�@333@3"�@2�@2��@2^5@2-@1�#@1X@17L@1G�@1&�@1%@0�`@0b@/��@/|�@/K�@/+@.��@.$�@-�@-��@-��@-O�@-V@,��@,Z@,(�@+��@+�F@+"�@*=q@*�@*J@)��@)�@)�@)�#@)��@)hs@)7L@)%@(bN@'l�@'�@'�@'�@'�@'
=@&��@&��@&v�@%��@%O�@%O�@%?}@%/@%V@$�@$�D@$(�@#�F@#�@#C�@#"�@#o@#@#@"��@"�!@"M�@"�@"J@!�@!��@!�^@!��@!hs@!&�@ Ĝ@ �u@ �@ �@ �@ �@ r�@ Q�@   @�w@l�@;d@
=@
=@��@�@��@v�@ff@E�@{@�@@�-@�h@`B@O�@O�@/@�@�/@��@��@�j@�D@(�@�@�m@�@"�@�!@=q@=q@�@��@x�@x�@hs@hs@hs@X@X@X@G�@&�@�`@�u@  @�;@�@|�@
=@�R@ff@V@5?@{@��@O�@/@/@V@��@��@�@�@�D@(�@��@�
@�@@��@�\@=q@�@��@�7@hs@7L@��@�u@r�@Q�@  @�;@��@�@��@ȴ@��@v�@ff@5?@@�T@��@@��@?}@/@�/@��@�D@j@I�@��@�
@��@33@
�@
�H@
�!@
~�@
~�@
~�@
n�@
n�@
-@
�@	��@	��@	�@	�@	�#@	�#@	�^@	��@	�7@	x�@	x�@	G�@	&�@	%@��@�u@r�@bN@1'@b@  @�@�w@��@�P@|�@l�@K�@+@+@
=@�y@ȴ@�R@��@��@ff@V@5?@{@�@��@�-@�h@`B@��@�@��@��@�D@z�@Z@(�@�@�m@ƨ@ƨ@�
@ƨ@ƨ@ƨ@��@dZ@C�@C�@33A�S�A�A�l�A��A��mA�ƨAɬAɅA�M�A�?}A�;dA�9XA�33A�-A�&�A�$�A�"�A�VA�A�A���A���A��A��A��A��A��A��#A��
A���A���A�ȴA�A���AȾwAȼjAȸRAȶFAȬAȣ�Aȡ�Aț�Aȟ�Aȟ�Aș�Aȕ�Aȕ�Aȗ�Aȗ�Aȕ�AȓuAȍPAȇ+AȋDAȋDAȇ+AȅAȇ+Aȉ7AȃA�~�A�~�A�~�AȁAȁA�~�A�|�A�z�A�x�A�x�A�x�A�t�A�r�A�z�A�z�A�x�A�v�A�t�A�r�A�p�A�r�A�t�A�v�A�t�A�r�A�n�A�n�A�n�A�p�A�p�A�n�A�l�A�jA�jA�l�A�p�A�p�A�n�A�l�A�l�A�l�A�n�A�p�A�n�A�l�A�jA�jA�n�A�n�A�n�A�jA�jA�jA�l�A�jA�jA�hsA�ffA�ffA�hsA�jA�l�A�jA�jA�ffA�ffA�hsA�jA�jA�jA�ffA�ffA�hsA�jA�ffA�M�A�oA��A��;AǾwAǗ�A�l�A�E�A�1A��TA���AƬAƟ�AƉ7A�t�A�ZA�XA�C�A��A��A��A���Aŉ7AŁA�|�A�`BA�M�A��A��
Aĺ^AĬAĝ�Aė�A�^5A��A�oA���A���A���A��
A���A���A�=qA�ZA�E�A�=qA��-A��A���A�A���A���A�9XA�A�7LA�1A�+A�&�A��A���A���A���A��jA��\A�x�A�VA�M�A�/A�{A�r�A��A���A�K�A�A���A��A���A�O�A��/A���A��A�ƨA���A���A��9A�v�A�hsA�E�A�O�A�%A�l�A��jA��-A���A�A�r�A��#A���A���A�t�A�XA�;dA�33A�&�A��A�VA�1A��A���A���A�ffA�5?A�&�A��A�bA�A��/A��-A�^5A� �A��A��^A���A�l�A�^5A�jA�;dA�~�A�JA�ĜA���A��7A��A�n�A�1'A��TA��-A�/A��;A��A�O�A�(�A��/A�ĜA��uA�S�A���A���A�1'A�ȴA�`BA��A���A��FA��uA�|�A�r�A�XA�E�A�=qA�7LA�-A�"�A�A��wA��DA�bNA��A���A��A��jA���A��+A�l�A�O�A�7LA� �A�bA���A��A���A���A�A���A�v�A�jA�ZA�=qA�(�A�oA�  A��A��/A�ĜA��A���A�z�A�33A� �A�bA��yA��
A���A�ƨA��FA���A���A���A��+A�t�A�E�A��A��-A�`BA�{A��A��
A��wA���A�|�A�bNA�S�A�G�A�=qA�/A��A�A���A��-A���A���A�v�A�K�A�/A���A��-A���A���A���A�bNA��A��-A��!A��A���A��PA��A�z�A�M�A�$�A��A��jA�A���A��A�?}A��A�%A���A��/A�ȴA���A��A�t�A�\)A�9XA��;A��FA���A��PA�~�A�t�A�hsA�XA�C�A�5?A�1'A�/A�$�A�A��#A���A��hA�v�A�7LA��A��wA���A���A���A���A���A���A���A��DA�t�A�^5A�M�A�%A���A���A��yA���A���A���A���A���A���A���A�t�A�ZA�5?A�33A�-A�"�A�1A�mA��A?}A~��A~r�A~�A~�A~JA}��A}�wA}��A}��A}�hA}XA}33A}"�A|�RA|bNA|1A{�FA{��Az��Az1Ay?}Ax�Ax�jAx�!Ax��Ax�+Ax�Axz�Axn�Axz�Axr�AxI�AxbAw�#Awp�Av��Av��Av1'Au`BAu�Au?}Aul�Au�PAu�Au�PAuC�At��At�yAt�/AtĜAt��At�DAtv�At=qAt�As�#As�As�PAsx�As/AsoAsVAsAr��Ar�Ar��AsoAs?}AsS�AsXAs`BAsl�Ast�AsXAr��Ar�Ar�!Ar�DArjArM�Ar1'Ar�Aq�TAq�-Aq��Aq�Aql�Aqx�Aq|�Aql�AqK�Aq/Aq%Ap�9Ap-Ao��Ao�Ao��Ao��Ao�Ao�Ao?}AooAn��An�/An�/An�HAn�/An�An��An��An��An�An�An��An�jAn��An�+Anz�An�DAn��An��An��An��An�jAnȴAn��Ao
=Ao"�Ao+Ao33Ao7LAo+Ao+Ao/Ao"�AooAo
=Ao%An��An�9AnjAnbAm��AmVAlȴAl��Alv�Al5?Al �Ak�AkAk�Ak��Ak��Ak��Ak��Ak��Ak��Ak|�Akx�Akt�Akp�Akp�Akp�Akl�AkhsAkp�Akt�Akt�Akp�Ak\)AkXAkG�AkK�AkO�Ak\)Akl�Ak`BAkG�Ak7LAk/AkoAj��Aj�AkVAkoAj�/Aj��Aj~�Ajn�AjM�Aj$�AjJAi�Ai��Ai�Ah~�AhAg�FAg�AgdZAgO�Ag?}Ag;dAg/Ag�Ag%Af�yAf��Af�jAf��Af�+AfZAf-Af�Af1Ae�Ae�#Ae��Ae��AeK�Ae?}Ae7LAe33Ae&�Ae+Ae+Ae"�Ae&�Ae&�Ae"�Ae"�Ae"�AeoAe
=AeAd��Ad�/AdȴAd��Ad�9Ad�RAd�RAd�Ad��Ad��Ad�Ad�Ad�Adr�AdjAdE�Ad�Ac��Ac��AcAc��Ac�Acl�Ac?}Ac"�Ac
=Ab�Ab�RAb�Ab�!Ab�Ab��Ab~�Ab-AbAa��Aa�Aa��Aa�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         A�S�Aɛ�A�=qA�+A�JA���A��A���A�AȺ^Aȩ�Aȝ�AȑhAȇ+A�~�A�x�A�v�A�t�A�p�A�l�A�n�A�l�A�jA�hsA�+A��A���A��AöFA���A��^A�M�A��A���A�/A��A�VA�?}A�=qA���A�n�A���A�5?A�JA�Q�A��jA��-A�oA���A�{A��hA��mA��PA�^5A��A�JA�n�A���A��!A�9XA�hsA���A�1A�n�A�$�A�x�A��A��A���A���A�Q�A�wA~(�A}�A|VAy��Ax�+Ax�Av1Aul�At�As�As+Ar��Aq��ApQ�Ao
=AnĜAn��Ao"�An=qAlAk�AkhsAkC�Aj�RAh�/Ag�Af9XAeC�Ae�AdĜAdffAcp�Ab�DAa|�A`Q�A_��A^v�A^9XA]�A\r�A[K�AZ�AZ��AZ1'AYAX��AXA�AV�AU�AU7LAT��AS��ARv�AP��AP=qAO;dAN�AM�AL�AK�#AKC�AJVAIS�AH�AH(�AG�TAF��AF�AE��AD(�AC\)AC
=ABffAA�wA@�jA?XA>ȴA>A�A=�7A<��A<�jA;�A:�uA9��A9��A9�^A9&�A7�^A6^5A5A4M�A3\)A2�RA2A1O�A0~�A/�wA/33A.~�A-��A-�A,z�A+�A*��A*=qA)p�A({A&�HA& �A$��A$A#�hA#?}A"z�A!hsA!A�#A7LA�TAA�A��A�HA�wA/A9XA�^AC�A��AE�A�wA%AbNA��A��AI�AhsA�DA��A��A;dAn�A��A�A��AjAJAt�AoA
�9A
E�A	��A	A�`A�AZA��A�A5?A��A�#A�A�DAJA+A �A �A ��A I�A b@�
=@�ff@��^@�A�@�=q@�/@��@�\)@�E�@�O�@���@��m@��@��/@�b@�w@�o@��@���@�bN@� �@�@�p�@�j@���@�C�@�M�@�x�@��/@���@�@�j@�\)@�$�@݁@ܛ�@�S�@���@��@��@���@�|�@�@Ь@�l�@�@���@˅@ʸR@�^5@Ɂ@�7L@��`@ȃ@�1@�K�@���@�~�@őh@��`@�z�@�1'@��
@�C�@�E�@�X@�A�@�dZ@��@���@�(�@�|�@�M�@�%@���@��@�|�@�K�@���@��D@��F@�S�@���@��7@�Ĝ@�  @�;d@�^5@��7@���@�(�@���@�C�@�v�@���@��@�I�@�C�@�E�@���@�O�@��@��/@���@��\@���@�G�@��/@���@�  @�l�@��+@�J@�x�@��@�(�@�+@��!@��^@�?}@�(�@�\)@�@���@�M�@��^@�G�@���@��m@�dZ@���@�ȴ@���@��\@�-@���@��h@�O�@��/@�b@�l�@�dZ@�33@��@��!@���@�~�@�E�@��@��@��@�r�@�  @��m@��;@�ƨ@���@�dZ@�@���@���@���@�@��@��+@�v�@�^5@�=q@��@�p�@��@��9@�bN@��m@��P@��@�l�@��@���@�ff@�M�@�=q@�=q@�E�@�=q@�5?@�$�@���@��@���@�`B@�7L@�Ĝ@��u@�r�@�I�@�(�@��@���@�S�@�o@���@�^5@�E�@�$�@�J@��@��^@��@�V@�@�@~�y@~�@~��@~�+@~5?@}@}`B@}V@|��@|��@|j@|9X@|(�@{��@{ƨ@{��@{��@{��@{dZ@{"�@zn�@yhs@x��@x1'@wl�@vȴ@vv�@v$�@u��@u�h@up�@u/@t�@t��@t(�@st�@s33@r��@r-@qx�@qG�@q%@pbN@o��@ol�@o\)@oK�@o;d@o
=@n��@n�R@m��@m?}@l�/@l(�@kC�@j��@jM�@j�@j�@jJ@jJ@i��@i��@i�@i��@i��@ix�@iG�@i%@hĜ@hbN@h �@g�;@g�P@g;d@g
=@f�@fV@f@e�-@e�h@e�h@e�h@e`B@d9X@cƨ@c�F@c��@c�@c��@cdZ@cC�@co@b��@b~�@bM�@b=q@b-@b�@a�@a��@a�7@aG�@`��@`��@`Q�@_�;@_\)@_�@^��@^�y@^�@^ȴ@^�R@^��@^�+@^V@^$�@]�T@]��@]p�@]?}@\��@\�@[�m@[ƨ@[�@[dZ@[33@[o@Z�H@Z��@Z=q@Y�^@Y��@Y�@Xr�@XbN@XA�@Xb@W��@Wl�@W;d@W+@W�@V�@V5?@U��@U�h@UO�@U/@U�@T�/@Tz�@TI�@T(�@S�
@S"�@RJ@Q�#@Q��@Qx�@Q%@P�`@P��@PbN@P1'@P �@P �@Nȴ@M@MO�@L�@L1@K�
@Kƨ@K��@K�@KC�@Ko@J��@J~�@J=q@JJ@I��@I��@Ix�@IX@I%@H��@H�`@H�u@G�@G|�@G;d@G�@G
=@F�y@F�+@F@E��@E�-@Ep�@EO�@EO�@EV@D��@D��@D�D@DZ@D�@C�m@C�@Ct�@C"�@B^5@A��@A��@A��@A�7@Ahs@AX@A&�@@��@@��@@�9@@�@@bN@@bN@@bN@?��@?\)@?;d@>�@>�+@>V@=�@=p�@=�@<�j@<z�@<1@;�@;33@;@:�!@:~�@:-@9�#@9hs@9X@9G�@9&�@8��@8�`@8�9@8�u@8Q�@7�@7�@7\)@7+@7+@7
=@6�@6�R@6�R@6��@65?@5�T@5�h@5O�@4�/@4z�@4j@4I�@4(�@3��@3��@3C�@333@3"�@2�@2��@2^5@2-@1�#@1X@17L@1G�@1&�@1%@0�`@0b@/��@/|�@/K�@/+@.��@.$�@-�@-��@-��@-O�@-V@,��@,Z@,(�@+��@+�F@+"�@*=q@*�@*J@)��@)�@)�@)�#@)��@)hs@)7L@)%@(bN@'l�@'�@'�@'�@'�@'
=@&��@&��@&v�@%��@%O�@%O�@%?}@%/@%V@$�@$�D@$(�@#�F@#�@#C�@#"�@#o@#@#@"��@"�!@"M�@"�@"J@!�@!��@!�^@!��@!hs@!&�@ Ĝ@ �u@ �@ �@ �@ �@ r�@ Q�@   @�w@l�@;d@
=@
=@��@�@��@v�@ff@E�@{@�@@�-@�h@`B@O�@O�@/@�@�/@��@��@�j@�D@(�@�@�m@�@"�@�!@=q@=q@�@��@x�@x�@hs@hs@hs@X@X@X@G�@&�@�`@�u@  @�;@�@|�@
=@�R@ff@V@5?@{@��@O�@/@/@V@��@��@�@�@�D@(�@��@�
@�@@��@�\@=q@�@��@�7@hs@7L@��@�u@r�@Q�@  @�;@��@�@��@ȴ@��@v�@ff@5?@@�T@��@@��@?}@/@�/@��@�D@j@I�@��@�
@��@33@
�@
�H@
�!@
~�@
~�@
~�@
n�@
n�@
-@
�@	��@	��@	�@	�@	�#@	�#@	�^@	��@	�7@	x�@	x�@	G�@	&�@	%@��@�u@r�@bN@1'@b@  @�@�w@��@�P@|�@l�@K�@+@+@
=@�y@ȴ@�R@��@��@ff@V@5?@{@�@��@�-@�h@`B@��@�@��@��@�D@z�@Z@(�@�@�m@ƨ@ƨ@�
@ƨ@ƨ@ƨ@��@dZ@C�@C�G�O�A�S�A�A�l�A��A��mA�ƨAɬAɅA�M�A�?}A�;dA�9XA�33A�-A�&�A�$�A�"�A�VA�A�A���A���A��A��A��A��A��A��#A��
A���A���A�ȴA�A���AȾwAȼjAȸRAȶFAȬAȣ�Aȡ�Aț�Aȟ�Aȟ�Aș�Aȕ�Aȕ�Aȗ�Aȗ�Aȕ�AȓuAȍPAȇ+AȋDAȋDAȇ+AȅAȇ+Aȉ7AȃA�~�A�~�A�~�AȁAȁA�~�A�|�A�z�A�x�A�x�A�x�A�t�A�r�A�z�A�z�A�x�A�v�A�t�A�r�A�p�A�r�A�t�A�v�A�t�A�r�A�n�A�n�A�n�A�p�A�p�A�n�A�l�A�jA�jA�l�A�p�A�p�A�n�A�l�A�l�A�l�A�n�A�p�A�n�A�l�A�jA�jA�n�A�n�A�n�A�jA�jA�jA�l�A�jA�jA�hsA�ffA�ffA�hsA�jA�l�A�jA�jA�ffA�ffA�hsA�jA�jA�jA�ffA�ffA�hsA�jA�ffA�M�A�oA��A��;AǾwAǗ�A�l�A�E�A�1A��TA���AƬAƟ�AƉ7A�t�A�ZA�XA�C�A��A��A��A���Aŉ7AŁA�|�A�`BA�M�A��A��
Aĺ^AĬAĝ�Aė�A�^5A��A�oA���A���A���A��
A���A���A�=qA�ZA�E�A�=qA��-A��A���A�A���A���A�9XA�A�7LA�1A�+A�&�A��A���A���A���A��jA��\A�x�A�VA�M�A�/A�{A�r�A��A���A�K�A�A���A��A���A�O�A��/A���A��A�ƨA���A���A��9A�v�A�hsA�E�A�O�A�%A�l�A��jA��-A���A�A�r�A��#A���A���A�t�A�XA�;dA�33A�&�A��A�VA�1A��A���A���A�ffA�5?A�&�A��A�bA�A��/A��-A�^5A� �A��A��^A���A�l�A�^5A�jA�;dA�~�A�JA�ĜA���A��7A��A�n�A�1'A��TA��-A�/A��;A��A�O�A�(�A��/A�ĜA��uA�S�A���A���A�1'A�ȴA�`BA��A���A��FA��uA�|�A�r�A�XA�E�A�=qA�7LA�-A�"�A�A��wA��DA�bNA��A���A��A��jA���A��+A�l�A�O�A�7LA� �A�bA���A��A���A���A�A���A�v�A�jA�ZA�=qA�(�A�oA�  A��A��/A�ĜA��A���A�z�A�33A� �A�bA��yA��
A���A�ƨA��FA���A���A���A��+A�t�A�E�A��A��-A�`BA�{A��A��
A��wA���A�|�A�bNA�S�A�G�A�=qA�/A��A�A���A��-A���A���A�v�A�K�A�/A���A��-A���A���A���A�bNA��A��-A��!A��A���A��PA��A�z�A�M�A�$�A��A��jA�A���A��A�?}A��A�%A���A��/A�ȴA���A��A�t�A�\)A�9XA��;A��FA���A��PA�~�A�t�A�hsA�XA�C�A�5?A�1'A�/A�$�A�A��#A���A��hA�v�A�7LA��A��wA���A���A���A���A���A���A���A��DA�t�A�^5A�M�A�%A���A���A��yA���A���A���A���A���A���A���A�t�A�ZA�5?A�33A�-A�"�A�1A�mA��A?}A~��A~r�A~�A~�A~JA}��A}�wA}��A}��A}�hA}XA}33A}"�A|�RA|bNA|1A{�FA{��Az��Az1Ay?}Ax�Ax�jAx�!Ax��Ax�+Ax�Axz�Axn�Axz�Axr�AxI�AxbAw�#Awp�Av��Av��Av1'Au`BAu�Au?}Aul�Au�PAu�Au�PAuC�At��At�yAt�/AtĜAt��At�DAtv�At=qAt�As�#As�As�PAsx�As/AsoAsVAsAr��Ar�Ar��AsoAs?}AsS�AsXAs`BAsl�Ast�AsXAr��Ar�Ar�!Ar�DArjArM�Ar1'Ar�Aq�TAq�-Aq��Aq�Aql�Aqx�Aq|�Aql�AqK�Aq/Aq%Ap�9Ap-Ao��Ao�Ao��Ao��Ao�Ao�Ao?}AooAn��An�/An�/An�HAn�/An�An��An��An��An�An�An��An�jAn��An�+Anz�An�DAn��An��An��An��An�jAnȴAn��Ao
=Ao"�Ao+Ao33Ao7LAo+Ao+Ao/Ao"�AooAo
=Ao%An��An�9AnjAnbAm��AmVAlȴAl��Alv�Al5?Al �Ak�AkAk�Ak��Ak��Ak��Ak��Ak��Ak��Ak|�Akx�Akt�Akp�Akp�Akp�Akl�AkhsAkp�Akt�Akt�Akp�Ak\)AkXAkG�AkK�AkO�Ak\)Akl�Ak`BAkG�Ak7LAk/AkoAj��Aj�AkVAkoAj�/Aj��Aj~�Ajn�AjM�Aj$�AjJAi�Ai��Ai�Ah~�AhAg�FAg�AgdZAgO�Ag?}Ag;dAg/Ag�Ag%Af�yAf��Af�jAf��Af�+AfZAf-Af�Af1Ae�Ae�#Ae��Ae��AeK�Ae?}Ae7LAe33Ae&�Ae+Ae+Ae"�Ae&�Ae&�Ae"�Ae"�Ae"�AeoAe
=AeAd��Ad�/AdȴAd��Ad�9Ad�RAd�RAd�Ad��Ad��Ad�Ad�Ad�Adr�AdjAdE�Ad�Ac��Ac��AcAc��Ac�Acl�Ac?}Ac"�Ac
=Ab�Ab�RAb�Ab�!Ab�Ab��Ab~�Ab-AbAa��Aa�Aa��Aa�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�YBpoBh>Bh�Bj�BjBk�Bk�Bk�BlWBm)Bl�Bl�Bl"Bk�Bk�BlWBl�Bl�Bl�Bm)Bm)Bm�Bm�Bk�B\�BN�B>�B(�BSB�B��BxBg8BQNBH�BAUB>�BF?BM�B?HB7LB+B�B�BVB!bB�BB=BB:B�BB
��B
�oB
�KB
�B
�TB
�#B
�^B
��B
��B
��B
��B
�B
��B
�@B
�bB
��B
�B
z�B
f2B
`�B
^�B
PB
9XB
4nB
 �B
!B
B

�B
{B
YB	�2B	�/B	�B	�jB	�vB	�"B	�yB	ѷB	��B	�6B	��B	��B	��B	� B	�jB	�B	��B	�B	��B	��B	��B	��B	��B	��B	�LB	��B	�\B	�CB	�SB	��B	�B	�.B	��B	��B	��B	��B	z�B	w2B	uZB	q�B	j�B	e`B	aB	\)B	Y�B	U�B	P}B	K^B	IRB	E9B	A�B	=�B	<�B	<6B	:^B	7�B	49B	.�B	)_B	'B	%B	!�B	�B	YB	�B	�B		B	�B	B��B�B��B�B�B��B�B��B��B��BӏB�HB�B�RBŢB��B��B�B�RB�B��B��B�=B�XB�@B��B��B��B��B�bB�PB�B�rB�B��B~]BzDBx8Bs�Bn�Bn/BjBgBe�Bc Ba�B`�B`B^jB[�BY�BY�BU�BT�BS�BQ�BM�BMBL�BJ�BI�BF�BF?BD�BD3BCaBA�B@�B@B?HB=B<B;dB:�B9XB:^B6�B5B49B4�B1[B0�B/OB/�B,�B)�B)�B(�B($B&�B%�B%�B%FB"4B"4B!�B \B 'B�BOBIB�BBCB=BxB=B�B1B�B�B�B�BSB�B�BuBBB�BBB�B@BoBbBhBhB B�B�B�B BbB4B�B�B�B�B:BB@B�B�B�BuB@BFBFB�B�B�BMB�B�B$B�B1B7B�B�B�B!�B!bB �B �B%zB&LB'�B'RB(XB*0B+kB,�B.B/�B1'B33B49B5?B6zB8�B9�B;0B;dB>�BB'BC�BD�BE�BFtBK^BOvBR BS&BT�BU2BW�BY�B[�B]/B_;B`�Bd&Bg8BhsBm�Bo�Bt�By�B{�B.B�oB�MB�YB�1B�B�"B��B��B�B��B��B��B�B�qB��B��B��B�tB�FB��B�*B��B��B�qB�IB�'B��B��B��B��B�B��B��B��B��B�wB�wB�B��B�3B�EBǮB�BȀBʌB�B�BѷB�&B֡B�KB�QB�#B��B�vB�B�TB�ZB��B��B��B�`B�2B�8B�
B�B��B�B�B�AB�B�B�B�B��B�B�JB��B	�B	uB	�B	�B	�B	�B		7B	~B	�B	�B	�B	=B	CB	�B	B	 \B	"4B	#�B	$�B	%�B	&�B	'�B	(XB	)*B	)�B	*�B	*�B	+6B	,B	-wB	1'B	5�B	6�B	;�B	?B	A�B	B�B	D�B	GB	G�B	G�B	H�B	J#B	K^B	MjB	P}B	QB	S&B	VB	X�B	YB	ZQB	^5B	aB	a|B	a�B	a�B	bNB	b�B	c B	c�B	h
B	jKB	lWB	o�B	s�B	t�B	wfB	xB	xB	x8B	xlB	xlB	xlB	x�B	y>B	zDB	z�B	{B	|�B	~(B	�4B	��B	�B	��B	�YB	�+B	�1B	��B	�~B	�"B	��B	�VB	�VB	�(B	��B	�FB	��B	�SB	��B	��B	�$B	��B	�+B	��B	�qB	�CB	�xB	�xB	��B	�~B	��B	��B	��B	�\B	��B	��B	��B	�zB	�B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	��B	�[B	�aB	�3B	�B	�LB	��B	��B	��B	��B	��B	�dB	�6B	�<B	�HB	��B	�'B	�gB	ƨB	�B	�EB	��B	ɺB	�XB	��B	�^B	�^B	�dB	�<B	ϫB	�}B	�NB	ѷB	ѷB	ҽB	�,B	�aB	��B	��B	�B	��B	�)B	�]B	�dB	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	�B	��B	�]B	�B	��B	�/B	��B	�B	�oB	�B	�vB	�|B	��B	�MB	�B	�B	�%B	�%B	��B	�+B	��B	��B	��B	��B	�B	�DB	�B	��B	��B	�VB	�(B	�]B	�]B	�.B	��B
 4B
 iB
 �B
oB
�B
�B
�B
�B
�B
YB
�B
�B
+B
�B
�B
�B
fB
�B
	B
	lB
	�B
	lB
	lB
�B
�B
B
B
PB
�B
�B
�B
bB
 B
hB
�B
�B
FB
{B
B
MB
SB
$B
�B
�B
+B
+B
�B
�B
�B
eB
�B
�B
	B
�B
B
B
CB
�B
�B
�B
B
�B
�B
�B
VB
 �B
 �B
 �B
!bB
!bB
!�B
"�B
#B
#:B
#:B
#�B
$B
%B
%FB
%�B
&�B
&�B
&�B
&�B
&�B
'RB
)*B
)�B
)�B
*0B
*eB
+kB
,=B
,=B
,=B
-B
-CB
-�B
.IB
.�B
/B
/�B
/�B
1'B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3hB
3�B
4B
4nB
5tB
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
:^B
:^B
:*B
:^B
:�B
:�B
:�B
;�B
<6B
<�B
=<B
=�B
=�B
>B
>B
>B
>wB
>�B
?HB
?�B
?�B
@B
@B
@OB
@�B
@�B
AUB
A�B
B[B
B�B
B�B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
D3B
D�B
DgB
D�B
D�B
EB
EmB
E�B
E�B
E�B
F?B
FtB
F�B
F�B
GEB
GEB
GB
GzB
G�B
G�B
G�B
G�B
G�B
HKB
H�B
H�B
IB
I�B
J#B
J�B
K^B
K)B
K�B
LdB
L�B
L�B
L�B
MB
MB
L�B
MB
MB
MB
M6B
MjB
N<B
N�B
OB
OB
OBB
PB
P�B
QB
P�B
QB
QNB
R B
R�B
R�B
R�B
R�B
R�B
S&B
S[B
S[B
S�B
S�B
TaB
TaB
T�B
VB
U�B
VmB
V�B
V�B
W?B
W�B
W�B
XEB
YKB
YB
YB
YB
Y�B
Y�B
ZQB
Z�B
Z�B
[#B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
\�B
]/B
]�B
]�B
^jB
^�B
^�B
_B
_;B
_�B
`B
`vB
aB
a|B
aHB
a�B
bB
a�B
bB
bB
bB
b�B
b�B
c B
cTB
c�B
cTB
c�B
c�B
c�B
dZB
d�B
d�B
dZB
d�B
e,B
e`B
e�B
e�B
f2B
ffB
f�B
gB
gB
g8B
gmB
gmB
g�B
g�B
g�B
h>B
h>B
hsB
h�B
hsB
h�B
h�B
h�B
iB
iyB
iDB
i�B
i�B
i�B
jB
jB
jB
jB
kQB
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m)B
m�B
m]B
m�B��B��B|By>Bv�Bq�BncBq�BoiBiDBiBg�BhsBh�Bi�BhsBg�Bl�Bk�Bl�Bi�Bi�BkBk�Bk�BiDBl�Bm�Bl�BkBkQBl"Bk�Bk�Bl"Bm�Bl�BlWBm)Bm�Bm]Bn/Bk�Bk�Bm]Bm�Bm�BlWBk�Bl"Bl�BncBm)Bk�BkBk�Bm�Bl"BkBkBl�BlWBl�BkQBkQBkQBl�Bm)Bm)BjKBjKBk�Bk�Bm�Bm]Bk�Bk�Bl�Bm)Bm)Bl�BlWBkQBk�Bm)Bm�Bm)Bl"BkQBk�Bl"Bl�Bm�Bl�Bl�Bk�BlWBl�Bm�Bm�Bm]Bl�Bk�Bm)Bm�Bm�Bm�Bl�Bl"Bl�Bm�Bm�Bm�Bm�Bm)Bl�Bn�Bo BoiBn�Bm]Bm]Bm]Bm�Bn�Bn�Bm�Bm)Bl�Bl�Bm�Bm�Bl�BkQBk�BqABr�BjBg�Bg8BkBb�Bd�BffBYKBY�BXyBUgBS�BS[BW�BT,BW?BK�BOBI�BM�BJ�BF�BC�BK�BIBB[BK)B5�B9$B6FB7�B>wB0�B.IB1�B'RB&LB#�B&�B 'BIRBOBYB~B*�BoB�B��B��B�B B��B��B�B�B��B� B��B�fB�B��B��B�lB�iB�4B�4B�;B�JB�BpoB�+Bg�Bx8B{JBsMBe�B~�B�oB�gB�\BpoB^�B=BHKB=B=Br�BK)BJXBU�BVB_B?}BB�BW�BS[BB�BGzBE�BEmBA�BB'BA B?}B?HBD3BCaBA�BEBCaB=�B:^B8B:^B;�BA�BH�BQ�BI�BJ�BA�B@�B;�BAUBR�BrGBR�BC�B@�B9�B9XB=�BK�BCaBB'BEB9�B5�B1�B)�B)_B \B+B-wB0�B2�B*eB!bB($BBYB�BCB	B�BOBVBB \B \B�B#�B&�B7B!B~B�B$�B&�B"�B 'B �B�B�B�B�B=BkB�BMB�B~B=B�BxBVBeBIBSB�BqBqB�B�B�B=B�B�B�BuB\BVB(BPB�B�B�BBoBBAB�B
�.B
�>B
�fB
�B
�`B
�%B
�|B
�AB
��B
�cB
�B
��B
�B
�B
�B
�]B
�B
�B
�B
�NB
��B
��B
�)B
��B
�B
�NB
�B
��B
��B
�6B
�B
͟B
ɆB
��B
��B
��B
�dB
�?B
�<B
�0B
�9B
�zB
�$B
��B
�*B
�UB
�RB
�B
�$B
��B
��B
��B
�-B
��B
�B
�{B
��B
�B
�B
��B
�OB
��B
�7B
��B
��B
��B
�B
��B
�:B
��B
�B
��B
�B
��B
��B
��B
�B
�\B
�oB
��B
��B
��B
�B
� B
��B
��B
��B
�PB
��B
�lB
�+B
��B
�B
�SB
��B
��B
�1B
��B
~(B
~]B
� B
�oB
z�B
y�B
t�B
v`B
t�B
e�B
`�B
`�B
aB
e,B
Z�B
[�B
c�B
f�B
`�B
bNB
aHB
bNB
\]B
U�B
ZQB
_�B
\�B
R B
F�B
@B
=qB
:�B
:^B
6B
7�B
5tB
6zB
49B
6�B
3�B
1�B
4nB
%B
#�B
/OB
~B
�B
�B
�B
1B
%B
#�B
($B
"�B
OB
�B
7B
�B
�B
�B
�B
4B
�B
JB
B
DB
�B
%B
uB
�B
AB
;B	�.B
 �B

=B	�JB
�B
�B
+B
�B
�B

�B
�B
MB
YB
{B
 iB
B	�(B
oB	�fB	�B	��B	�B	�B	�B	�TB	��B	�B	�B	�B	��B	�2B	�8B	��B	��B	�BB	�|B	�,B	�B	�jB	�B	��B	�/B	�]B	�dB	ޞB	��B	ݘB	ܒB	ޞB	��B	��B	�jB	��B	�dB	�)B	ߤB	�B	�vB	�|B	ޞB	�B	�B	�TB	��B	�]B	��B	��B	�cB	��B	�"B	�/B	�B	�WB	��B	�B	�]B	�B	�DB	��B	�8B	یB	՛B	�EB	�
B	�B	�HB	�vB	�6B	�6B	�)B	̘B	�0B	�^B	��B	˒B	ʌB	ɺB	��B	�#B	��B	��B	�^B	��B	��B	�6B	ϫB	ΥB	��B	�B	�jB	�<B	�vB	֡B	֡B	ԕB	֡B	��B	�9B	�B	�NB	͟B	��B	�2B	�B	�BB	�B	��B	�5B	�dB	�dB	�|B	�B	��B	�/B	��B	�mB	�,B	�,B	��B	ѷB	�NB	��B	��B	�TB	�HB	�pB	�B	�B	�vB	�B	��B	̘B	�^B	��B	˒B	��B	�B	��B	�EB	��B	�KB	ǮB	�tB	�B	�zB	�B	�tB	ƨB	�?B	��B	�B	�B	�tB	�tB	ŢB	ÖB	�9B	�3B	ÖB	�B	��B	�mB	�'B	��B	ĜB	�'B	��B	��B	�UB	�aB	��B	�B	�OB	�HB	�<B	��B	��B	��B	�jB	�^B	�$B	��B	�LB	��B	��B	�qB	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         B�DBt;Bi�Bi�Bk�Bk*Bl�Bl;Bl	BmBm�Bl�Bm)BlSBlBk�BlbBl�Bl�Bl�Bm6Bm:Bn0Bo�BrJBb�BT�BG�BC{BC�B�rB��B��B��BntBO�BF�BEBN;BWuBI�BA�B7�B$�B!�B%zB%PB�BB�BB�BvB�B
��B
�vB
�B
�B
��B
�%B
�lB
��B
��B
�B
��B
��B
��B
�~B
��B
�/B
�B
�B
h�B
d�B
f�B
S�B
;�B
:�B
"�B
!�B
�B
B
B
	�B	��B	�%B	��B	��B	�`B	�bB	�B	�]B	˃B	��B	�4B	�8B	�AB	�9B	�WB	��B	�	B	�pB	�#B	�B	�B	��B	�oB	��B	�nB	�TB	��B	��B	��B	��B	��B	��B	��B	�FB	�,B	�%B	{�B	xvB	y�B	u�B	o�B	hB	dfB	]�B	\�B	Z�B	R�B	M�B	LtB	H�B	D�B	?B	=�B	?-B	<)B	:�B	9B	1rB	*�B	)_B	'�B	%?B	":B	kB	�B	B	
�B	!B	�B	hB�B��B�B�bB��B��B�aB�}B�B��B��BϔB�FB�KB�B�PB�B�1B�UB�B�B��B�fB�B�B��B��B��B�B��B��B�B��B��B�B B}�Bu�Bq�BrBl�BjkBgyBd�Bb�BcXBb$BaB^OB\�B\�BW�BX@BWBTBN�BN�BOsBM%BLNBHBG�BF*BFNBD�BClBB6BB�BAbB=�B<�B<�B<|B<sB=B7�B5�B7gB6~B3iB4"B38B3�B.B*�B*�B*yB)iB(6B(�B)VB'UB#�B$TB#�B"$B!�B =B pB �B!7B�BsBB�BB�BPB�B�B�BB
BjBBaB�B�B�BFBHB�B}B�BQB�B�B�BGBGBBtBfB�B7BwB&BcB�B�BKBB�B�B6B�B#B�BB�B�B=B�B�B�B+B�B�B�B TB"AB!�B!�B!�B$&B'�B'�B(qB(�B*sB+�B- B.?B/�B1XB2�B4�B5QB6&B8	B:�B;:B<�B=lB@�BCjBDlBE'BFTBH�BMmBQ6BR�BS�BU.BVzBX�B[eB]B^eB``BbrBfBhNBjUBn�Bq�BvBz�B|�B�B��B�CB��B��B�B��B�B�"B�YB��B�vB��B��B�uB�yB�/B�B��B��B�KB�_B�B�"B�*B�>B�HB��B��B��B�B�`B��B�{B��B��B��B��B�B�#B��B�sB��B�qB�)BˌB��B��B�qB�B�KB�pBښB��BްB��B��B�B�gB��B��B�B�B�B�sB�B�B�*B�wB�B�B�B�jB�2B��B�2B��B��B��B	�B	�B	%B	B	_B	OB	
MB	�B	@B	�B	�B	yB	vB	DB	�B	 �B	"�B	#�B	%B	&B	&�B	'�B	(�B	)fB	*'B	*�B	*�B	+vB	,aB	.5B	21B	6(B	7�B	<WB	?�B	BB	B�B	ESB	G/B	G�B	H/B	I5B	J�B	K�B	NB	P�B	Q�B	S�B	V�B	X�B	YlB	Z�B	^�B	aHB	a�B	a�B	bB	b�B	cB	cxB	d�B	h�B	j�B	mB	p�B	s�B	uuB	w�B	xB	xB	xDB	x�B	xyB	x�B	y B	yxB	zoB	z�B	{�B	}B	~�B	�~B	��B	�kB	�B	��B	�mB	��B	�3B	��B	�EB	��B	�fB	��B	�EB	�B	�aB	�B	�fB	��B	��B	�PB	��B	�yB	�$B	��B	�]B	��B	��B	�B	��B	�3B	�6B	�EB	��B	��B	�B	�VB	��B	�@B	�B	�lB	�B	�B	��B	��B	�B	�B	��B	�2B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	³B	�B	��B	�:B	ǄB	�[B	��B	ʎB	�B	�zB	ˮB	�B	΢B	��B	��B	�uB	��B	�B	�!B	�bB	ԑB	�.B	֎B	�B	�$B	�HB	ܵB	��B	��B	�PB	� B	�B	�,B	�8B	�#B	�B	�%B	�B	�B	�'B	�wB	��B	��B	�uB	�B	��B	�B	�RB	�B	�B	�B	�uB	��B	�sB	�:B	�CB	�TB	��B	�AB	�B	��B	��B	�=B	��B	��B	��B	�B	��B	�LB	�iB	��B	�sB
  B
 NB
 �B
B
�B
6B
�B
B
jB
B
�B
�B
B
QB
�B
�B
2B
�B
�B
	9B
	�B
	�B
	�B

'B
�B
�B
zB
oB
�B
"B
B
 B
�B
KB
�B
(B
�B
|B
�B
SB
�B
�B
�B
�B
�B
RB
aB
�B
�B
&B
�B
3B
�B
^B
B
B
7B
yB
�B
�B
�B
zB
B
�B
:B
�B
 �B
!B
! B
!�B
!�B
!�B
"�B
#B
#RB
#rB
$B
$qB
%KB
%�B
&_B
&�B
&�B
&�B
'B
'$B
()B
)�B
)�B
)�B
*`B
*�B
+�B
,rB
,eB
,{B
-aB
-�B
.(B
.�B
/B
/TB
/�B
0]B
2 B
2�B
2�B
2�B
3B
3B
3B
36B
3�B
4B
4GB
5B
6_B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8B
9�B
:�B
:fB
:AB
:vB
:�B
:�B
;0B
<B
<�B
=	B
=�B
=�B
=�B
>"B
>B
>EB
>�B
?
B
?|B
?�B
@B
@=B
@2B
@xB
@�B
A2B
A�B
B&B
BoB
B�B
B�B
B�B
B�B
B�B
CLB
CvB
C�B
D5B
DeB
D�B
DB
D�B
EB
E8B
E�B
E�B
E�B
E�B
FrB
F�B
F�B
GB
GXB
GMB
G:B
G�B
G�B
G�B
G�B
G�B
HB
H�B
IB
I#B
I�B
J$B
J�B
K`B
KlB
K�B
LJB
L�B
L�B
L�B
L�B
MB
MB
L�B
MB
MB
M)B
M}B
M�B
N�B
N�B
OFB
OKB
O�B
PlB
QB
Q0B
QB
QGB
Q�B
RtB
R�B
R�B
R�B
S	B
S(B
SIB
ScB
S�B
T!B
T/B
T�B
T�B
U�B
VFB
VB
V�B
W B
V�B
W�B
W�B
XB
X�B
Y�B
Y�B
YCB
YiB
ZB
Z B
Z�B
[B
["B
[KB
[�B
[�B
\+B
\_B
\�B
\�B
]B
\�B
]�B
]�B
^!B
^�B
^�B
^�B
_/B
_�B
_�B
`RB
`�B
aUB
a�B
a}B
bB
bB
a�B
b,B
b$B
b\B
b�B
b�B
c&B
cgB
c�B
ciB
c�B
dB
dB
doB
d�B
d�B
d�B
eB
eSB
e�B
e�B
f"B
fIB
f�B
f�B
gB
gB
gkB
g�B
g�B
g�B
g�B
g�B
h_B
hGB
h�B
h�B
h�B
h�B
h�B
h�B
iAB
i�B
iiB
i�B
i�B
i�B
j:B
j?B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
lB
l�B
l�B
l�B
mB
l�B
l�B
l�B
l�B
l�B
l�B
m�B
mOB
m�B
mxG�O�B��B��B|By>Bv�Bq�BncBq�BoiBiDBiBg�BhsBh�Bi�BhsBg�Bl�Bk�Bl�Bi�Bi�BkBk�Bk�BiDBl�Bm�Bl�BkBkQBl"Bk�Bk�Bl"Bm�Bl�BlWBm)Bm�Bm]Bn/Bk�Bk�Bm]Bm�Bm�BlWBk�Bl"Bl�BncBm)Bk�BkBk�Bm�Bl"BkBkBl�BlWBl�BkQBkQBkQBl�Bm)Bm)BjKBjKBk�Bk�Bm�Bm]Bk�Bk�Bl�Bm)Bm)Bl�BlWBkQBk�Bm)Bm�Bm)Bl"BkQBk�Bl"Bl�Bm�Bl�Bl�Bk�BlWBl�Bm�Bm�Bm]Bl�Bk�Bm)Bm�Bm�Bm�Bl�Bl"Bl�Bm�Bm�Bm�Bm�Bm)Bl�Bn�Bo BoiBn�Bm]Bm]Bm]Bm�Bn�Bn�Bm�Bm)Bl�Bl�Bm�Bm�Bl�BkQBk�BqABr�BjBg�Bg8BkBb�Bd�BffBYKBY�BXyBUgBS�BS[BW�BT,BW?BK�BOBI�BM�BJ�BF�BC�BK�BIBB[BK)B5�B9$B6FB7�B>wB0�B.IB1�B'RB&LB#�B&�B 'BIRBOBYB~B*�BoB�B��B��B�B B��B��B�B�B��B� B��B�fB�B��B��B�lB�iB�4B�4B�;B�JB�BpoB�+Bg�Bx8B{JBsMBe�B~�B�oB�gB�\BpoB^�B=BHKB=B=Br�BK)BJXBU�BVB_B?}BB�BW�BS[BB�BGzBE�BEmBA�BB'BA B?}B?HBD3BCaBA�BEBCaB=�B:^B8B:^B;�BA�BH�BQ�BI�BJ�BA�B@�B;�BAUBR�BrGBR�BC�B@�B9�B9XB=�BK�BCaBB'BEB9�B5�B1�B)�B)_B \B+B-wB0�B2�B*eB!bB($BBYB�BCB	B�BOBVBB \B \B�B#�B&�B7B!B~B�B$�B&�B"�B 'B �B�B�B�B�B=BkB�BMB�B~B=B�BxBVBeBIBSB�BqBqB�B�B�B=B�B�B�BuB\BVB(BPB�B�B�BBoBBAB�B
�.B
�>B
�fB
�B
�`B
�%B
�|B
�AB
��B
�cB
�B
��B
�B
�B
�B
�]B
�B
�B
�B
�NB
��B
��B
�)B
��B
�B
�NB
�B
��B
��B
�6B
�B
͟B
ɆB
��B
��B
��B
�dB
�?B
�<B
�0B
�9B
�zB
�$B
��B
�*B
�UB
�RB
�B
�$B
��B
��B
��B
�-B
��B
�B
�{B
��B
�B
�B
��B
�OB
��B
�7B
��B
��B
��B
�B
��B
�:B
��B
�B
��B
�B
��B
��B
��B
�B
�\B
�oB
��B
��B
��B
�B
� B
��B
��B
��B
�PB
��B
�lB
�+B
��B
�B
�SB
��B
��B
�1B
��B
~(B
~]B
� B
�oB
z�B
y�B
t�B
v`B
t�B
e�B
`�B
`�B
aB
e,B
Z�B
[�B
c�B
f�B
`�B
bNB
aHB
bNB
\]B
U�B
ZQB
_�B
\�B
R B
F�B
@B
=qB
:�B
:^B
6B
7�B
5tB
6zB
49B
6�B
3�B
1�B
4nB
%B
#�B
/OB
~B
�B
�B
�B
1B
%B
#�B
($B
"�B
OB
�B
7B
�B
�B
�B
�B
4B
�B
JB
B
DB
�B
%B
uB
�B
AB
;B	�.B
 �B

=B	�JB
�B
�B
+B
�B
�B

�B
�B
MB
YB
{B
 iB
B	�(B
oB	�fB	�B	��B	�B	�B	�B	�TB	��B	�B	�B	�B	��B	�2B	�8B	��B	��B	�BB	�|B	�,B	�B	�jB	�B	��B	�/B	�]B	�dB	ޞB	��B	ݘB	ܒB	ޞB	��B	��B	�jB	��B	�dB	�)B	ߤB	�B	�vB	�|B	ޞB	�B	�B	�TB	��B	�]B	��B	��B	�cB	��B	�"B	�/B	�B	�WB	��B	�B	�]B	�B	�DB	��B	�8B	یB	՛B	�EB	�
B	�B	�HB	�vB	�6B	�6B	�)B	̘B	�0B	�^B	��B	˒B	ʌB	ɺB	��B	�#B	��B	��B	�^B	��B	��B	�6B	ϫB	ΥB	��B	�B	�jB	�<B	�vB	֡B	֡B	ԕB	֡B	��B	�9B	�B	�NB	͟B	��B	�2B	�B	�BB	�B	��B	�5B	�dB	�dB	�|B	�B	��B	�/B	��B	�mB	�,B	�,B	��B	ѷB	�NB	��B	��B	�TB	�HB	�pB	�B	�B	�vB	�B	��B	̘B	�^B	��B	˒B	��B	�B	��B	�EB	��B	�KB	ǮB	�tB	�B	�zB	�B	�tB	ƨB	�?B	��B	�B	�B	�tB	�tB	ŢB	ÖB	�9B	�3B	ÖB	�B	��B	�mB	�'B	��B	ĜB	�'B	��B	��B	�UB	�aB	��B	�B	�OB	�HB	�<B	��B	��B	��B	�jB	�^B	�$B	��B	�LB	��B	��B	�qB	��B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<���=zk�<yW�<���=7��=��<�<<#�
<#�
<#�
<#�
<#�
<)V�<+�i<J�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202006200039532020062000395320200620003953202006200039532020062000395320200620003953SI  SI  ARFMARFM                                                                                                                                                2019110101092420191101010924IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019111101003620191111010036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019111101003620191111010036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020061910341820200619103418IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020062000400220200620004002IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V01                       ARGO_for_DMQC Climatology Version 2020V01                       2020062000400220200620004002IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020062000400220200620004002IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                