CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-09T20:40:33Z creation; 2021-04-29T20:27:09Z DMQC;      
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
_FillValue        G�O�     h  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \p   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     h  dL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     h  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     h  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h @,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     h gp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �P   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201209204033  20210429202817  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_154                 6810_008521_154                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�M��M@�M��M11  @�M�J#9�@�M�J#9�@2���#�@2���#��e쿱[W�e쿱[W11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	?�  ?�@@  @�  @��R@�G�@�G�@��RA�RA   A+�A@  A`  A�  A�Q�A�  A��A�  A�Q�A�Q�A�B   B(�B�
B�
B�
B((�B0(�B8(�B@  BG�
BP  BX  B`(�Bhz�BpQ�Bx(�B�  B�  B��B��
B��B��B�  B�  B�{B�{B�  B��B��
B��B�{B�{B��B�  B��B��
B��
B�  B�(�B�{B��B��B��B�  B�{B��B��B�  C   C��C  C  C��C

=C  C��C��C  C��C  C  C  C��C  C 
=C!��C#��C&{C(
=C*  C+��C.  C0{C2{C4
=C6  C7�C:  C<
=C>  C@
=CB
=CD
=CE��CG��CJ  CL
=CN
=CP{CR
=CT  CU�CX
=CZ  C\  C^  C_��Cb  Cd  Cf  Cg��Cj
=Cl  Cn  Co��Cr  Ct
=Cv
=Cx  Cz  C|
=C~
=C�  C���C�  C�  C�C�  C���C���C�  C�  C�
=C�
=C�C���C���C�  C�C�  C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C���C���C�C���C�  C�C�  C���C�  C���C���C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�C���C���C�  C���C���C���C���C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C���C���C���C���C�C�  C���C�  C�  C�C�\C�C���C���C���C�  C�
=C�
=C�
=C�
=C�  C��C���C���C���C���C���C���C�C�
=C�  C�
=C�C�C�C�  C���C�  C�C�  C�  C�C�C�
=C�C�  C�C�C�C�C�  C�C�  C�C�
=D   D ��D�D}qD�qDz�D�qD��D  D� D�qD� D  D� D  D� D  D� D�qD	� D
  D
� D�D� D  D}qD  D��D�qDz�D�qD� D  D}qD  D� D�qD� D�D� D  D}qD�qD� D�D}qD�qD� D�D��D�qD� D  D� D�qD}qD  Dz�D�qD� D�D�DD��D �D � D!�D!� D!��D"}qD#�D#}qD#�qD$}qD$��D%z�D%�qD&� D'  D'� D(  D(� D)�D)��D*  D*��D+D+��D,  D,��D-D-��D.  D.��D/�D/� D0  D0��D1  D1}qD1��D2}qD3  D3�D4  D4z�D5�D5��D6  D6� D6�qD7}qD7�qD8}qD9  D9}qD9�RD:}qD;  D;� D<  D<� D<�qD=��D>D>��D?  D?��D@  D@� D@�qDA}qDB  DB��DB�qDC}qDD  DD� DE  DE}qDE�qDF� DG  DG� DH  DH� DI  DI��DJ�DJ��DK�DK��DL�DL�DM  DM� DN�DN��DO  DO}qDP  DPz�DQ  DQ� DQ�qDR� DS  DS� DT  DT}qDU  DU��DV  DV� DW  DW��DX  DX� DYDY� DY��DZ}qD[  D[� D\  D\� D\�qD]}qD]�qD^��D_�D_�D`D`��Da�Da��Db  Db� Dc�Dc��Dd  Dd��DeDe� De��Dfz�Dg  Dg� Dg�qDh}qDi  Di��Di�qDj}qDk  Dk� Dk�qDl� Dm�Dm}qDm�qDn��Do�Do� Dp  Dp}qDq�Dq��Dr  Dr}qDr�qDs}qDs�qDt}qDt��Du}qDu�qDv}qDv�qDw� Dx  Dx� Dy  Dy� Dz  Dz}qDz�qD{� D{�qD|}qD}�D}� D~  D~�D�D� D�qD�>�D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D���D���D�>�D�� D���D��qD�>�D��HD�D�  D�@ D��HD��HD�HD�@ D��HD��HD�HD�B�D��HD�D�HD�@ D�� D���D�  D�AHD�� D�� D���D�@ D��HD�� D�HD�@ D�}qD��qD���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�@ D��HD��HD�  D�B�D��HD��HD�HD�>�D�~�D�� D�  D�>�D�}qD�� D�  D�=qD�~�D���D�  D�@ D���D�� D��qD�@ D�� D�� D�  D�@ D���D�D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D��D�B�D��HD��HD���D�>�D�� D���D���D�@ D�� D�� D�HD�@ D�}qD���D�HD�AHD�� D��HD���D�>�D�~�D���D�  D�AHD�~�D��qD��qD�=qD�~�D���D�  D�AHD���D�D�HD�=qD�|)D�� D�HD�>�D�}qD���D�  D�AHD��HD�� D��qD�>�D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D���D�HD�AHD�� D��HD��D�@ D�~�D���D�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�@ D��HD��HD���D�@ D�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD��D�B�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D��qD�>�D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D���D���D�>�D�~�D��qD�  D�B�D��HD�� D�HD�AHD��HD�� D�  D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D��HD�HD�AHDHD��HD�HD�@ DÀ D��HD��D�AHDĀ Dľ�D���D�@ DŁHD��HD�HD�AHDƁHD�� D�  D�@ Dǀ D��HD�  D�>�DȁHD�D�HD�AHDɁHD�� D�  D�AHDʀ D�� D�  D�@ DˁHD�� D�  D�>�D́HD�� D���D�@ D̀ D�� D�HD�AHD΀ D�� D���D�>�Dπ D��HD�  D�>�D�~�Dо�D���D�>�D�~�D�� D�  D�>�DҀ D��HD�  D�AHDӀ DӾ�D���D�@ D�~�DԽqD�  D�B�DՀ D�� D�  D�>�D�~�D�� D�HD�@ D׀ D�� D���D�>�D�}qDؾ�D�HD�AHDـ D��HD�  D�>�DځHD�D�HD�@ Dۀ D۾�D���D�>�D�~�D�� D�HD�@ D�~�D�� D���D�>�Dނ�D��HD�HD�@ D�~�D߾�D�  D�@ D�~�DྸD���D�@ D� D�� D���D�>�D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�>�D� D�� D�HD�>�D�~�D澸D���D�@ D�HD�� D�HD�AHD�~�D辸D�  D�B�D� D龸D�  D�@ D�HD��HD�  D�@ D�HD뾸D���D�@ D�HD��HD���D�>�D� D���D���D�>�D� DD���D�>�D� D�� D���D�AHD��HD��HD�  D�AHD�HD�D���D�>�D� D�D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�AHD�� D��HD�  D�@ D��HD�� D�  D�@ D�~�D��HD�HD�@ D�}qD���?.{?8Q�?W
=?�=q?�33?Ǯ?�(�@�\@��@(�@.{@8Q�@E�@Y��@aG�@n{@��\@���@���@�@�p�@��
@�{@�@��H@�ff@���@У�@�p�@��
@�=q@�z�@�(�A ��AA
=qAp�A��AA=qA��A!�A'
=A)��A.{A333A5A9��A>�RAA�AEAK�AMp�AQ�AW�AZ�HA^{Ac�
Ag
=Aj�HAp  Atz�AxQ�A|��A���A�=qA���A��RA�Q�A�33A��A��RA��A��A��A�  A��\A�(�A�{A���A��HA���A�\)A���A��A��A�  A��A��A�A���A��A�(�A�
=A�Q�A\A�p�A�\)Aȣ�A��HA�p�A�
=A���A��
A�p�A�
=A��A�(�A�A�\)A�=qA��
A��A�A�\A��
A�A�  A��A�33A�{A�  A���A��
A�A��B ��B��B�HB�Bz�B��B�HB�Bz�B	B
ffB\)B��B��B=qB�B��Bp�BffB�B��Bp�B�\B�
B��Bp�BffB�B��Bp�B�RB�
B ��B!p�B"�\B#�
B$��B%p�B&�RB'�B(Q�B)p�B*�RB+�B,Q�B-B.�\B/\)B0Q�B1��B2=qB333B4z�B5p�B6=qB6�HB8(�B9G�B:{B:�RB<  B=�B=�B>�\B?�
B@��BA��BBffBC�BD��BEG�BF=qBG\)BHz�BIG�BI�BK
=BL(�BMG�BM�BN�RBP  BQ�BQBR�\BS�BT��BUp�BVffBW�BXz�BY�BZ=qB[�B\(�B\��B^{B_33B_�
B`��BaBc
=Bc�Bdz�Be��Bf�RBg�Bh(�BiG�Bj�\Bk\)Bl  Bl��Bn=qBn�HBo�Bp��Br{Br�\Bs\)Btz�Bu��BvffBw
=Bx  By�Bz{Bz�RB|  B}�B}B~�RB�B�z�B���B�33B��B�ffB���B��B��
B�=qB��\B�33B��
B�=qB��\B��B�B�=qB��\B��B�B�=qB���B�33B��
B�Q�B��RB�33B��
B�ffB��RB�33B��
B�Q�B��RB��B�B�Q�B���B��B�B�Q�B��\B��B��B�  B�z�B��B���B��B�z�B��B�\)B��
B�z�B��HB�33B��B�Q�B��RB�
=B��B�=qB���B���B���B�(�B�ffB���B��B��B�Q�B��HB�p�B�B�(�B���B�\)B��B�(�B���B�33B��B�=qB��RB�
=B�p�B�{B��\B���B�G�B��B�ffB��RB�G�B��
B�Q�B���B�
=B��B�(�B�z�B��HB�p�B�  B�ffB��RB�\)B��
B�=qB��\B��B�B�{B�ffB�
=B���B��
B�=qB���B�\)B��B�  B��\B��B�\)B�B�Q�B���B��B��B�  B��\B��HB�33B�B�=qB���B��HB��B��B�=qB£�B�33BîB�{B�Q�BĸRB�G�B�B�  B�ffB��HB�p�B��
B�(�B�z�B���B�p�B��B�Q�Bʣ�B���B˅B�{B�z�B��HB�33B�B�=qB���B��B�\)B�  B�z�B���B��B�B�Q�Bң�B�
=Bә�B�(�Bԣ�B�
=B�\)B�  B֏\B�
=B�\)B�B�Q�B��HB�G�BٮB�{BڸRB�33BۮB��B܏\B�
=B�\)BݮB�Q�B���B��B߅B�  B��\B���B�G�B�B�Q�B��HB�G�B㙚B�{B��B�33B噚B�  B�ffB�
=B癚B�(�B�\B���B�p�B�  B��B�
=B�p�B��B�\B��B�p�B��
B�ffB�
=B�p�B��
B�z�B�
=B�B��B�Q�B�
=B�B�  B�ffB���B�\)B�  B���B�
=B�p�B�  B��\B��B���B�{B�ffB�
=B��B�(�B���B�
=B���B�Q�B��HB�\)B��
C (�C p�C C{CQ�C�CC
=CQ�C��C�C�C\)C��C��CG�Cp�C�C��C=qC�CC��C(�Cz�C��C
=C=qCp�C�C��CG�Cz�C�C�HC	33C	z�C	�RC	�HC
�C
ffC
�C
�HC{CQ�C��C�HC{C=qCz�CC  C=qC\)C��C�C33C\)C�\C��C�CffC��C�
C
=CG�C��C�HC33Cp�C��C��C{C\)C��C��C
=CffC�C�HC�CffC�C  CG�C�CC
=C\)C��C�HC{C\)C�RC  C=qCp�C�C  CG�Cz�C�C  CG�C�C�C�C33C�C��C  C=qC�\C�HC�C\)C��C�HC33C�CC��C=qC��C�HC �C \)C ��C ��C!G�C!�C!��C"
=C"\)C"�C#  C#=qC#z�C#C$�C$p�C$�C$�C%33C%z�C%�HC&33C&z�C&�RC&��C'Q�C'�C'�C(33C(p�C(�RC){C)ffC)�C)�HC*33C*p�C*��C+�C+Q�C+�\C+�
C,(�C,z�C,��C-
=C-=qC-�\C-�C.=qC.p�C.�RC/  C/G�C/��C/�C0(�C0ffC0��C0��C1G�C1��C1�C2(�C2p�C2�C2��C3G�C3��C3��C433C4p�C4�RC5
=C5\)C5�C5��C633C6p�C6�RC7
=C7\)C7�C7�C833C8z�C8�
C933C9�C9C:{C:Q�C:��C:�C;=qC;��C;�HC<=qC<�C<�RC=  C=Q�C=�C=��C>G�C>�\C>��C?{C?ffC?C@
=C@\)C@�C@�CA(�CAp�CA��CB(�CBp�CB�RCB�CC33CCz�CC��CD�CDp�CD�RCD��CEG�CE�CECF{CFffCFCG{CGQ�CG�\CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                     ?�  ?�@@  @�  @��R@�G�@�G�@��RA�RA   A+�A@  A`  A�  A�Q�A�  A��A�  A�Q�A�Q�A�B   B(�B�
B�
B�
B((�B0(�B8(�B@  BG�
BP  BX  B`(�Bhz�BpQ�Bx(�B�  B�  B��B��
B��B��B�  B�  B�{B�{B�  B��B��
B��B�{B�{B��B�  B��B��
B��
B�  B�(�B�{B��B��B��B�  B�{B��B��B�  C   C��C  C  C��C

=C  C��C��C  C��C  C  C  C��C  C 
=C!��C#��C&{C(
=C*  C+��C.  C0{C2{C4
=C6  C7�C:  C<
=C>  C@
=CB
=CD
=CE��CG��CJ  CL
=CN
=CP{CR
=CT  CU�CX
=CZ  C\  C^  C_��Cb  Cd  Cf  Cg��Cj
=Cl  Cn  Co��Cr  Ct
=Cv
=Cx  Cz  C|
=C~
=C�  C���C�  C�  C�C�  C���C���C�  C�  C�
=C�
=C�C���C���C�  C�C�  C�C�  C�  C�C�  C���C�  C�  C�  C�  C�C���C���C�C���C�  C�C�  C���C�  C���C���C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�C���C���C�  C���C���C���C���C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C���C���C���C���C�C�  C���C�  C�  C�C�\C�C���C���C���C�  C�
=C�
=C�
=C�
=C�  C��C���C���C���C���C���C���C�C�
=C�  C�
=C�C�C�C�  C���C�  C�C�  C�  C�C�C�
=C�C�  C�C�C�C�C�  C�C�  C�C�
=D   D ��D�D}qD�qDz�D�qD��D  D� D�qD� D  D� D  D� D  D� D�qD	� D
  D
� D�D� D  D}qD  D��D�qDz�D�qD� D  D}qD  D� D�qD� D�D� D  D}qD�qD� D�D}qD�qD� D�D��D�qD� D  D� D�qD}qD  Dz�D�qD� D�D�DD��D �D � D!�D!� D!��D"}qD#�D#}qD#�qD$}qD$��D%z�D%�qD&� D'  D'� D(  D(� D)�D)��D*  D*��D+D+��D,  D,��D-D-��D.  D.��D/�D/� D0  D0��D1  D1}qD1��D2}qD3  D3�D4  D4z�D5�D5��D6  D6� D6�qD7}qD7�qD8}qD9  D9}qD9�RD:}qD;  D;� D<  D<� D<�qD=��D>D>��D?  D?��D@  D@� D@�qDA}qDB  DB��DB�qDC}qDD  DD� DE  DE}qDE�qDF� DG  DG� DH  DH� DI  DI��DJ�DJ��DK�DK��DL�DL�DM  DM� DN�DN��DO  DO}qDP  DPz�DQ  DQ� DQ�qDR� DS  DS� DT  DT}qDU  DU��DV  DV� DW  DW��DX  DX� DYDY� DY��DZ}qD[  D[� D\  D\� D\�qD]}qD]�qD^��D_�D_�D`D`��Da�Da��Db  Db� Dc�Dc��Dd  Dd��DeDe� De��Dfz�Dg  Dg� Dg�qDh}qDi  Di��Di�qDj}qDk  Dk� Dk�qDl� Dm�Dm}qDm�qDn��Do�Do� Dp  Dp}qDq�Dq��Dr  Dr}qDr�qDs}qDs�qDt}qDt��Du}qDu�qDv}qDv�qDw� Dx  Dx� Dy  Dy� Dz  Dz}qDz�qD{� D{�qD|}qD}�D}� D~  D~�D�D� D�qD�>�D�~�D���D���D�>�D�~�D�� D�  D�>�D�~�D���D���D�>�D�� D���D��qD�>�D��HD�D�  D�@ D��HD��HD�HD�@ D��HD��HD�HD�B�D��HD�D�HD�@ D�� D���D�  D�AHD�� D�� D���D�@ D��HD�� D�HD�@ D�}qD��qD���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�@ D��HD��HD�  D�B�D��HD��HD�HD�>�D�~�D�� D�  D�>�D�}qD�� D�  D�=qD�~�D���D�  D�@ D���D�� D��qD�@ D�� D�� D�  D�@ D���D�D�HD�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�� D�� D��D�B�D��HD��HD���D�>�D�� D���D���D�@ D�� D�� D�HD�@ D�}qD���D�HD�AHD�� D��HD���D�>�D�~�D���D�  D�AHD�~�D��qD��qD�=qD�~�D���D�  D�AHD���D�D�HD�=qD�|)D�� D�HD�>�D�}qD���D�  D�AHD��HD�� D��qD�>�D��HD��HD�HD�AHD��HD��HD�  D�@ D�~�D���D�HD�AHD�� D��HD��D�@ D�~�D���D�  D�@ D�� D���D���D�>�D�~�D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�@ D��HD��HD���D�@ D�� D�� D�HD�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD��D�B�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D���D�  D�AHD�� D���D��qD�>�D�� D��HD�HD�AHD��HD�� D�  D�@ D�� D���D���D�>�D�~�D��qD�  D�B�D��HD�� D�HD�AHD��HD�� D�  D�@ D�~�D���D���D�>�D�� D��HD�HD�@ D�~�D��HD�HD�AHDHD��HD�HD�@ DÀ D��HD��D�AHDĀ Dľ�D���D�@ DŁHD��HD�HD�AHDƁHD�� D�  D�@ Dǀ D��HD�  D�>�DȁHD�D�HD�AHDɁHD�� D�  D�AHDʀ D�� D�  D�@ DˁHD�� D�  D�>�D́HD�� D���D�@ D̀ D�� D�HD�AHD΀ D�� D���D�>�Dπ D��HD�  D�>�D�~�Dо�D���D�>�D�~�D�� D�  D�>�DҀ D��HD�  D�AHDӀ DӾ�D���D�@ D�~�DԽqD�  D�B�DՀ D�� D�  D�>�D�~�D�� D�HD�@ D׀ D�� D���D�>�D�}qDؾ�D�HD�AHDـ D��HD�  D�>�DځHD�D�HD�@ Dۀ D۾�D���D�>�D�~�D�� D�HD�@ D�~�D�� D���D�>�Dނ�D��HD�HD�@ D�~�D߾�D�  D�@ D�~�DྸD���D�@ D� D�� D���D�>�D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�  D�>�D� D�� D�HD�>�D�~�D澸D���D�@ D�HD�� D�HD�AHD�~�D辸D�  D�B�D� D龸D�  D�@ D�HD��HD�  D�@ D�HD뾸D���D�@ D�HD��HD���D�>�D� D���D���D�>�D� DD���D�>�D� D�� D���D�AHD��HD��HD�  D�AHD�HD�D���D�>�D� D�D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�=qD�~�D��HD�HD�AHD�� D��HD�  D�@ D��HD�� D�  D�@ D�~�D��HD�HD�@ D�}qG�O�?.{?8Q�?W
=?�=q?�33?Ǯ?�(�@�\@��@(�@.{@8Q�@E�@Y��@aG�@n{@��\@���@���@�@�p�@��
@�{@�@��H@�ff@���@У�@�p�@��
@�=q@�z�@�(�A ��AA
=qAp�A��AA=qA��A!�A'
=A)��A.{A333A5A9��A>�RAA�AEAK�AMp�AQ�AW�AZ�HA^{Ac�
Ag
=Aj�HAp  Atz�AxQ�A|��A���A�=qA���A��RA�Q�A�33A��A��RA��A��A��A�  A��\A�(�A�{A���A��HA���A�\)A���A��A��A�  A��A��A�A���A��A�(�A�
=A�Q�A\A�p�A�\)Aȣ�A��HA�p�A�
=A���A��
A�p�A�
=A��A�(�A�A�\)A�=qA��
A��A�A�\A��
A�A�  A��A�33A�{A�  A���A��
A�A��B ��B��B�HB�Bz�B��B�HB�Bz�B	B
ffB\)B��B��B=qB�B��Bp�BffB�B��Bp�B�\B�
B��Bp�BffB�B��Bp�B�RB�
B ��B!p�B"�\B#�
B$��B%p�B&�RB'�B(Q�B)p�B*�RB+�B,Q�B-B.�\B/\)B0Q�B1��B2=qB333B4z�B5p�B6=qB6�HB8(�B9G�B:{B:�RB<  B=�B=�B>�\B?�
B@��BA��BBffBC�BD��BEG�BF=qBG\)BHz�BIG�BI�BK
=BL(�BMG�BM�BN�RBP  BQ�BQBR�\BS�BT��BUp�BVffBW�BXz�BY�BZ=qB[�B\(�B\��B^{B_33B_�
B`��BaBc
=Bc�Bdz�Be��Bf�RBg�Bh(�BiG�Bj�\Bk\)Bl  Bl��Bn=qBn�HBo�Bp��Br{Br�\Bs\)Btz�Bu��BvffBw
=Bx  By�Bz{Bz�RB|  B}�B}B~�RB�B�z�B���B�33B��B�ffB���B��B��
B�=qB��\B�33B��
B�=qB��\B��B�B�=qB��\B��B�B�=qB���B�33B��
B�Q�B��RB�33B��
B�ffB��RB�33B��
B�Q�B��RB��B�B�Q�B���B��B�B�Q�B��\B��B��B�  B�z�B��B���B��B�z�B��B�\)B��
B�z�B��HB�33B��B�Q�B��RB�
=B��B�=qB���B���B���B�(�B�ffB���B��B��B�Q�B��HB�p�B�B�(�B���B�\)B��B�(�B���B�33B��B�=qB��RB�
=B�p�B�{B��\B���B�G�B��B�ffB��RB�G�B��
B�Q�B���B�
=B��B�(�B�z�B��HB�p�B�  B�ffB��RB�\)B��
B�=qB��\B��B�B�{B�ffB�
=B���B��
B�=qB���B�\)B��B�  B��\B��B�\)B�B�Q�B���B��B��B�  B��\B��HB�33B�B�=qB���B��HB��B��B�=qB£�B�33BîB�{B�Q�BĸRB�G�B�B�  B�ffB��HB�p�B��
B�(�B�z�B���B�p�B��B�Q�Bʣ�B���B˅B�{B�z�B��HB�33B�B�=qB���B��B�\)B�  B�z�B���B��B�B�Q�Bң�B�
=Bә�B�(�Bԣ�B�
=B�\)B�  B֏\B�
=B�\)B�B�Q�B��HB�G�BٮB�{BڸRB�33BۮB��B܏\B�
=B�\)BݮB�Q�B���B��B߅B�  B��\B���B�G�B�B�Q�B��HB�G�B㙚B�{B��B�33B噚B�  B�ffB�
=B癚B�(�B�\B���B�p�B�  B��B�
=B�p�B��B�\B��B�p�B��
B�ffB�
=B�p�B��
B�z�B�
=B�B��B�Q�B�
=B�B�  B�ffB���B�\)B�  B���B�
=B�p�B�  B��\B��B���B�{B�ffB�
=B��B�(�B���B�
=B���B�Q�B��HB�\)B��
C (�C p�C C{CQ�C�CC
=CQ�C��C�C�C\)C��C��CG�Cp�C�C��C=qC�CC��C(�Cz�C��C
=C=qCp�C�C��CG�Cz�C�C�HC	33C	z�C	�RC	�HC
�C
ffC
�C
�HC{CQ�C��C�HC{C=qCz�CC  C=qC\)C��C�C33C\)C�\C��C�CffC��C�
C
=CG�C��C�HC33Cp�C��C��C{C\)C��C��C
=CffC�C�HC�CffC�C  CG�C�CC
=C\)C��C�HC{C\)C�RC  C=qCp�C�C  CG�Cz�C�C  CG�C�C�C�C33C�C��C  C=qC�\C�HC�C\)C��C�HC33C�CC��C=qC��C�HC �C \)C ��C ��C!G�C!�C!��C"
=C"\)C"�C#  C#=qC#z�C#C$�C$p�C$�C$�C%33C%z�C%�HC&33C&z�C&�RC&��C'Q�C'�C'�C(33C(p�C(�RC){C)ffC)�C)�HC*33C*p�C*��C+�C+Q�C+�\C+�
C,(�C,z�C,��C-
=C-=qC-�\C-�C.=qC.p�C.�RC/  C/G�C/��C/�C0(�C0ffC0��C0��C1G�C1��C1�C2(�C2p�C2�C2��C3G�C3��C3��C433C4p�C4�RC5
=C5\)C5�C5��C633C6p�C6�RC7
=C7\)C7�C7�C833C8z�C8�
C933C9�C9C:{C:Q�C:��C:�C;=qC;��C;�HC<=qC<�C<�RC=  C=Q�C=�C=��C>G�C>�\C>��C?{C?ffC?C@
=C@\)C@�C@�CA(�CAp�CA��CB(�CBp�CB�RCB�CC33CCz�CC��CD�CDp�CD�RCD��CEG�CE�CECF{CFffCFCG{CGQ�CG�\CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                     @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aٰ!AٸRAٶFAٶFAټjAپwAپwAپwA���A���AپwA���A���A�A�A�A�ĜA�ĜA�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��/A��/A��/A��
A��A��#A���A���A���A���A���A�Aٝ�A�S�A��#A؃A�K�A�7LAӛ�AѲ-Aκ^A���A�&�A̩�Aˇ+A��TA�(�A��AÕ�A�5?A��A��TA�A��wA��RA��RA��9A��9A��jA��A��A��/A���A�x�A���A�ZA��A�ffA�K�A�
=A��DA���A��^A���A��A�
=A�A��
A�n�A��jA�7LA���A�(�A��PA��hA���A�v�A�ZA��A�~�A�33A��/A�l�A�p�A��mA�jA��A�9XA���A��A�hsA��`A��A�(�A�ZA���A��A��^A��A~�`A}��A|v�Az��AyoAv1Aq�AnI�Al�\Ak33Ait�Ah=qAf��Ae��Ad��Ab^5A^��A]t�AX��AW33AV�AS�
ARjAP�/AOO�AM��AK�AJ(�AI\)AHr�AG�^AF�`AES�AB1A@A>��A=��A<�\A:^5A7�A4�DA3p�A3?}A21'A.~�A-l�A,�/A,I�A+�
A+|�A+O�A+"�A*^5A)/A'��A&�DA#�A!`BA Q�AoA�RA�\AffA��A�AXA33A"�A�A�HA7LAz�A  AAv�AA�A�A  A/A��A9XA$�A�
Al�A�AM�A+Ar�A�Ax�A
�`A
~�A	�A	|�A	
=A��A�A~�A�mA�HAA��AA/A �/A ��A (�@�33@���@���@��+@�~�@�V@��^@��7@�V@�1'@��@���@��@�t�@��R@��@�I�@�/@�1'@@��H@�@�n�@�@�Q�@�"�@��y@�J@�Ĝ@�9X@�I�@�j@�z�@��@���@�5?@�X@܋D@���@�ƨ@۝�@�@�5?@�/@ו�@ְ!@�X@�V@Լj@�+@�v�@�p�@�t�@�^5@͉7@̛�@���@ʸR@�-@���@�O�@���@�j@��@Ǿw@Ǯ@ǥ�@Ǖ�@ǅ@�ȴ@��T@���@���@���@�%@�V@�%@�V@�V@�V@��@�V@��`@�j@�9X@��@�  @���@�o@�n�@�v�@+@�@��@�b@��;@î@Õ�@�"�@�V@���@���@�"�@��@�^5@�n�@���@�M�@�?}@�V@�r�@�Q�@�Z@�A�@� �@���@��@�I�@�I�@�I�@�9X@�1'@�(�@��m@��P@�dZ@�@�~�@�J@�@��@���@�O�@�V@�hs@��@�%@��/@��@�bN@�9X@�1@�ƨ@��y@�~�@�E�@���@��@���@��/@��/@���@���@�1'@��
@��@�;d@���@��T@��7@�7L@��@�1@���@���@�\)@�+@�"�@�+@�+@�o@��+@��-@�7L@�A�@��
@�|�@�
=@���@���@�v�@�ff@�5?@���@�x�@�G�@�&�@���@��9@�bN@�1'@��@�1@��@���@���@���@�-@��@��@���@��h@�x�@�?}@��@���@�Q�@��;@���@���@���@��P@��@�"�@��h@���@��j@���@�j@�I�@�1'@�b@��@�dZ@���@�$�@���@���@��-@�7L@�j@�  @��
@���@��w@���@��@���@���@�M�@���@�hs@���@���@�Q�@�Q�@��@��m@���@���@��@�S�@��@�@��@���@�ȴ@���@��R@�~�@�$�@��@�{@��7@��`@���@���@��D@�z�@�9X@��;@�33@�ȴ@�n�@��@���@�hs@���@�z�@�1'@�b@��m@���@�dZ@�33@���@���@�v�@�ff@�M�@�$�@�J@��#@���@�O�@�7L@�/@�&�@�V@��@���@� �@��m@��@�K�@��@���@�~�@�ff@�M�@�=q@��#@��@��u@�j@�bN@�I�@�A�@�A�@�A�@�A�@�1'@��w@���@�t�@�+@��@���@�V@�5?@��@�J@��@�@��h@�p�@�X@�/@��@��@��@�V@�Ĝ@��u@�j@� �@�ƨ@���@�S�@�;d@��@���@��+@�v�@�E�@�J@��@���@��@���@��u@��D@�A�@�b@��@K�@~�@~�+@~5?@~@}@}�@}`B@}O�@}V@|��@|Z@|9X@|(�@|1@{��@{��@{�
@{ƨ@{t�@{33@z��@zM�@y��@x��@xr�@x  @w�@w\)@w�@v��@u�@uO�@u�@t�/@t�@s��@s"�@so@so@r�@r~�@r�@qx�@q&�@pĜ@pA�@p1'@o�;@o�P@n�y@nV@m@m�h@mp�@l�@k��@ko@j=q@i�7@h��@h �@g�;@g��@g��@g��@g�P@g;d@f{@e`B@e/@d��@d�D@dj@d9X@d1@c�
@c��@cS�@c@b��@b��@b~�@a��@a7L@`��@`�9@`�@`A�@`  @_��@_\)@_+@^��@^{@]p�@\�@\�D@[�F@[S�@Z�!@Zn�@ZJ@Yx�@Xr�@W�@V�R@VV@V@U@Up�@U?}@U/@U�@T�@T�j@Tz�@T9X@T�@S�
@St�@S@Rn�@Q�@Q��@Qhs@Q&�@Q%@Q%@Q%@Q%@P�9@O�@O
=@N�y@N��@M��@M�@M`B@L��@Lz�@L�@KS�@Ko@K@J�@I�#@IX@I%@Hr�@G��@G�@FV@E@E��@E��@E�h@EO�@EV@D�@D�j@DZ@D1@C�F@CdZ@Co@B��@B��@B~�@B�@A��@A&�@@�@@A�@@ �@?�@?��@?|�@?|�@?|�@?l�@?;d@>�y@>�+@=`B@;�m@:�H@:M�@:J@9��@9x�@9&�@9%@8��@8 �@7+@6�y@6��@6E�@5@5p�@5O�@5V@4Z@4�@3��@3�
@3t�@3o@2��@2��@2~�@2^5@2-@1�@1X@0��@0A�@/\)@.�@.��@.�+@.v�@.ff@.E�@.$�@.{@-�T@-�h@,��@,�@,j@,I�@,1@+ƨ@+S�@*�@*~�@*�@)�#@)��@)��@)��@)X@(�`@(r�@(b@'�@'��@'\)@&��@&�@&�R@&v�@&ff@&5?@%�@%@%�-@%��@%�@%p�@%`B@%`B@%?}@%/@%V@$��@$�@$�/@$��@$�j@$�j@$�@$j@$(�@$1@#��@#��@#C�@"�H@"��@"�!@"�!@"~�@"�@!�^@!X@!7L@!&�@!%@ �`@ ��@ �`@ ��@ Ĝ@ Q�@   @�P@�@ȴ@E�@��@��@�@�@�@�@p�@`B@�@�j@��@�D@j@I�@(�@��@��@��@S�@C�@"�@o@��@n�@J@�@�#@��@��@X@G�@�@��@�@1'@b@�@�@��@�w@K�@ȴ@��@�+@5?@$�@{@{@@@�@��@@�h@p�@/@V@V@�@�/@�j@�D@(�@��@��@�m@�m@ƨ@��@��@�@S�@o@�H@~�@-@J@��@�#@��@x�@7L@�@�@�`@Ĝ@��@�u@r�@bN@A�@A�@ �@  @  @�;@��@�w@�w@�w@��@�P@|�@l�@\)AٸRAٺ^AٮAٟ�Aٺ^AٸRAٶFAټjAٸRAٴ9AٸRAٲ-Aٴ9Aٺ^AٶFAٴ9AٸRAٸRAٶFAټjAټjAټjAپwAپwAٺ^A���AپwAٺ^A�AپwAټjA���A���AټjAپwA�AپwAټjA���A�AټjAپwA�AټjA���A�AپwA���A�AټjAپwA�AټjA���A�AپwAپwA�ĜAپwAپwA���A�AپwA���A�AپwA�ĜA�A���A�ĜA�ĜA���A�ƨA�A���A�A�ƨA���A�A�ƨA�ĜA���A�A�ƨA�ĜA�A�ƨA�ƨA���A�ĜA�ƨA�A�A�ƨA�A�ĜA�ȴA�ƨA�A�ĜA�ƨA�ĜA�ĜA�ȴA�ĜA�A�ƨA�ȴA�A�ĜA�ȴA�ƨA�ĜA�ȴA���A�ƨA�ĜA���A�ƨA�ĜA�ȴA���A�ƨA�ȴA���A���A�ƨA�ƨA���A�ȴA�ƨA�ȴA���A�ȴA�ȴA���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A���A���A���A��
A���A���A���A��
A���A���A��
A��#A��A���A��A��/A��/A��A��A��/A��/A��A��#A��/A��#A��A��#A��;A��#A��A��;A��;A��#A��#A��HA��/A��#A��/A��;A��/A��A��/A��;A��;A��A��#A��#A��
A���A���A��
A��A��
A��
A��A��A���A���A��#A��A��
A���A��A��#A��
A��
A��#A��#A��#A��#A��HA��#A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ĜAپwA�A�Aٺ^Aٺ^AپwAپwAٺ^AٶFA٩�A٣�Aه+A�t�A�r�A�t�A�n�A�\)A�^5A�XA�S�A�M�A�=qA�1'A��A�
=A��`A��;A��A���A���A�ȴAظRAجAجAا�A؛�AؓuA؇+A؅A�l�A�9XA���Aש�AדuA׋DA�n�A�XA�&�A���A�ȴA֬A�|�A��A��
A�`BA�JA���AԁA�^5A�ZA�O�A�;dA��A��`AӴ9Aӗ�A�l�A�bA��TA���AҸRAґhA�dZA�;dA���A�dZA��;A�jA��Aω7A�7LA��;AΣ�A΃A�t�A�n�A�VA��A�1A�  A��A��mA��A�ĜAͩ�A�p�A�^5A�S�A�O�A�C�A�?}A�&�A�JA���A��yA���A���A���A̶FA̝�ȂhȂhA̍PA�~�A�p�A�hsA�E�A�ȴA�r�A���A�ĜAʗ�A�p�A�S�A�7LA��A�l�A��#AȃA���A�ZA�$�A�ƨA�VA�-A�VA��A���AčPA�t�A�ZA�5?A�+A�&�A��A���A���AÉ7A�ffA�|�AÅAËDAÙ�Aô9A���AüjAìAç�AÇ+A�XA���A���A§�A�=qA���A���A�r�A�VA�G�A�/A� �A�VA���A��/A��HA��yA��
A��
A���A���A�ȴA���A��wA��jA��jA��wA�A�A���A��wA��^A��jA��wA��wA��^A��FA��FA��^A��^A��RA��FA��RA��^A��RA��RA��FA��9A��FA��RA��RA��9A��-A��-A��9A��FA��FA��9A��-A��9A��FA��9A��-A��-A��9A��9A��-A��!A��9A��FA���A���A��jA��wA�ĜA���A���A���A���A��A��#A��jA�ZA�9XA�$�A�bA��`A��jA�t�A�G�A�  A��hA�;dA�{A���A��HA���A��FA���A��PA�~�A�Q�A��A��TA�ĜA��A�ZA�1'A�1A���A���A�S�A�33A�{A��;A��TA��;A���A�ȴA�A�A��jA���A��\A��A�v�A�hsA�S�A�7LA��A��mA���A��A��hA��A��/A���A���A��+A�l�A�ffA�dZA�^5A�ZA�ZA�XA�\)A�^5A�VA�I�A�C�A�9XA�5?A�/A�+A�"�A��A�
=A��RA�VA�+A���A� �A�A���A�t�A� �A���A�A�ƨA�ȴA�ȴA���A�ƨA���A�ĜA��jA��-A��-A��-A��FA��FA��-A���A���A��DA��PA�~�A�jA�^5A� �A��`A���A��FA���A��PA�hsA�A�A�JA�ȴA�\)A�5?A���A�A�&�A�ƨA�n�A�M�A�/A��mA�hsA��yA�jA�`BA�C�A�(�A�  A��;A���A�E�A�{A��TA�?}A��A��;A���A���A��7A��+A��A��A�M�A��A�~�A�%A��A��7A���A�p�A�hsA�`BA�\)A�XA�G�A�;dA�-A�&�A�oA�  A���A��A��mA�ƨA�hsA�E�A��A�l�A�-A��A���A�bA��^A���A���A��uA��uA���A���A���A��uA��uA��uA���A��PA�p�A�Q�A�oA��TA�x�A�^5A�I�A��A��A��
A�v�A�5?A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                     Aٰ!AٸRAٶFAٶFAټjAپwAپwAپwA���A���AپwA���A���A�A�A�A�ĜA�ĜA�ĜA�ƨA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��#A��/A��/A��/A��
A��A��#A���A���A���A���A���A�Aٝ�A�S�A��#A؃A�K�A�7LAӛ�AѲ-Aκ^A���A�&�A̩�Aˇ+A��TA�(�A��AÕ�A�5?A��A��TA�A��wA��RA��RA��9A��9A��jA��A��A��/A���A�x�A���A�ZA��A�ffA�K�A�
=A��DA���A��^A���A��A�
=A�A��
A�n�A��jA�7LA���A�(�A��PA��hA���A�v�A�ZA��A�~�A�33A��/A�l�A�p�A��mA�jA��A�9XA���A��A�hsA��`A��A�(�A�ZA���A��A��^A��A~�`A}��A|v�Az��AyoAv1Aq�AnI�Al�\Ak33Ait�Ah=qAf��Ae��Ad��Ab^5A^��A]t�AX��AW33AV�AS�
ARjAP�/AOO�AM��AK�AJ(�AI\)AHr�AG�^AF�`AES�AB1A@A>��A=��A<�\A:^5A7�A4�DA3p�A3?}A21'A.~�A-l�A,�/A,I�A+�
A+|�A+O�A+"�A*^5A)/A'��A&�DA#�A!`BA Q�AoA�RA�\AffA��A�AXA33A"�A�A�HA7LAz�A  AAv�AA�A�A  A/A��A9XA$�A�
Al�A�AM�A+Ar�A�Ax�A
�`A
~�A	�A	|�A	
=A��A�A~�A�mA�HAA��AA/A �/A ��A (�@�33@���@���@��+@�~�@�V@��^@��7@�V@�1'@��@���@��@�t�@��R@��@�I�@�/@�1'@@��H@�@�n�@�@�Q�@�"�@��y@�J@�Ĝ@�9X@�I�@�j@�z�@��@���@�5?@�X@܋D@���@�ƨ@۝�@�@�5?@�/@ו�@ְ!@�X@�V@Լj@�+@�v�@�p�@�t�@�^5@͉7@̛�@���@ʸR@�-@���@�O�@���@�j@��@Ǿw@Ǯ@ǥ�@Ǖ�@ǅ@�ȴ@��T@���@���@���@�%@�V@�%@�V@�V@�V@��@�V@��`@�j@�9X@��@�  @���@�o@�n�@�v�@+@�@��@�b@��;@î@Õ�@�"�@�V@���@���@�"�@��@�^5@�n�@���@�M�@�?}@�V@�r�@�Q�@�Z@�A�@� �@���@��@�I�@�I�@�I�@�9X@�1'@�(�@��m@��P@�dZ@�@�~�@�J@�@��@���@�O�@�V@�hs@��@�%@��/@��@�bN@�9X@�1@�ƨ@��y@�~�@�E�@���@��@���@��/@��/@���@���@�1'@��
@��@�;d@���@��T@��7@�7L@��@�1@���@���@�\)@�+@�"�@�+@�+@�o@��+@��-@�7L@�A�@��
@�|�@�
=@���@���@�v�@�ff@�5?@���@�x�@�G�@�&�@���@��9@�bN@�1'@��@�1@��@���@���@���@�-@��@��@���@��h@�x�@�?}@��@���@�Q�@��;@���@���@���@��P@��@�"�@��h@���@��j@���@�j@�I�@�1'@�b@��@�dZ@���@�$�@���@���@��-@�7L@�j@�  @��
@���@��w@���@��@���@���@�M�@���@�hs@���@���@�Q�@�Q�@��@��m@���@���@��@�S�@��@�@��@���@�ȴ@���@��R@�~�@�$�@��@�{@��7@��`@���@���@��D@�z�@�9X@��;@�33@�ȴ@�n�@��@���@�hs@���@�z�@�1'@�b@��m@���@�dZ@�33@���@���@�v�@�ff@�M�@�$�@�J@��#@���@�O�@�7L@�/@�&�@�V@��@���@� �@��m@��@�K�@��@���@�~�@�ff@�M�@�=q@��#@��@��u@�j@�bN@�I�@�A�@�A�@�A�@�A�@�1'@��w@���@�t�@�+@��@���@�V@�5?@��@�J@��@�@��h@�p�@�X@�/@��@��@��@�V@�Ĝ@��u@�j@� �@�ƨ@���@�S�@�;d@��@���@��+@�v�@�E�@�J@��@���@��@���@��u@��D@�A�@�b@��@K�@~�@~�+@~5?@~@}@}�@}`B@}O�@}V@|��@|Z@|9X@|(�@|1@{��@{��@{�
@{ƨ@{t�@{33@z��@zM�@y��@x��@xr�@x  @w�@w\)@w�@v��@u�@uO�@u�@t�/@t�@s��@s"�@so@so@r�@r~�@r�@qx�@q&�@pĜ@pA�@p1'@o�;@o�P@n�y@nV@m@m�h@mp�@l�@k��@ko@j=q@i�7@h��@h �@g�;@g��@g��@g��@g�P@g;d@f{@e`B@e/@d��@d�D@dj@d9X@d1@c�
@c��@cS�@c@b��@b��@b~�@a��@a7L@`��@`�9@`�@`A�@`  @_��@_\)@_+@^��@^{@]p�@\�@\�D@[�F@[S�@Z�!@Zn�@ZJ@Yx�@Xr�@W�@V�R@VV@V@U@Up�@U?}@U/@U�@T�@T�j@Tz�@T9X@T�@S�
@St�@S@Rn�@Q�@Q��@Qhs@Q&�@Q%@Q%@Q%@Q%@P�9@O�@O
=@N�y@N��@M��@M�@M`B@L��@Lz�@L�@KS�@Ko@K@J�@I�#@IX@I%@Hr�@G��@G�@FV@E@E��@E��@E�h@EO�@EV@D�@D�j@DZ@D1@C�F@CdZ@Co@B��@B��@B~�@B�@A��@A&�@@�@@A�@@ �@?�@?��@?|�@?|�@?|�@?l�@?;d@>�y@>�+@=`B@;�m@:�H@:M�@:J@9��@9x�@9&�@9%@8��@8 �@7+@6�y@6��@6E�@5@5p�@5O�@5V@4Z@4�@3��@3�
@3t�@3o@2��@2��@2~�@2^5@2-@1�@1X@0��@0A�@/\)@.�@.��@.�+@.v�@.ff@.E�@.$�@.{@-�T@-�h@,��@,�@,j@,I�@,1@+ƨ@+S�@*�@*~�@*�@)�#@)��@)��@)��@)X@(�`@(r�@(b@'�@'��@'\)@&��@&�@&�R@&v�@&ff@&5?@%�@%@%�-@%��@%�@%p�@%`B@%`B@%?}@%/@%V@$��@$�@$�/@$��@$�j@$�j@$�@$j@$(�@$1@#��@#��@#C�@"�H@"��@"�!@"�!@"~�@"�@!�^@!X@!7L@!&�@!%@ �`@ ��@ �`@ ��@ Ĝ@ Q�@   @�P@�@ȴ@E�@��@��@�@�@�@�@p�@`B@�@�j@��@�D@j@I�@(�@��@��@��@S�@C�@"�@o@��@n�@J@�@�#@��@��@X@G�@�@��@�@1'@b@�@�@��@�w@K�@ȴ@��@�+@5?@$�@{@{@@@�@��@@�h@p�@/@V@V@�@�/@�j@�D@(�@��@��@�m@�m@ƨ@��@��@�@S�@o@�H@~�@-@J@��@�#@��@x�@7L@�@�@�`@Ĝ@��@�u@r�@bN@A�@A�@ �@  @  @�;@��@�w@�w@�w@��@�P@|�@l�G�O�AٸRAٺ^AٮAٟ�Aٺ^AٸRAٶFAټjAٸRAٴ9AٸRAٲ-Aٴ9Aٺ^AٶFAٴ9AٸRAٸRAٶFAټjAټjAټjAپwAپwAٺ^A���AپwAٺ^A�AپwAټjA���A���AټjAپwA�AپwAټjA���A�AټjAپwA�AټjA���A�AپwA���A�AټjAپwA�AټjA���A�AپwAپwA�ĜAپwAپwA���A�AپwA���A�AپwA�ĜA�A���A�ĜA�ĜA���A�ƨA�A���A�A�ƨA���A�A�ƨA�ĜA���A�A�ƨA�ĜA�A�ƨA�ƨA���A�ĜA�ƨA�A�A�ƨA�A�ĜA�ȴA�ƨA�A�ĜA�ƨA�ĜA�ĜA�ȴA�ĜA�A�ƨA�ȴA�A�ĜA�ȴA�ƨA�ĜA�ȴA���A�ƨA�ĜA���A�ƨA�ĜA�ȴA���A�ƨA�ȴA���A���A�ƨA�ƨA���A�ȴA�ƨA�ȴA���A�ȴA�ȴA���A���A�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A���A���A���A��
A���A���A���A��
A���A���A��
A��#A��A���A��A��/A��/A��A��A��/A��/A��A��#A��/A��#A��A��#A��;A��#A��A��;A��;A��#A��#A��HA��/A��#A��/A��;A��/A��A��/A��;A��;A��A��#A��#A��
A���A���A��
A��A��
A��
A��A��A���A���A��#A��A��
A���A��A��#A��
A��
A��#A��#A��#A��#A��HA��#A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A�ȴA���A���A�ĜAپwA�A�Aٺ^Aٺ^AپwAپwAٺ^AٶFA٩�A٣�Aه+A�t�A�r�A�t�A�n�A�\)A�^5A�XA�S�A�M�A�=qA�1'A��A�
=A��`A��;A��A���A���A�ȴAظRAجAجAا�A؛�AؓuA؇+A؅A�l�A�9XA���Aש�AדuA׋DA�n�A�XA�&�A���A�ȴA֬A�|�A��A��
A�`BA�JA���AԁA�^5A�ZA�O�A�;dA��A��`AӴ9Aӗ�A�l�A�bA��TA���AҸRAґhA�dZA�;dA���A�dZA��;A�jA��Aω7A�7LA��;AΣ�A΃A�t�A�n�A�VA��A�1A�  A��A��mA��A�ĜAͩ�A�p�A�^5A�S�A�O�A�C�A�?}A�&�A�JA���A��yA���A���A���A̶FA̝�ȂhȂhA̍PA�~�A�p�A�hsA�E�A�ȴA�r�A���A�ĜAʗ�A�p�A�S�A�7LA��A�l�A��#AȃA���A�ZA�$�A�ƨA�VA�-A�VA��A���AčPA�t�A�ZA�5?A�+A�&�A��A���A���AÉ7A�ffA�|�AÅAËDAÙ�Aô9A���AüjAìAç�AÇ+A�XA���A���A§�A�=qA���A���A�r�A�VA�G�A�/A� �A�VA���A��/A��HA��yA��
A��
A���A���A�ȴA���A��wA��jA��jA��wA�A�A���A��wA��^A��jA��wA��wA��^A��FA��FA��^A��^A��RA��FA��RA��^A��RA��RA��FA��9A��FA��RA��RA��9A��-A��-A��9A��FA��FA��9A��-A��9A��FA��9A��-A��-A��9A��9A��-A��!A��9A��FA���A���A��jA��wA�ĜA���A���A���A���A��A��#A��jA�ZA�9XA�$�A�bA��`A��jA�t�A�G�A�  A��hA�;dA�{A���A��HA���A��FA���A��PA�~�A�Q�A��A��TA�ĜA��A�ZA�1'A�1A���A���A�S�A�33A�{A��;A��TA��;A���A�ȴA�A�A��jA���A��\A��A�v�A�hsA�S�A�7LA��A��mA���A��A��hA��A��/A���A���A��+A�l�A�ffA�dZA�^5A�ZA�ZA�XA�\)A�^5A�VA�I�A�C�A�9XA�5?A�/A�+A�"�A��A�
=A��RA�VA�+A���A� �A�A���A�t�A� �A���A�A�ƨA�ȴA�ȴA���A�ƨA���A�ĜA��jA��-A��-A��-A��FA��FA��-A���A���A��DA��PA�~�A�jA�^5A� �A��`A���A��FA���A��PA�hsA�A�A�JA�ȴA�\)A�5?A���A�A�&�A�ƨA�n�A�M�A�/A��mA�hsA��yA�jA�`BA�C�A�(�A�  A��;A���A�E�A�{A��TA�?}A��A��;A���A���A��7A��+A��A��A�M�A��A�~�A�%A��A��7A���A�p�A�hsA�`BA�\)A�XA�G�A�;dA�-A�&�A�oA�  A���A��A��mA�ƨA�hsA�E�A��A�l�A�-A��A���A�bA��^A���A���A��uA��uA���A���A���A��uA��uA��uA���A��PA�p�A�Q�A�oA��TA�x�A�^5A�I�A��A��A��
A�v�A�5?A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
{B
{�B
{B
{�B
{�B
|B
|�B
|�B
|�B
|�B
}"B
|�B
|�B
|�B
}"B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
}�B
}�B
~(B
}�B
}�B
~(B
}�B
}�B
~(B
~(B
~]B
~�B
~�B
~�B
~�B
}�B
~(B
}�B
}�B
}"B
|�B
|�B
|B
z�B
y	B
s�B
o B
k�B
bB
ZB
NB
M�B
a�B
kQB
u�B
qAB
kQB
kQB
o�B
��B
��B
��B
�HB
��B
�LB
��B
��B
��B
��B
�wB
�tB
�mBxB�B-wBT�Bt�Bt�B��B�B"�B.�BDgB:�B<B>�BT,B]dB_;BRTBH�B2-BF�B(�B1BFBB��B�
B�TB�]B��B�<B��B��B��B�=B�{B}�Bu�Be`B5�B_B�B�B
�xB
��B
��B
�BB
�aB
�eB
��B
�uB
zB
tB
jB
^5B
I�B
/�B
�B
$B
1B	��B	�B	�B	��B	�HB	��B	��B	�6B	��B	��B	�.B	�B	zB	w�B	rB	l�B	j�B	cTB	c B	[�B	V�B	S&B	I�B	EmB	=<B	7�B	5B	+�B	,qB	"�B	�B	MB	kB	�B	�B	.B	(B	"B	PB	�B	�B	B		7B	1B	B	B	B	B	�B	�B	7B	�B	�B	�B	#:B	)�B	-B	49B	6�B	?B	;�B	=�B	7�B	7LB	5�B	4�B	2�B	5�B	1�B	3�B	6FB	6�B	6B	<jB	C�B	EmB	DgB	D�B	F?B	HKB	J�B	K^B	H�B	I�B	M�B	K)B	MB	O�B	R�B	T,B	XB	X�B	Z�B	Y�B	\)B	_�B	aHB	aB	aHB	a�B	a�B	b�B	c B	a�B	a�B	b�B	c�B	b�B	aHB	^5B	^B	\]B	_B	[WB	U�B	S[B	Q�B	P}B	R�B	M�B	S&B	R�B	RTB	U�B	V�B	XB	YB	[WB	^�B	c�B	i�B	k�B	l�B	p�B	u�B	v+B	v�B	x�B	x�B	}"B	�{B	��B	��B	��B	��B	�	B	��B	�B	�YB	�_B	�B	��B	�FB	�B	�oB	�@B	��B	��B	��B	�VB	�\B	��B	��B	�bB	�bB	�B	��B	�9B	�XB	�B	�qB	��B	�B	��B	��B	B	��B	�B	ȀB	˒B	��B	��B	�jB	�pB	��B	�WB	��B	��B	�B	�B	��B	��B	�+B	��B	��B	�TB	��B	��B	�;B	�B	��B	�B	��B	��B	��B	�8B	��B	�(B	�.B	��B
 iB
;B
�B
	7B
	7B
	7B
	7B
�B
	lB
DB
B
�B
\B
.B
�B
�B
 B
�B
bB
�B
�B
�B
7B
�B
	B
qB
=B
�B
�B
OB
B
IB
IB
�B
�B
!�B
!�B
"�B
#B
$@B
&�B
'RB
(�B
)�B
+kB
*eB
+B
+B
+B
*�B
+6B
+�B
-CB
-wB
-CB
-B
-�B
/OB
.}B
/B
-CB
-wB
-�B
/OB
.�B
.�B
/B
/OB
/�B
/�B
/OB
0�B
2�B
5�B
6FB
7�B
6�B
6zB
6FB
5�B
5?B
5B
7B
8RB
8�B
9$B
8�B
9XB
9�B
:*B
:�B
:�B
;�B
>B
>�B
>�B
>wB
>wB
>BB
@B
@�B
@�B
A�B
EB
E9B
E9B
E�B
E�B
E9B
FtB
GB
J�B
I�B
JXB
J�B
JXB
I�B
K�B
L0B
L0B
LdB
L0B
L�B
K�B
J�B
K�B
LdB
LdB
NB
N�B
NpB
OBB
O�B
N�B
OB
P�B
R�B
S�B
TaB
TaB
TaB
T�B
TaB
TaB
TaB
U�B
V9B
VB
VB
W�B
XB
XB
XEB
XyB
X�B
YB
YB
XB
V9B
V�B
WsB
VmB
V�B
XyB
X�B
X�B
YKB
Y�B
Y�B
[#B
[#B
[WB
\)B
\�B
]/B
]�B
^5B
^�B
^jB
^�B
_pB
_pB
_B
_;B
_pB
_�B
aHB
aHB
a�B
b�B
b�B
dZB
d�B
d�B
d�B
d�B
d�B
e�B
f�B
ffB
g8B
g�B
hsB
h>B
hsB
hsB
h>B
h�B
iyB
iyB
i�B
jB
jKB
jB
j�B
j�B
j�B
jB
j�B
j�B
j�B
j�B
kB
k�B
l"B
lWB
lWB
lWB
m)B
l�B
m)B
m�B
m�B
m�B
m�B
m]B
n/B
m�B
m�B
m)B
m�B
m]B
m)B
m�B
m�B
n�B
ncB
n�B
o5B
o5B
oiB
o5B
oiB
o�B
o5B
o B
o B
n�B
ncB
ncB
ncB
n/B
m�B
m�B
n/B
n/B
n�B
o�B
o�B
o�B
p;B
p;B
p;B
p�B
qvB
q�B
rB
r�B
sB
sB
sMB
s�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
v`B
v`B
v�B
w�B
wfB
xB
w�B
xB
xB
w�B
w�B
w�B
x8B
yrB
zB
y�B
y>B
y�B
z�B
zDB
{�B
{�B
|�B
}�B
}�B
}�B
}�B
}VB
}"B
}�B
.B
.B
~�B
�B
�B
�B
� B
� B
�iB
��B
�B
��B
�oB
�;B
��B
��B
��B
�B
�GB
�{B
��B
��B
�B
��B
��B
��B
�MB
�B
�B
��B
��B
��B
�B
�SB
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
��B
��B
�1B
�fB
��B
��B
��B
�	B
�	B
�rB
�rB
�rB
�=B
�	B
�=B
�DB
�xB
�DB
��B
��B
��B
��B
��B
��B
�"B
��B
�(B
�(B
��B
�bB
�.B
�bB
��B
��B
�B
��B
�@B
�@B
�@B
�@B
��B
��B
�B
�B
��B
��B
�B
��B
��B
��B
�B
��B
�SB
�SB
�$B
��B
��B
��B
��B
�+B
�+B
�+B
�+B
��B
�+B
�_B
��B
��B
��B
��B
��B
�	B
�	B
��B
��B
��B
�qB
��B
�~B
�~B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�'B
��B
��B
��B
��B
��B
�-B
�bB
��B
�4B
��B
��B
�B
�@B
�@B
�tB
�@B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�LB
��B
��B
�RB
��B
��B
�$B
�XB
�XB
�$B
�XB
��B
�*B
��B
�_B
��B
��B
�0B
�eB
�eB
��B
��B
�B
�B
�6B
�6B
�6B
�6B
�kB
�kB
�kB
�kB
��B
��B
��B
��B
�B
�B
�=B
�=B
�qB
�CB
�wB
�wB
��B
�B
�IB
�}B
�}B
�}B
��B
��B
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
�!B
��B
��B
��B
��B
��B
��B
�-B
�-B
�aB
�-B
�-B
�-B
��B
��B
��B
��B
��B
�3B
�3B
�hB
�hB
��B
�B
��B
�B
��B
�9B
��B
�B
�B
�?B
��B
��B
�B
��B
�B
�B
��B
��B
�B
�LB
��B
��B
��B
��B
��B
��B
�$B
�XB
��B
��B
��B
��B
��B
��B
��B
��B
�*B
��B
�^B
�*B
�*B
��B
��B
��B
�dB
��B
�B
�B
�B
�6B
�6B
�6B
�jB
��B
��B
��B
��B
�qB
�qB
�qB
��B
��B
��B
�B
�BB
�BB
�BB
��B
��B
��B
��B
��B
��B
�B
�B
�HB
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�OB
z�B
{B
~(B
{JB
{B
{�B
|�B
zxB
|�B
|�B
zxB
{�B
{�B
zxB
|�B
|B
zDB
{�B
|�B
z�B
{JB
{B
{�B
{�B
|�B
z�B
{�B
|�B
{JB
|�B
}�B
{�B
{�B
}�B
|�B
{�B
|�B
}VB
{�B
{�B
}�B
|�B
{B
}�B
|�B
{JB
}"B
|�B
{B
}�B
}"B
{B
}�B
}"B
{JB
|�B
}VB
{JB
}"B
}VB
|B
{�B
}VB
|�B
{JB
}�B
|B
|�B
~]B
|B
|�B
~(B
{B
|�B
}�B
|�B
{B
}�B
}VB
{�B
|�B
}�B
|�B
{B
|�B
}�B
{�B
|B
}�B
|�B
{�B
}�B
}VB
{�B
}�B
|�B
{B
|PB
}�B
|�B
{�B
}"B
}"B
{�B
|�B
}�B
|B
|B
}�B
}�B
{�B
|�B
~(B
|PB
{�B
}"B
}�B
{B
|�B
}�B
|B
|B
~�B
|�B
{�B
}VB
~(B
}�B
{�B
|�B
~]B
}�B
|PB
}�B
~(B
|B
}�B
~�B
|PB
}�B
~�B
}VB
|PB
~]B
~]B
|�B
}VB
~�B
~(B
|�B
}VB
~�B
~�B
}�B
|�B
~�B
}�B
}"B
}"B
~�B
~(B
|�B
}�B
~]B
|�B
~(B
.B
~�B
|�B
~(B
.B
}VB
}VB
~�B
~]B
|�B
~�B
~�B
}�B
|�B
~]B
~�B
}VB
}VB
}�B
~�B
}�B
|�B
}�B
~�B
}�B
|�B
}�B
.B
}�B
}"B
~]B
cB
~(B
|�B
}�B
.B
~�B
}VB
}"B
~�B
.B
}"B
}"B
~(B
~�B
}"B
}�B
~�B
~�B
}"B
}�B
~�B
~]B
|�B
}�B
�B
}�B
|�B
~�B
cB
}�B
{JB
~�B
� B
~(B
}"B
~(B
cB
}�B
}"B
}�B
~�B
~�B
}"B
~]B
~�B
}"B
|�B
~�B
�4B
~�B
}�B
}�B
�B
�B
}�B
~(B
�B
~�B
}�B
~�B
�B
�B
}�B
.B
� B
}�B
}�B
cB
�B
}�B
~�B
� B
~�B
}"B
~�B
�iB
cB
}�B
}�B
�B
~�B
~�B
}�B
~]B
~�B
|�B
|B
~�B
~�B
|�B
|�B
�B
~�B
}�B
~�B
~�B
�B
}"B
}VB
~�B
~(B
}�B
}�B
�B
}�B
}"B
�B
~�B
~]B
|B
~�B
|�B
{�B
~�B
|�B
|PB
|�B
}�B
}�B
|�B
|�B
~]B
|�B
{B
|�B
~(B
|B
|B
}VB
}"B
{�B
|�B
~(B
}�B
{B
}"B
|�B
{B
z�B
{�B
}"B
{B
|B
|�B
{JB
z�B
|B
}"B
zB
z�B
|B
|PB
y>B
zB
|�B
{B
x�B
x�B
z�B
z�B
}"B
y>B
y	B
y	B
u�B
s�B
u%B
s�B
sMB
rGB
r�B
s�B
v`B
p�B
p�B
qvB
t�B
m�B
ncB
ncB
m�B
l�B
l�B
ncB
k�B
j�B
k�B
j�B
jKB
g8B
jB
p�B
o�B
e�B
`�B
bNB
b�B
`B
c B
^�B
YKB
[�B
Y�B
f2B
V�B
h�B
^5B
\�B
V�B
S�B
L�B
MB
R�B
N�B
N�B
M�B
I�B
TaB
M�B
J�B
D�B
GzB
HKB
F�B
EmB
GB
Q�B
T,B
W
B
iDB
YKB
d&B
k�B
h
B
a|B
\]B
\�B
`BB
e,B
]/B
a�B
d�B
iB
k�B
m]B
s�B
}"B
u�B
t�B
t�B
v�B
u%B
y	B
s�B
t�B
w2B
w2B
sMB
r|B
sMB
sMB
m)B
l�B
n/B
oiB
oiB
m�B
oiB
�YB
n�B
e,B
c�B
_pB
`�B
`B
[�B
hsB
p;B
jB
`BB
|B
{�B
g�B
p�B
x�B
jB
jKB
o�B
o�B
x8B
u�B
� B
�4B
��B
��B
��B
�MB
��B
��B
��B
�!B
��B
��B
�wB
��B
�#B
уB
֡B
�aB
�&B
ܒB
�B
�B
��B
��B
ĜB
�^B
�jB
�*B
��B
��B
��B
�$B
�XB
��B
�B
�[B
�B
�FB
��B
�?B
�B
�B
�B
�RB
�B
��B
�FB
��B
��B
��B
��B
�B
�LB
�LB
��B
��B
�XB
��B
��B
��B
��B
�XB
�B
��B
��B
�*B
��B
�*B
��B
��B
��B
��B
�<B
��B
��B
�^B
��B
��B
��B
��B
�jB
��B
�wB
��B
�OB
��B
�[B
� B
�-B
��B
ɆB
�XB
�#B
��B
ϫB
�B
רB
��B
�B
ܒB
�BB�B�BB�B"BCBLdBBqB�B.B�B�B�BB�BeBkB�B)�B%�B$�BD�BGB7�B>�B@�BV9B]�B^5BjBk�Bn�BwfBt�BtTBsMBt�BxBx�Bu�BtTBu�Bu�BuZBt�Bu�BqABxlBr�B�B��B��B��B�EB��B��BB{B+B�B1B�B=B�B!-B%�B'RB)�B+6B)*B*�B+�B-B1'B8�B8�B,�B=<B~]B@�B>B?�BB�BA�B9XB7�B8RB8RB:^B;0B:�B<B;�B=B=qB<6B:�B:�B;dB=�B?HBC�BB�BHKBG�BN�BX�B\]B[#B[WBZ�B]/B]/B[�BX�B^5BbNBZ�B`�B[�Bm�BbNB\]BX�B\)B^�BYKBJ�BJ�BGzBK�BM6BS�BJ#BOBBA�B=�B;�BAUB/�B+6B2�B0�B3�B4B5�BffBG�B<BB�B9�B;�BZ�B8BOB=BqB�BkB�BCB�B�B�B�B$BB�B�B�B	BqBBB  B;BB�B�cB�KB�DB�B�8B�B�B�B�8B��B�,B�mB�B�B�B�B�B�/B�B�B��B�#B��B��B�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2020120920403320201209204033IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401120210224164011QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401120210224164011QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194020210429101940IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                