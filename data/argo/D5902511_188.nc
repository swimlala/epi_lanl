CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-15T04:39:00Z creation; 2022-02-04T23:30:05Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  c\   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �X   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ά   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � `P   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �X   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �X   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211115043900  20220204223519  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_188                 6810_008521_188                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٢�`�@٢�`�11  @٢ƔFs�@٢ƔFs�@0%x��@0%x���dO���&�dO���&11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @B�\@�  @�  @�G�@޸R@��RA  A ��A+�A@��AaG�A���A��A�\)A��A��A�  A�  A�Q�B Q�B  B�
B(�B (�B(  B/�
B7�
B@  BH(�BP(�BX(�B`  Bg�
Bp(�BxQ�B�  B�{B�{B�  B�  B�  B�{B�(�B�{B�{B�{B��B��B��
B�  B�{B�  B�{B��B��B�{B�(�B�{B�  B��B��B�  B�  B�  B�{B�{B�  B��
C�HC��C
=C{C
  C��C��C  C
=C�C�C
=C  C��C  C   C"  C#��C&
=C({C*
=C,
=C-��C0  C1��C3�C6  C8
=C:
=C<  C>
=C@
=CA��CD  CF
=CH
=CJ
=CL
=CN  CP  CR  CS��CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch{Ci��Cl{Cn
=Cp
=Cr
=Ct
=Cv  Cx  Cy��C|
=C~
=C��C�  C�C���C���C�  C�  C�
=C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C���C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�
=C�  C�C�  C���C���C���C���C���C�  C�C���C���C���C�  C�  C�  C�  C�
=C�C�  C�  C�C���C���C���C���C���C���C���C���C�  C�  C���C���C���C���C�  C���C���C�  C�  C�C�  C���C�C�C���C���C���C�  C�C�  C�  C�C�C���C�  C�C���C�  C�C�  C�C�  C�  C�C�  C���C���C�  C�C�  C���C���C�  C���C���C�  C���C�  C�C�  C�
=C�
=C�  C���C�C�  C���C�C�  C���C�  C�  D �D � D  D��D  D��D  D� D  D��D  D� D  D}qD��Dz�D�qDz�D��D	}qD
  D
}qD  D�DD��D  D}qD��Dz�D�qD��D  D� D  Dz�D�qD}qD  D�D  D� D  D� D  D� D�D�D�qD}qD�D��D�qD}qD�qD� D  D��D  D� D�D� D  D��D �D ��D!  D!� D"  D"��D"�qD#z�D$  D$� D%  D%}qD%�qD&� D'�D'� D(  D(}qD)  D)� D*  D*��D*�qD+� D,  D,� D-  D-��D.  D.}qD/  D/� D/�qD0� D1�D1��D2D2� D2�qD3� D4D4��D5  D5� D5�qD6z�D6��D7}qD8  D8}qD9  D9}qD9��D:}qD;  D;��D<�D<� D=�D=�D>  D>� D?�D?��D?�qD@}qDA  DA� DA�qDBz�DC  DC��DD  DD� DE  DE� DF  DF}qDF�qDG� DH  DH}qDI  DI��DJ  DJ� DK  DK� DK�qDL��DMDM� DM�qDN}qDO  DO� DO�qDP}qDQ  DQ}qDR  DR��DS�DS��DT�DT��DU�DU� DU��DV}qDV�qDW� DX  DX��DY  DY� DZ  DZ� D[  D[}qD[�qD\��D\�qD]� D^�D^�D_�D_��D`  D`� Da�Da�Db�Db�Dc  Dcz�Dc�qDd�De�De��Df�Df��DgDg��Dh�Dh� Di  Di� Di�qDj� Dk�Dk}qDl  Dl�Dm  Dm}qDn  Dn� Do�Do��Do��Dp� Dq�Dq��Dr  Dr��Ds  Ds}qDt  Dt}qDu  Du� Du��Dv� DwDw}qDw�qDx� Dx�qDyz�Dy��Dz}qD{  D{� D|  D|}qD|��D}� D~D~��D�D� D��D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D��HD��HD�  D�>�D�� D���D�  D�@ D�~�D��HD�  D�AHD��HD�� D���D�>�D��HD�� D��qD�>�D�~�D���D�HD�AHD��HD�� D�  D�>�D�~�D�� D��D�@ D�~�D�� D���D�AHD���D�� D�  D�B�D���D��HD�  D�@ D�� D���D���D�AHD�� D�� D�  D�AHD��HD��HD��D�>�D�~�D���D���D�AHD��HD���D�  D�AHD��HD�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D��qD�@ D��HD�� D�  D�AHD�� D��HD�HD�AHD��HD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D�D��D�@ D�� D���D���D�=qD�~�D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�HD�@ D��HD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�D�  D�>�D�� D��HD�HD�AHD�~�D���D���D�=qD�~�D��HD�  D�@ D���D��HD�HD�@ D�}qD��qD�  D�AHD��HD��HD�HD�AHD��HD��HD���D�>�D�� D���D���D�AHD��HD�� D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�~�D���D���D�@ D�� D�� D�  D�AHD�� D���D��D�@ D�}qD�� D�  D�=qD�~�D��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD���D�AHD��HD�� D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�@ D��HD��HD���D�>�D�~�D���D�HD�@ D�~�D��HD��D�AHDÁHD�� D���D�@ D�~�DĽqD���D�@ DŁHD��HD��qD�@ DƁHD�� D���D�>�D�~�D��HD�HD�>�DȀ D�� D�  D�>�D�~�Dɾ�D�  D�@ Dʀ Dʾ�D�  D�AHD�~�D��HD�HD�@ D̀ D�� D�HD�AHD�~�D�� D�  D�@ D�~�D�� D���D�>�Dπ DϾ�D�  D�>�DЀ D��HD���D�>�Dр DѾ�D�  D�>�D�}qDҾ�D��D�AHDӀ D�� D�HD�B�DԂ�D�� D���D�@ DՁHD�D�  D�>�D�~�D־�D�  D�@ D׀ D��HD���D�>�D�~�D�� D�  D�@ DفHD��HD�  D�@ Dڀ Dھ�D��qD�>�Dۀ D�� D�HD�AHD�~�D�� D��D�AHD݁HDݾ�D���D�=qD�|)D޾�D���D�>�D߀ D߾�D���D�>�D�� D��HD�  D�>�D� D�� D�  D�@ D� D��HD�HD�@ D� D�� D���D�>�D� D�� D�  D�AHD傏D��HD���D�=qD�}qD澸D�  D�B�D炏D��HD�  D�@ D�~�D�qD���D�@ D邏D��HD�  D�@ D� D��HD�HD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�HD���D���D�AHD� DD��D�B�D� DﾸD�HD�B�D���D��HD���D�>�D� D�� D�HD�>�D�~�D�� D�  D�>�D�HD�>��
>�Q�?.{?��?��
?\?��@�@��@5@B�\@Q�@h��@xQ�@�ff@��@�
=@��\@��@���@��H@��@˅@��@�p�@�\@�@�@��HA�\A�A
�HA\)AA��Ap�A#33A'�A*�HA0��A5�A9��A>�RAA�AG�AL(�AO\)AUAY��A\��Ac33Ag�Ak�Ap  AvffAz=qA~{A��A�(�A�{A�  A��HA�p�A�
=A�G�A�(�A�A�  A��HA�(�A��RA���A��A�p�A�Q�A��A�(�A�
=A���A�33A�A�\)A�=qA�(�A�{A�G�A�33A�p�A�Q�A��A�(�A�
=A���A��HA�A�\)Aٙ�A�z�A�{A���A�A��A�\)A��A�(�A�A�Q�A�33A��A�
=A���A��
A�{B   B�BffB33Bz�BBffB�B��B	B
�RB(�B��B{B\)BQ�Bp�B�HB�B��B=qB33B  Bp�BffB33B��B�B�RB�
B!G�B"=qB#
=B$z�B%��B&ffB'�B(��B*{B*�RB,  B-G�B.=qB/
=B0z�B1��B2ffB333B4��B5B6�\B7�
B9�B9B;\)B<Q�B=G�B>�RB?�B@��BA�BC\)BD(�BEG�BF�\BG�BHz�BIBK33BK�
BL��BNffBO\)BPQ�BQ��BS
=BS�BT��BV=qBW\)BX(�BYp�BZ�HB[�B\��B^=qB^�HB`z�Bap�BbffBc�Be�Be�Bf�HBhQ�Bip�BjffBk\)Bl��Bn{Bn�RBp  Bqp�Br�\Bs\)Bt��Bv{Bv�HBw�
ByG�Bz=qB{33B|z�B}B~�RB�B�z�B���B�p�B�(�B���B��B��B�z�B���B�\)B��B���B�
=B���B�Q�B���B�33B�  B��\B��HB���B�(�B���B�\)B�B�Q�B��HB��B�{B��RB�\)B��
B�ffB��B���B�  B��RB�\)B�B�Q�B��B���B�  B��RB�G�B�B�=qB���B���B��B��\B�33B��B�(�B��HB�p�B��
B�ffB��B���B�{B��RB�\)B�B�Q�B���B���B��B���B�G�B���B�(�B��HB�p�B�B��\B�
=B�p�B�{B��RB��B��B�z�B���B�\)B�{B��RB��B��
B�Q�B���B�p�B�{B�ffB�
=B�B�=qB���B�\)B��B�=qB��HB��B��B�ffB��B���B�{B��RB��B��B�(�B��RB��B��B�Q�B��HB�G�B��
B\B���B�\)B�{BĸRB��Bř�B�(�B��HB�G�BǮB�z�B���B�\)B�{B�ffB�
=B�B�Q�B̸RB�33B��
B�z�B�
=B�\)B�{BиRB�G�BѮB�=qB���B�p�B��Bԣ�B�G�Bՙ�B�(�B��HBׅB��
B؏\B�33BٮB�{Bڣ�B�G�B�  B�Q�B�
=BݮB�(�Bޏ\B�33B��
B�=qB���B�p�B�{B�\B�
=B㙚B�=qB���B�33B��
B�z�B��HB�\)B�  B��B�
=B�B�=qB���B�33B�B�Q�B���B홚B�{B�z�B�G�B�  B�z�B���B�B�Q�B���B�\)B�  B���B�G�B�B�z�B�33B��B�ffB��B��B�(�B��HB�p�B��
B�z�B�33B��
B�=qB���B��C {C Q�C �RC  C33Cz�C�
C(�C\)C�C
=CQ�C�\C�
C(�C�CC
=C\)CC��C=qC��C�
C(�C�C��C  C\)C�RC	  C	=qC	��C
  C
=qC
�C
�HC33Cp�CC(�Cz�C�RC{Cp�CC
=CQ�C�RC
=CG�C��C  C\)C��C�CG�C�C�C33C��C�C(�C�\C�HC�Cz�C�
C�CQ�C�RC
=C=qC�\C��CG�C�C��C=qC��C�
C(�Cz�C�HC(�Cp�C�
C(�Cp�CC(�Cz�CC
=Cp�C��C
=C\)CC{C\)C��C   C ffC ��C �C!G�C!�RC"
=C"Q�C"��C#{C#ffC#�RC$  C$ffC$C%
=C%\)C%�C&{C&ffC&�C'  C'\)C'�RC(�C(ffC(�RC)
=C)ffC)�
C*33C*z�C*��C+33C+�\C+�HC,(�C,�\C,��C-=qC-�C-�
C.33C.��C.�C/(�C/p�C/��C0(�C0�C0�
C1(�C1p�C1�RC2  C2\)C2�C3
=C3G�C3�\C3�C4G�C4��C4�C5�C5�C5�
C633C6�C6C7
=C7\)C7C8�C8ffC8�C9  C9\)C9C:{C:Q�C:��C;{C;ffC;�RC<  C<Q�C<��C<��C=Q�C=�C>
=C>Q�C>��C>�HC?G�C?��C?��C@Q�C@��C@�CA=qCA�CA�
CB(�CB�\CB�HCC33CC�CC��CD�CDz�CD��CE(�CE�CE�
CF(�CFffCF�RCG  CG\)CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              ?�=q@   @B�\@�  @�  @�G�@޸R@��RA  A ��A+�A@��AaG�A���A��A�\)A��A��A�  A�  A�Q�B Q�B  B�
B(�B (�B(  B/�
B7�
B@  BH(�BP(�BX(�B`  Bg�
Bp(�BxQ�B�  B�{B�{B�  B�  B�  B�{B�(�B�{B�{B�{B��B��B��
B�  B�{B�  B�{B��B��B�{B�(�B�{B�  B��B��B�  B�  B�  B�{B�{B�  B��
C�HC��C
=C{C
  C��C��C  C
=C�C�C
=C  C��C  C   C"  C#��C&
=C({C*
=C,
=C-��C0  C1��C3�C6  C8
=C:
=C<  C>
=C@
=CA��CD  CF
=CH
=CJ
=CL
=CN  CP  CR  CS��CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch{Ci��Cl{Cn
=Cp
=Cr
=Ct
=Cv  Cx  Cy��C|
=C~
=C��C�  C�C���C���C�  C�  C�
=C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C���C�C�C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�C�
=C�  C�C�  C���C���C���C���C���C�  C�C���C���C���C�  C�  C�  C�  C�
=C�C�  C�  C�C���C���C���C���C���C���C���C���C�  C�  C���C���C���C���C�  C���C���C�  C�  C�C�  C���C�C�C���C���C���C�  C�C�  C�  C�C�C���C�  C�C���C�  C�C�  C�C�  C�  C�C�  C���C���C�  C�C�  C���C���C�  C���C���C�  C���C�  C�C�  C�
=C�
=C�  C���C�C�  C���C�C�  C���C�  C�  D �D � D  D��D  D��D  D� D  D��D  D� D  D}qD��Dz�D�qDz�D��D	}qD
  D
}qD  D�DD��D  D}qD��Dz�D�qD��D  D� D  Dz�D�qD}qD  D�D  D� D  D� D  D� D�D�D�qD}qD�D��D�qD}qD�qD� D  D��D  D� D�D� D  D��D �D ��D!  D!� D"  D"��D"�qD#z�D$  D$� D%  D%}qD%�qD&� D'�D'� D(  D(}qD)  D)� D*  D*��D*�qD+� D,  D,� D-  D-��D.  D.}qD/  D/� D/�qD0� D1�D1��D2D2� D2�qD3� D4D4��D5  D5� D5�qD6z�D6��D7}qD8  D8}qD9  D9}qD9��D:}qD;  D;��D<�D<� D=�D=�D>  D>� D?�D?��D?�qD@}qDA  DA� DA�qDBz�DC  DC��DD  DD� DE  DE� DF  DF}qDF�qDG� DH  DH}qDI  DI��DJ  DJ� DK  DK� DK�qDL��DMDM� DM�qDN}qDO  DO� DO�qDP}qDQ  DQ}qDR  DR��DS�DS��DT�DT��DU�DU� DU��DV}qDV�qDW� DX  DX��DY  DY� DZ  DZ� D[  D[}qD[�qD\��D\�qD]� D^�D^�D_�D_��D`  D`� Da�Da�Db�Db�Dc  Dcz�Dc�qDd�De�De��Df�Df��DgDg��Dh�Dh� Di  Di� Di�qDj� Dk�Dk}qDl  Dl�Dm  Dm}qDn  Dn� Do�Do��Do��Dp� Dq�Dq��Dr  Dr��Ds  Ds}qDt  Dt}qDu  Du� Du��Dv� DwDw}qDw�qDx� Dx�qDyz�Dy��Dz}qD{  D{� D|  D|}qD|��D}� D~D~��D�D� D��D�>�D�� D�� D�  D�@ D�� D�� D�  D�AHD�~�D���D���D�@ D��HD��HD�  D�>�D�� D���D�  D�@ D�~�D��HD�  D�AHD��HD�� D���D�>�D��HD�� D��qD�>�D�~�D���D�HD�AHD��HD�� D�  D�>�D�~�D�� D��D�@ D�~�D�� D���D�AHD���D�� D�  D�B�D���D��HD�  D�@ D�� D���D���D�AHD�� D�� D�  D�AHD��HD��HD��D�>�D�~�D���D���D�AHD��HD���D�  D�AHD��HD�� D�  D�>�D�}qD�� D�  D�>�D��HD�� D��qD�@ D��HD�� D�  D�AHD�� D��HD�HD�AHD��HD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D�D��D�@ D�� D���D���D�=qD�~�D���D�  D�>�D�~�D�� D�  D�AHD��HD��HD�HD�@ D��HD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD�D�  D�>�D�� D��HD�HD�AHD�~�D���D���D�=qD�~�D��HD�  D�@ D���D��HD�HD�@ D�}qD��qD�  D�AHD��HD��HD�HD�AHD��HD��HD���D�>�D�� D���D���D�AHD��HD�� D�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�>�D�� D�� D�  D�AHD�~�D���D���D�@ D�� D�� D�  D�AHD�� D���D��D�@ D�}qD�� D�  D�=qD�~�D��qD���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D��qD���D�AHD��HD�� D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�@ D��HD��HD���D�>�D�~�D���D�HD�@ D�~�D��HD��D�AHDÁHD�� D���D�@ D�~�DĽqD���D�@ DŁHD��HD��qD�@ DƁHD�� D���D�>�D�~�D��HD�HD�>�DȀ D�� D�  D�>�D�~�Dɾ�D�  D�@ Dʀ Dʾ�D�  D�AHD�~�D��HD�HD�@ D̀ D�� D�HD�AHD�~�D�� D�  D�@ D�~�D�� D���D�>�Dπ DϾ�D�  D�>�DЀ D��HD���D�>�Dр DѾ�D�  D�>�D�}qDҾ�D��D�AHDӀ D�� D�HD�B�DԂ�D�� D���D�@ DՁHD�D�  D�>�D�~�D־�D�  D�@ D׀ D��HD���D�>�D�~�D�� D�  D�@ DفHD��HD�  D�@ Dڀ Dھ�D��qD�>�Dۀ D�� D�HD�AHD�~�D�� D��D�AHD݁HDݾ�D���D�=qD�|)D޾�D���D�>�D߀ D߾�D���D�>�D�� D��HD�  D�>�D� D�� D�  D�@ D� D��HD�HD�@ D� D�� D���D�>�D� D�� D�  D�AHD傏D��HD���D�=qD�}qD澸D�  D�B�D炏D��HD�  D�@ D�~�D�qD���D�@ D邏D��HD�  D�@ D� D��HD�HD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�AHD�HD���D���D�AHD� DD��D�B�D� DﾸD�HD�B�D���D��HD���D�>�D� D�� D�HD�>�D�~�D�� D�  D�>�D�HG�O�>��
>�Q�?.{?��?��
?\?��@�@��@5@B�\@Q�@h��@xQ�@�ff@��@�
=@��\@��@���@��H@��@˅@��@�p�@�\@�@�@��HA�\A�A
�HA\)AA��Ap�A#33A'�A*�HA0��A5�A9��A>�RAA�AG�AL(�AO\)AUAY��A\��Ac33Ag�Ak�Ap  AvffAz=qA~{A��A�(�A�{A�  A��HA�p�A�
=A�G�A�(�A�A�  A��HA�(�A��RA���A��A�p�A�Q�A��A�(�A�
=A���A�33A�A�\)A�=qA�(�A�{A�G�A�33A�p�A�Q�A��A�(�A�
=A���A��HA�A�\)Aٙ�A�z�A�{A���A�A��A�\)A��A�(�A�A�Q�A�33A��A�
=A���A��
A�{B   B�BffB33Bz�BBffB�B��B	B
�RB(�B��B{B\)BQ�Bp�B�HB�B��B=qB33B  Bp�BffB33B��B�B�RB�
B!G�B"=qB#
=B$z�B%��B&ffB'�B(��B*{B*�RB,  B-G�B.=qB/
=B0z�B1��B2ffB333B4��B5B6�\B7�
B9�B9B;\)B<Q�B=G�B>�RB?�B@��BA�BC\)BD(�BEG�BF�\BG�BHz�BIBK33BK�
BL��BNffBO\)BPQ�BQ��BS
=BS�BT��BV=qBW\)BX(�BYp�BZ�HB[�B\��B^=qB^�HB`z�Bap�BbffBc�Be�Be�Bf�HBhQ�Bip�BjffBk\)Bl��Bn{Bn�RBp  Bqp�Br�\Bs\)Bt��Bv{Bv�HBw�
ByG�Bz=qB{33B|z�B}B~�RB�B�z�B���B�p�B�(�B���B��B��B�z�B���B�\)B��B���B�
=B���B�Q�B���B�33B�  B��\B��HB���B�(�B���B�\)B�B�Q�B��HB��B�{B��RB�\)B��
B�ffB��B���B�  B��RB�\)B�B�Q�B��B���B�  B��RB�G�B�B�=qB���B���B��B��\B�33B��B�(�B��HB�p�B��
B�ffB��B���B�{B��RB�\)B�B�Q�B���B���B��B���B�G�B���B�(�B��HB�p�B�B��\B�
=B�p�B�{B��RB��B��B�z�B���B�\)B�{B��RB��B��
B�Q�B���B�p�B�{B�ffB�
=B�B�=qB���B�\)B��B�=qB��HB��B��B�ffB��B���B�{B��RB��B��B�(�B��RB��B��B�Q�B��HB�G�B��
B\B���B�\)B�{BĸRB��Bř�B�(�B��HB�G�BǮB�z�B���B�\)B�{B�ffB�
=B�B�Q�B̸RB�33B��
B�z�B�
=B�\)B�{BиRB�G�BѮB�=qB���B�p�B��Bԣ�B�G�Bՙ�B�(�B��HBׅB��
B؏\B�33BٮB�{Bڣ�B�G�B�  B�Q�B�
=BݮB�(�Bޏ\B�33B��
B�=qB���B�p�B�{B�\B�
=B㙚B�=qB���B�33B��
B�z�B��HB�\)B�  B��B�
=B�B�=qB���B�33B�B�Q�B���B홚B�{B�z�B�G�B�  B�z�B���B�B�Q�B���B�\)B�  B���B�G�B�B�z�B�33B��B�ffB��B��B�(�B��HB�p�B��
B�z�B�33B��
B�=qB���B��C {C Q�C �RC  C33Cz�C�
C(�C\)C�C
=CQ�C�\C�
C(�C�CC
=C\)CC��C=qC��C�
C(�C�C��C  C\)C�RC	  C	=qC	��C
  C
=qC
�C
�HC33Cp�CC(�Cz�C�RC{Cp�CC
=CQ�C�RC
=CG�C��C  C\)C��C�CG�C�C�C33C��C�C(�C�\C�HC�Cz�C�
C�CQ�C�RC
=C=qC�\C��CG�C�C��C=qC��C�
C(�Cz�C�HC(�Cp�C�
C(�Cp�CC(�Cz�CC
=Cp�C��C
=C\)CC{C\)C��C   C ffC ��C �C!G�C!�RC"
=C"Q�C"��C#{C#ffC#�RC$  C$ffC$C%
=C%\)C%�C&{C&ffC&�C'  C'\)C'�RC(�C(ffC(�RC)
=C)ffC)�
C*33C*z�C*��C+33C+�\C+�HC,(�C,�\C,��C-=qC-�C-�
C.33C.��C.�C/(�C/p�C/��C0(�C0�C0�
C1(�C1p�C1�RC2  C2\)C2�C3
=C3G�C3�\C3�C4G�C4��C4�C5�C5�C5�
C633C6�C6C7
=C7\)C7C8�C8ffC8�C9  C9\)C9C:{C:Q�C:��C;{C;ffC;�RC<  C<Q�C<��C<��C=Q�C=�C>
=C>Q�C>��C>�HC?G�C?��C?��C@Q�C@��C@�CA=qCA�CA�
CB(�CB�\CB�HCC33CC�CC��CD�CDz�CD��CE(�CE�CE�
CF(�CFffCF�RCG  CG\)CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
AԸRAԲ-AԮA԰!Aԩ�Aԟ�Aԝ�A�bNA��AӰ!Aө�Aӟ�Aӛ�Aӗ�AӑhAӏ\AӍPAӉ7AӅAӁAӁAӁAӁA�~�A�~�A�|�A�z�A�z�A�v�A�p�A�l�A�l�A�l�A�l�A�jA�l�A�jA�jA�hsA�jA�jA�jA�l�A�l�A�l�A�l�A�n�A�n�A�n�A�n�A�l�A�jA�"�A�A���A� �ÃA˾wA�jA�ĜA�r�A�E�A���A���A���A���A���A���A�1'A�-A���A��PA��`A�+A�bNA��RA�E�A��RA�ZA�VA�C�A��HA��`A�&�A��A��A�VA��9A�ƨA���A��hA�^5A���A��wA��/A��A�bA���A�=qA�=qA��A��
A�bA�r�A�\)A���A�x�A�hsA�hsA��AzjAwXAt�ArbApȴAo�PAoAn�HAm�hAk��Ajr�Ai�Ah1'AgS�Af~�Ae�TAeVAdE�AcK�Aa7LA]t�A[`BAY�AW+AT�HAR��AP�\AO
=ALQ�AJ �AF��AC�-A@�`A>��A=+A;�A9A7x�A6Q�A5��A3�A2Q�A0��A/l�A,�RA*�A)�A(�yA&n�A%�7A$�9A$VA${A#�;A"�jA ��A��AVA\)A�!A�Az�AAAĜA�A�hAr�A�A%A1A7LA�A$�A��A�AbNA��A�A(�AS�A
�\A
(�A	|�A��A��A33AG�A?}AffA"�A��AI�AAC�A��@�"�@��@�@���A A I�AoA �u@��m@�V@���@��@���@��@��@�dZ@��9@�1'@�
=@��#@��@�-@��-@�1@ꟾ@��@�E�@�X@�D@旍@�hs@�S�@��@�o@�R@�O�@�h@��#@�^@��`@��;@ޏ\@�ȴ@١�@١�@�G�@���@�%@ش9@�1'@ם�@֟�@պ^@�X@ԋD@��H@��@�p�@���@Ѓ@Л�@Гu@Ѓ@�9X@��m@�+@Ώ\@�M�@͑h@���@�1'@ʟ�@ɩ�@ə�@ɑh@Ɂ@�O�@�V@���@���@ȼj@�j@��m@Ǖ�@�
=@Ƨ�@�~�@�{@�@ź^@�hs@��@���@�ƨ@Õ�@��@�M�@��#@��^@��h@�7L@��@���@�j@��@���@��F@���@��@�;d@�+@�S�@��@��y@��H@��@���@�^5@���@�G�@���@��@�z�@��@��@���@��@�\)@�+@��y@�~�@�5?@��T@���@���@�O�@�V@���@�b@��;@��F@�l�@�33@�
=@��H@�ȴ@��!@�~�@�^5@�=q@�{@���@�p�@���@��@�I�@�b@��@��@���@���@���@��@���@�=q@�{@���@��@���@��D@�z�@�Z@�1@�dZ@��!@�v�@�ff@�V@�E�@���@���@��D@�A�@��m@�S�@���@���@���@�~�@�M�@��@���@���@���@��7@��@��/@�z�@�\)@�@���@���@�n�@�=q@��@��7@�?}@��@��@�t�@�;d@�
=@���@���@�~�@�@��@�G�@��@��`@��@�Z@�l�@�+@�o@���@��!@�~�@�n�@�M�@��@��@��@�1'@��w@�dZ@�o@�ȴ@���@�~�@�{@��-@�x�@�7L@�%@�Ĝ@��D@�r�@�A�@��@��w@���@�S�@�ȴ@���@�^5@�-@�J@��@���@���@�hs@�&�@��@��@��@�1'@�1@���@�S�@���@�ff@��@��-@�X@���@��/@��@�A�@���@�
=@��+@��@�@�p�@�O�@���@��u@��D@�z�@�j@�bN@�(�@�1@��m@�ƨ@���@�l�@�"�@��H@���@�n�@�V@��@��h@�hs@�X@�?}@���@��@���@�r�@�bN@�A�@�b@���@�"�@��@��!@�@�@��-@��h@��@��@��@��@��@��@��@��@�X@��@��D@�r�@�bN@�I�@�A�@�(�@�1@��w@���@��@�;d@�"�@���@�n�@�=q@�@��#@��@���@�Ĝ@��@�9X@�;@+@~�R@}��@}�@|�@{��@z�@z�!@z�\@z^5@z�@yX@xbN@w��@wl�@w
=@vv�@v@u`B@uO�@u�@t�@s�
@s��@s��@s"�@r��@rn�@r^5@r=q@r�@q��@q&�@pĜ@p  @o�P@o\)@o+@nȴ@nȴ@n�R@n�+@n$�@m@m��@m�@mp�@mO�@m�@l��@l9X@k�F@k�@kt�@kC�@j��@j=q@j�@i��@i�#@i��@ix�@h�`@hQ�@h1'@h1'@f�y@e�@e�-@e/@d�j@d9X@cƨ@cC�@b�@b�!@a�@aG�@`�`@`�u@`  @_�P@^�@^ff@^{@]��@\�@\1@[ƨ@[o@Z-@Yhs@Y7L@Y�@X��@X��@Xr�@W\)@W
=@W
=@Vv�@U@U/@T�j@Tz�@S��@Sƨ@S33@R~�@R=q@Q�@Q��@Q�@P�9@P1'@Pb@O�;@O�P@O;d@O
=@N��@N�@M@MO�@L��@L��@LZ@K��@Kƨ@K��@KC�@J�H@J�!@J�\@J�\@J~�@Jn�@J=q@JJ@H�`@H �@G�P@GK�@G;d@G;d@G+@F��@F��@FV@E�T@E�@EO�@E�@EV@D�/@D�@C�@CS�@B�H@B~�@A��@Ax�@A��@Ahs@AX@@�`@@1'@@  @?�@>��@>v�@=�@=@=��@=O�@<�@<�j@<�D@<�D@<z�@<Z@<(�@<1@;dZ@;o@:�H@:��@:��@:��@:�!@:~�@9��@9&�@8�`@8��@8Q�@81'@8 �@7�@7�w@7��@7|�@7;d@6��@6��@65?@5�T@5@5`B@5?}@4�j@4�@4��@4I�@3�
@3t�@3"�@2�H@2�H@2�H@2��@2��@2�\@2n�@2=q@2J@1��@1��@1�7@1hs@1x�@1�7@1&�@0�`@0�9@0r�@0A�@/�;@/l�@/l�@/\)@/\)@/�@.�y@.v�@.E�@.@-��@-��@-�@-?}@,��@,Z@,9X@,(�@,�@+ƨ@+�F@+�F@+�F@+��@+dZ@+S�@+33@*�!@)��@)�^@)hs@(�`@(r�@(1'@( �@'�@'|�@';d@&�@&�R@&��@&V@&{@&@&@%@%?}@$�@$�D@$Z@$9X@$(�@#��@#��@#S�@"�H@"�!@"M�@"�@"�@!��@!��@!x�@!hs@!X@!G�@!�@ ��@ �`@ ��@ ��@ r�@ 1'@ b@�w@l�@;d@�@ȴ@�R@��@v�@$�@@�h@�@�@`B@O�@?}@�@�@�@�D@z�@z�@j@j@Z@I�@9X@�m@��@�@33@"�@�@~�@=q@�@��@�7@x�@&�@��@�@A�@b@��@��@;d@
=@�y@�R@��@��@��@v�@V@�@�h@p�@O�@?}@?}@/@�@��@�@�j@��@�D@Z@(�@(�@(�@�@�m@ƨ@t�@o@@��@��@�!@��@�\@n�@J@�@�@�#@�#@��@��@hs@&�@��@��@�`@�9@�@1'@b@�@��@�@|�@|�@\)@+A��/A��;A���A��;A��#A�ĜAԺ^AԮAԾwAԲ-A԰!A԰!AԲ-AԬAԮAԮAԩ�AԲ-AԴ9AԶFAԬAԮAԥ�Aԣ�Aԡ�Aԛ�Aԟ�Aԟ�AԓuAԡ�Aԡ�Aԏ\A�dZA�=qA�K�A��A�AӼjAө�AӾwAӮAӧ�Aө�Aӣ�Aӧ�Aӥ�Aӡ�Aӣ�Aӡ�Aӝ�Aӟ�Aӟ�Aӛ�Aӛ�Aӟ�Aӛ�Aә�Aӟ�Aӛ�Aӗ�Aӗ�Aә�Aӛ�Aӕ�Aӗ�Aӗ�AӓuAӕ�Aӕ�Aӏ\AӑhAӓuAӏ\AӍPAӑhAӍPAӍPAӑhAӋDAӏ\AӍPAӋDAӑhAӍPAӉ7Aӏ\AӍPAӋDAӏ\AӋDAӉ7AӋDAӉ7AӅAӉ7AӅAӅAӇ+AӃAӅAӉ7AӃAӃAӇ+AӅA�|�A�~�AӁA�~�A�|�A�~�AӅAӁA�~�AӃAӃA�~�AӃAӃA�~�A�~�AӃA�|�AӁAӃAӁA�~�AӅA�~�A�~�AӁA�~�A�|�AӁAӁA�|�AӁA�~�A�z�A�~�AӁA�|�A�|�AӁA�~�A�z�AӁA�~�A�z�A�|�AӁA�~�A�x�A�z�A�|�A�x�A�x�A�z�A�|�A�x�A�x�A�|�A�|�A�v�A�x�A�z�A�v�A�z�A�x�A�t�A�x�A�v�A�t�A�r�A�v�A�p�A�r�A�v�A�t�A�l�A�n�A�r�A�n�A�jA�p�A�l�A�jA�l�A�n�A�jA�jA�p�A�n�A�jA�l�A�n�A�hsA�l�A�l�A�hsA�n�A�l�A�hsA�jA�n�A�jA�hsA�n�A�n�A�jA�jA�n�A�n�A�jA�hsA�l�A�l�A�hsA�l�A�n�A�l�A�jA�n�A�l�A�hsA�jA�n�A�jA�hsA�n�A�jA�hsA�l�A�n�A�hsA�jA�l�A�l�A�ffA�hsA�l�A�hsA�ffA�jA�hsA�ffA�jA�jA�ffA�l�A�hsA�hsA�l�A�hsA�ffA�hsA�l�A�hsA�jA�l�A�hsA�jA�n�A�l�A�hsA�l�A�l�A�hsA�jA�n�A�l�A�jA�l�A�n�A�l�A�jA�n�A�n�A�jA�n�A�n�A�jA�jA�n�A�n�A�jA�jA�n�A�l�A�jA�l�A�p�A�l�A�jA�n�A�n�A�jA�n�A�p�A�l�A�l�A�p�A�n�A�l�A�p�A�p�A�l�A�n�A�r�A�n�A�n�A�r�A�n�A�jA�p�A�n�A�jA�p�A�n�A�jA�n�A�p�A�l�A�n�A�p�A�n�A�jA�n�A�n�A�jA�jA�p�A�l�A�jA�l�A�jA�hsA�l�A�jA�ffA�hsA�jA�^5A�O�A�"�A��A�ȴAҩ�A҃A�"�A���AѬA�p�A�`BA�K�A�C�A�A�A�/A�A�AЕ�A�{Aϧ�A�1A�ƨA�~�A͗�A�\)A�-A�A��yA���A̩�A�~�A�O�A�33A�&�A��A�VA�%A���A��A˟�A��A�I�A��A���A�l�A�{A���A��;A���A�ƨA�ƨA�ȴA���A���AȾwAȝ�A�"�A�`BA�33A�"�A��A�
=AƼjAƉ7A�n�A�\)A�=qA�{A��A��A��mAŰ!A��A�|�A�5?A�bA���A���A×�A�`BA�oA��TA�~�A�/A���A��DA�O�A�+A���A�p�A�hsA�K�A�A�A�7LA�(�A��mA��\A�M�A�"�A�{A�JA�bA��`A��9A��A���A���A���A���A���A���A��uA��uA��A�A�A��^A��uA�x�A�ZA�=qA�33A�(�A��A��;A���A���A�r�A�(�A���A��\A��HA�dZA���A��A��/A��!A�
=A��A�C�A�"�A�{A�  A���A��\A�K�A��`A��A�XA���A�|�A�;dA���A���A�bNA�M�A�"�A��A���A�G�A���A�ĜA�bNA���A�|�A�XA�S�A�S�A�S�A�5?A��HA��A���A��^A��A���A��+A�r�A�v�A�t�A�bNA�7LA�1A�ƨA���A�33A���A���A��DA�dZA�A��A�Q�A�JA���A��^A�A�t�A�/A���A�A�A��^A��A�
=A��A��/A�ƨA���A�`BA��mA��hA�A�x�A�-A��A���A�|�A�O�A�  A�l�A�A�p�A�?}A���A�~�A�1'A���A���A�ffA�7LA��A�  A��mA��/A��
A���A���A��+A�I�A��A�A�A��RA���A�x�A�I�A�1A��TA�A��RA��!A���A���A��7A�t�A�r�A�p�A�n�A�dZA�S�A�K�A�G�A��A��
A��A��+A�jA�/A��A��/A�ƨA��FA���A��uA��A�\)A�7LA�
=A�ĜA�5?A��HA�t�A� �A��9A�|�A�O�A���A�ffA��^A�p�A�$�A���A�I�A��A�z�A���A���A�n�A�7LA��
A���A�O�A� �A�A��A��HA���A��wA��+A�(�A���A��A��-A�dZA�"�A��A��-A�(�A�  A��yA��;A���A��jA��\A�n�A�Q�A�+A��A���A��A��\A��A�p�A�l�A�ffA�dZA�ffA�ffA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              A��
AԸRAԲ-AԮA԰!Aԩ�Aԟ�Aԝ�A�bNA��AӰ!Aө�Aӟ�Aӛ�Aӗ�AӑhAӏ\AӍPAӉ7AӅAӁAӁAӁAӁA�~�A�~�A�|�A�z�A�z�A�v�A�p�A�l�A�l�A�l�A�l�A�jA�l�A�jA�jA�hsA�jA�jA�jA�l�A�l�A�l�A�l�A�n�A�n�A�n�A�n�A�l�A�jA�"�A�A���A� �ÃA˾wA�jA�ĜA�r�A�E�A���A���A���A���A���A���A�1'A�-A���A��PA��`A�+A�bNA��RA�E�A��RA�ZA�VA�C�A��HA��`A�&�A��A��A�VA��9A�ƨA���A��hA�^5A���A��wA��/A��A�bA���A�=qA�=qA��A��
A�bA�r�A�\)A���A�x�A�hsA�hsA��AzjAwXAt�ArbApȴAo�PAoAn�HAm�hAk��Ajr�Ai�Ah1'AgS�Af~�Ae�TAeVAdE�AcK�Aa7LA]t�A[`BAY�AW+AT�HAR��AP�\AO
=ALQ�AJ �AF��AC�-A@�`A>��A=+A;�A9A7x�A6Q�A5��A3�A2Q�A0��A/l�A,�RA*�A)�A(�yA&n�A%�7A$�9A$VA${A#�;A"�jA ��A��AVA\)A�!A�Az�AAAĜA�A�hAr�A�A%A1A7LA�A$�A��A�AbNA��A�A(�AS�A
�\A
(�A	|�A��A��A33AG�A?}AffA"�A��AI�AAC�A��@�"�@��@�@���A A I�AoA �u@��m@�V@���@��@���@��@��@�dZ@��9@�1'@�
=@��#@��@�-@��-@�1@ꟾ@��@�E�@�X@�D@旍@�hs@�S�@��@�o@�R@�O�@�h@��#@�^@��`@��;@ޏ\@�ȴ@١�@١�@�G�@���@�%@ش9@�1'@ם�@֟�@պ^@�X@ԋD@��H@��@�p�@���@Ѓ@Л�@Гu@Ѓ@�9X@��m@�+@Ώ\@�M�@͑h@���@�1'@ʟ�@ɩ�@ə�@ɑh@Ɂ@�O�@�V@���@���@ȼj@�j@��m@Ǖ�@�
=@Ƨ�@�~�@�{@�@ź^@�hs@��@���@�ƨ@Õ�@��@�M�@��#@��^@��h@�7L@��@���@�j@��@���@��F@���@��@�;d@�+@�S�@��@��y@��H@��@���@�^5@���@�G�@���@��@�z�@��@��@���@��@�\)@�+@��y@�~�@�5?@��T@���@���@�O�@�V@���@�b@��;@��F@�l�@�33@�
=@��H@�ȴ@��!@�~�@�^5@�=q@�{@���@�p�@���@��@�I�@�b@��@��@���@���@���@��@���@�=q@�{@���@��@���@��D@�z�@�Z@�1@�dZ@��!@�v�@�ff@�V@�E�@���@���@��D@�A�@��m@�S�@���@���@���@�~�@�M�@��@���@���@���@��7@��@��/@�z�@�\)@�@���@���@�n�@�=q@��@��7@�?}@��@��@�t�@�;d@�
=@���@���@�~�@�@��@�G�@��@��`@��@�Z@�l�@�+@�o@���@��!@�~�@�n�@�M�@��@��@��@�1'@��w@�dZ@�o@�ȴ@���@�~�@�{@��-@�x�@�7L@�%@�Ĝ@��D@�r�@�A�@��@��w@���@�S�@�ȴ@���@�^5@�-@�J@��@���@���@�hs@�&�@��@��@��@�1'@�1@���@�S�@���@�ff@��@��-@�X@���@��/@��@�A�@���@�
=@��+@��@�@�p�@�O�@���@��u@��D@�z�@�j@�bN@�(�@�1@��m@�ƨ@���@�l�@�"�@��H@���@�n�@�V@��@��h@�hs@�X@�?}@���@��@���@�r�@�bN@�A�@�b@���@�"�@��@��!@�@�@��-@��h@��@��@��@��@��@��@��@��@�X@��@��D@�r�@�bN@�I�@�A�@�(�@�1@��w@���@��@�;d@�"�@���@�n�@�=q@�@��#@��@���@�Ĝ@��@�9X@�;@+@~�R@}��@}�@|�@{��@z�@z�!@z�\@z^5@z�@yX@xbN@w��@wl�@w
=@vv�@v@u`B@uO�@u�@t�@s�
@s��@s��@s"�@r��@rn�@r^5@r=q@r�@q��@q&�@pĜ@p  @o�P@o\)@o+@nȴ@nȴ@n�R@n�+@n$�@m@m��@m�@mp�@mO�@m�@l��@l9X@k�F@k�@kt�@kC�@j��@j=q@j�@i��@i�#@i��@ix�@h�`@hQ�@h1'@h1'@f�y@e�@e�-@e/@d�j@d9X@cƨ@cC�@b�@b�!@a�@aG�@`�`@`�u@`  @_�P@^�@^ff@^{@]��@\�@\1@[ƨ@[o@Z-@Yhs@Y7L@Y�@X��@X��@Xr�@W\)@W
=@W
=@Vv�@U@U/@T�j@Tz�@S��@Sƨ@S33@R~�@R=q@Q�@Q��@Q�@P�9@P1'@Pb@O�;@O�P@O;d@O
=@N��@N�@M@MO�@L��@L��@LZ@K��@Kƨ@K��@KC�@J�H@J�!@J�\@J�\@J~�@Jn�@J=q@JJ@H�`@H �@G�P@GK�@G;d@G;d@G+@F��@F��@FV@E�T@E�@EO�@E�@EV@D�/@D�@C�@CS�@B�H@B~�@A��@Ax�@A��@Ahs@AX@@�`@@1'@@  @?�@>��@>v�@=�@=@=��@=O�@<�@<�j@<�D@<�D@<z�@<Z@<(�@<1@;dZ@;o@:�H@:��@:��@:��@:�!@:~�@9��@9&�@8�`@8��@8Q�@81'@8 �@7�@7�w@7��@7|�@7;d@6��@6��@65?@5�T@5@5`B@5?}@4�j@4�@4��@4I�@3�
@3t�@3"�@2�H@2�H@2�H@2��@2��@2�\@2n�@2=q@2J@1��@1��@1�7@1hs@1x�@1�7@1&�@0�`@0�9@0r�@0A�@/�;@/l�@/l�@/\)@/\)@/�@.�y@.v�@.E�@.@-��@-��@-�@-?}@,��@,Z@,9X@,(�@,�@+ƨ@+�F@+�F@+�F@+��@+dZ@+S�@+33@*�!@)��@)�^@)hs@(�`@(r�@(1'@( �@'�@'|�@';d@&�@&�R@&��@&V@&{@&@&@%@%?}@$�@$�D@$Z@$9X@$(�@#��@#��@#S�@"�H@"�!@"M�@"�@"�@!��@!��@!x�@!hs@!X@!G�@!�@ ��@ �`@ ��@ ��@ r�@ 1'@ b@�w@l�@;d@�@ȴ@�R@��@v�@$�@@�h@�@�@`B@O�@?}@�@�@�@�D@z�@z�@j@j@Z@I�@9X@�m@��@�@33@"�@�@~�@=q@�@��@�7@x�@&�@��@�@A�@b@��@��@;d@
=@�y@�R@��@��@��@v�@V@�@�h@p�@O�@?}@?}@/@�@��@�@�j@��@�D@Z@(�@(�@(�@�@�m@ƨ@t�@o@@��@��@�!@��@�\@n�@J@�@�@�#@�#@��@��@hs@&�@��@��@�`@�9@�@1'@b@�@��@�@|�@|�@\)G�O�A��/A��;A���A��;A��#A�ĜAԺ^AԮAԾwAԲ-A԰!A԰!AԲ-AԬAԮAԮAԩ�AԲ-AԴ9AԶFAԬAԮAԥ�Aԣ�Aԡ�Aԛ�Aԟ�Aԟ�AԓuAԡ�Aԡ�Aԏ\A�dZA�=qA�K�A��A�AӼjAө�AӾwAӮAӧ�Aө�Aӣ�Aӧ�Aӥ�Aӡ�Aӣ�Aӡ�Aӝ�Aӟ�Aӟ�Aӛ�Aӛ�Aӟ�Aӛ�Aә�Aӟ�Aӛ�Aӗ�Aӗ�Aә�Aӛ�Aӕ�Aӗ�Aӗ�AӓuAӕ�Aӕ�Aӏ\AӑhAӓuAӏ\AӍPAӑhAӍPAӍPAӑhAӋDAӏ\AӍPAӋDAӑhAӍPAӉ7Aӏ\AӍPAӋDAӏ\AӋDAӉ7AӋDAӉ7AӅAӉ7AӅAӅAӇ+AӃAӅAӉ7AӃAӃAӇ+AӅA�|�A�~�AӁA�~�A�|�A�~�AӅAӁA�~�AӃAӃA�~�AӃAӃA�~�A�~�AӃA�|�AӁAӃAӁA�~�AӅA�~�A�~�AӁA�~�A�|�AӁAӁA�|�AӁA�~�A�z�A�~�AӁA�|�A�|�AӁA�~�A�z�AӁA�~�A�z�A�|�AӁA�~�A�x�A�z�A�|�A�x�A�x�A�z�A�|�A�x�A�x�A�|�A�|�A�v�A�x�A�z�A�v�A�z�A�x�A�t�A�x�A�v�A�t�A�r�A�v�A�p�A�r�A�v�A�t�A�l�A�n�A�r�A�n�A�jA�p�A�l�A�jA�l�A�n�A�jA�jA�p�A�n�A�jA�l�A�n�A�hsA�l�A�l�A�hsA�n�A�l�A�hsA�jA�n�A�jA�hsA�n�A�n�A�jA�jA�n�A�n�A�jA�hsA�l�A�l�A�hsA�l�A�n�A�l�A�jA�n�A�l�A�hsA�jA�n�A�jA�hsA�n�A�jA�hsA�l�A�n�A�hsA�jA�l�A�l�A�ffA�hsA�l�A�hsA�ffA�jA�hsA�ffA�jA�jA�ffA�l�A�hsA�hsA�l�A�hsA�ffA�hsA�l�A�hsA�jA�l�A�hsA�jA�n�A�l�A�hsA�l�A�l�A�hsA�jA�n�A�l�A�jA�l�A�n�A�l�A�jA�n�A�n�A�jA�n�A�n�A�jA�jA�n�A�n�A�jA�jA�n�A�l�A�jA�l�A�p�A�l�A�jA�n�A�n�A�jA�n�A�p�A�l�A�l�A�p�A�n�A�l�A�p�A�p�A�l�A�n�A�r�A�n�A�n�A�r�A�n�A�jA�p�A�n�A�jA�p�A�n�A�jA�n�A�p�A�l�A�n�A�p�A�n�A�jA�n�A�n�A�jA�jA�p�A�l�A�jA�l�A�jA�hsA�l�A�jA�ffA�hsA�jA�^5A�O�A�"�A��A�ȴAҩ�A҃A�"�A���AѬA�p�A�`BA�K�A�C�A�A�A�/A�A�AЕ�A�{Aϧ�A�1A�ƨA�~�A͗�A�\)A�-A�A��yA���A̩�A�~�A�O�A�33A�&�A��A�VA�%A���A��A˟�A��A�I�A��A���A�l�A�{A���A��;A���A�ƨA�ƨA�ȴA���A���AȾwAȝ�A�"�A�`BA�33A�"�A��A�
=AƼjAƉ7A�n�A�\)A�=qA�{A��A��A��mAŰ!A��A�|�A�5?A�bA���A���A×�A�`BA�oA��TA�~�A�/A���A��DA�O�A�+A���A�p�A�hsA�K�A�A�A�7LA�(�A��mA��\A�M�A�"�A�{A�JA�bA��`A��9A��A���A���A���A���A���A���A��uA��uA��A�A�A��^A��uA�x�A�ZA�=qA�33A�(�A��A��;A���A���A�r�A�(�A���A��\A��HA�dZA���A��A��/A��!A�
=A��A�C�A�"�A�{A�  A���A��\A�K�A��`A��A�XA���A�|�A�;dA���A���A�bNA�M�A�"�A��A���A�G�A���A�ĜA�bNA���A�|�A�XA�S�A�S�A�S�A�5?A��HA��A���A��^A��A���A��+A�r�A�v�A�t�A�bNA�7LA�1A�ƨA���A�33A���A���A��DA�dZA�A��A�Q�A�JA���A��^A�A�t�A�/A���A�A�A��^A��A�
=A��A��/A�ƨA���A�`BA��mA��hA�A�x�A�-A��A���A�|�A�O�A�  A�l�A�A�p�A�?}A���A�~�A�1'A���A���A�ffA�7LA��A�  A��mA��/A��
A���A���A��+A�I�A��A�A�A��RA���A�x�A�I�A�1A��TA�A��RA��!A���A���A��7A�t�A�r�A�p�A�n�A�dZA�S�A�K�A�G�A��A��
A��A��+A�jA�/A��A��/A�ƨA��FA���A��uA��A�\)A�7LA�
=A�ĜA�5?A��HA�t�A� �A��9A�|�A�O�A���A�ffA��^A�p�A�$�A���A�I�A��A�z�A���A���A�n�A�7LA��
A���A�O�A� �A�A��A��HA���A��wA��+A�(�A���A��A��-A�dZA�"�A��A��-A�(�A�  A��yA��;A���A��jA��\A�n�A�Q�A�+A��A���A��A��\A��A�p�A�l�A�ffA�dZA�ffA�ffA�hsG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B��B��B�B��B��B��B�;B~�B|�B|�B|�B|PB{�B{�B{�B{�B{B{JB{�B{B{�B{�B{B{�B{B{JB{JBzDBzBzDBzDBzxBzxBzxBz�BzxBz�B{B{B{B{B{B{�B|B|B|�B}�B�B��B�uB��B�oB�xBu�BW�BVBI�B=�Bw2B� B�kB�wB�XB˒B��B�#B֡B�/B
�B�BB"hB�BMB �B
	BuB	B �B�ZB�B�BیB��B�BB�KB˒BɺB��B��B�$B�B�@B��B{B`BBC�B8RB �B
rBfB�B 4B
��B
��B
�B
�FB
��B
`�B
9�B
=�B
0!B
'B
 'B
B
7B
�B
oB
�B
�B	�B	��B	� B	�QB	�`B	ߤB	�?B	ϫB	�0B	�wB	�VB	�uB	�B	}VB	poB	iyB	[�B	P�B	C�B	5�B	-CB	%B	!B	B	4B	VB	%B	�B	B	  B	MB	�B��B�5B�B�B��B�]B��B��B�B��B�B�&B��B��B��B�>B��B�AB�vB��B��B�.B	_B��B��B�"B�(B��B�B��B��B��B�lB�2B�B�JB�]B�.B�(B��B	�B	�B	B	�B	_B	�B	�B	PB	�B	�B	B	B	�B	_B	�B	#B	2�B	:*B	LdB	X�B	VmB	[�B	`�B	b�B	dZB	poB	jB	jB	y>B	� B	{B	~�B	��B	u�B	`B	^�B	^�B	`�B	gmB	iyB	ncB	ncB	j�B	c B	d�B	e`B	o�B	m]B	poB	��B	�B	��B	��B	|PB	sB	sMB	w2B	v�B	zDB	�4B	��B	��B	��B	�MB	��B	�$B	��B	��B	��B	��B	��B	�}B	�!B	��B	��B	�tB	�OB	�EB	��B	�0B	��B	��B	��B	�HB	�pB	�pB	ΥB	ΥB	�vB	��B	ԕB	خB	ںB	��B	�pB	�|B	�B	�B	�ZB	��B	�B	��B	�fB	�B	�QB	�B	�B	��B	�B	�"B	��B	�]B	�cB	�cB	��B	�B	�;B	��B	�B	�iB	�B	�B	�B	��B	�>B	�B	�B	��B	�PB	��B	�cB	��B
�B
�B
�B
	�B

	B

	B

	B

rB

rB
B
�B
�B
B
�B
~B
�B
PB
�B
�B
(B
�B
bB
�B
�B
4B
hB
B
oB
oB
�B
�B
�B
FB
SB
�B
�B
eB
kB
�B
�B
	B
B
~B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
#B
$B
#�B
#�B
#nB
#B
$@B
$tB
%zB
&B
&�B
'�B
(XB
(XB
(�B
(�B
)*B
)�B
*eB
*�B
*eB
+B
,�B
,=B
-CB
.�B
.}B
.�B
/B
/B
/OB
/�B
0UB
0�B
2aB
49B
4nB
4�B
4�B
5tB
5�B
5�B
7B
8B
8�B
8�B
8�B
8�B
9XB
:�B
:*B
:*B
:*B
:�B
:^B
:*B
:*B
:�B
;�B
<B
=<B
>BB
>�B
?}B
?�B
@B
?�B
AUB
AUB
A�B
A B
A�B
B'B
CaB
B�B
C�B
C�B
C�B
C�B
D�B
E�B
E9B
E�B
EmB
EmB
E9B
E�B
E�B
F?B
F�B
GB
GB
GEB
HB
HB
I�B
JXB
K)B
K�B
MB
MB
M6B
M�B
M6B
MB
L�B
K�B
LdB
NB
N�B
O�B
O�B
PHB
QNB
R B
Q�B
R B
Q�B
RTB
R�B
R�B
S&B
S�B
S[B
T,B
T,B
TaB
T�B
T�B
T�B
V9B
V�B
W?B
W?B
W�B
XB
XB
X�B
X�B
X�B
YKB
YKB
Z�B
Z�B
[#B
Z�B
[�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[WB
[#B
[�B
[WB
[WB
[�B
\�B
]/B
]�B
]�B
^�B
^jB
^�B
_�B
`�B
`�B
`�B
`BB
`B
`�B
`�B
`vB
`BB
a|B
aB
`�B
`�B
`�B
a|B
a�B
aHB
aHB
b�B
bB
aHB
b�B
bB
b�B
c B
d&B
c�B
dZB
d&B
d&B
d&B
c�B
d�B
e`B
e�B
f2B
gmB
gmB
g�B
g�B
h
B
h�B
h�B
h�B
iB
i�B
jB
i�B
iyB
jB
j�B
jKB
jB
kQB
l"B
lWB
l�B
l�B
m�B
m�B
m]B
m)B
m)B
l�B
m)B
m�B
n/B
m�B
m�B
m�B
m�B
n�B
o�B
p;B
p;B
p�B
p�B
p�B
p�B
o5B
o B
qB
q�B
q�B
qAB
qB
p�B
qB
q�B
rGB
q�B
q�B
r|B
r�B
sMB
tTB
uZB
u�B
v�B
v�B
v�B
v�B
w2B
v�B
v�B
v�B
xB
xlB
x8B
x8B
x8B
x8B
x8B
y�B
zDB
z�B
z�B
|PB
|�B
|�B
|�B
}�B
}VB
~�B
~�B
~]B
~�B
~(B
~�B
�B
� B
� B
��B
�B
�B
�AB
�B
�AB
��B
��B
�AB
�AB
�B
��B
��B
��B
�GB
�{B
�MB
��B
��B
��B
��B
�MB
�B
�GB
�AB
�AB
�AB
��B
�B
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
�1B
�1B
�1B
�fB
�lB
�rB
�=B
��B
�B
�~B
��B
��B
��B
�B
��B
��B
��B
�"B
�"B
�VB
��B
�\B
��B
��B
��B
��B
�.B
�.B
�bB
�bB
�bB
�bB
�bB
�bB
��B
�bB
�.B
��B
��B
��B
��B
��B
��B
�.B
�bB
��B
� B
�4B
��B
�:B
��B
��B
�B
�@B
�@B
�B
��B
�@B
�uB
�@B
�oB
�oB
�B
�B
�:B
�B
��B
�B
�B
�FB
��B
��B
��B
��B
��B
�B
�$B
��B
�_B
��B
��B
��B
��B
��B
�eB
��B
��B
��B
�B
�kB
�kB
��B
�	B
�=B
�=B
�	B
�=B
�B
�CB
�B
�CB
��B
�xB
�xB
�CB
�xB
��B
��B
��B
��B
�B
�OB
�OB
��B
�!B
��B
�VB
��B
��B
�'B
�\B
��B
�\B
��B
��B
��B
��B
��B
�-B
��B
��B
��B
�hB
�4B
��B
��B
�:B
�:B
�B
��B
��B
��B
��B
�@B
�tB
�tB
��B
�zB
��B
��B
��B
��B
�B
�B
��B
�LB
��B
��B
�RB
�RB
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
��B
��B
�*B
��B
��B
��B
��B
�0B
��B
�0B
�0B
�0B
�0B
��B
��B
��B
�6B
�6B
�kB
�B
�B
�=B
�qB
�=B
�=B
�qB
�qB
��B
�qB
�qB
��B
��B
��B
�B
�B
��B
��B
�B
�}B
�B
�OB
�!B
�UB
��B
��B
��B
��B
��B
�'B
�'B
�[B
�[B
��B
��B
��B
�aB
�aB
�-B
�-B
��B
�aB
��B
�3B
�hB
��B
��B
��B
��B
��B
�9B
�nB
��B
��B
��B
��B
��B
��B
�?B
�tB
��B
�tB
�tB
��B
��B
�FB
�zB
�zB
�zB
��B
��B
��B
��B
�LB�MB�MB��B��B�fB��B�B�B��B��B��B�SB�B��B��B��B��B��B�;B�fB�MB��B��B�B��B��B�{B��B��B�oB��B�GB��B}�Bt�B�:B{B}�B�iB{�B� B}"B{�B}�B{B|PB}�B|B|�B}�B|�B{�B}�B}�B{�B|�B|�B{B|B}"B}�B{�B{B}�B|�B{�B|�B{�B{JB|�B|B{B{�B|�Bz�B|�B|B{B|�B{JB{�B|�Bz�B|B}"B{B{�B|�Bz�B|�B|�B{JBz�B|�B{B|PB{�BzxBzxB{JBzxB|�B|�B{B{B{�B{BzBz�B{�Bz�Bz�B{�B}"BzxBzB|�B{Bz�B|PB{�BzDB|�B{�Bz�B|B|�B{B|�B|PBz�B{�B|PBz�Bz�B|�Bz�B{�B|�B{�BzDB{�B|BzxB{�B|�BzxB{B|�B|BzxB{B}"B{�BzDB{�B|�B{BzxB{�B|�BzDB{B|�B{Bz�B|PBz�B{JB|PBzxB{B|B|PBy�B{B{By�By	B{JBy�BxBzDB{�By	BzBz�By�Bx�Bz�B{Bx�By�B{BzxByrB{JBy�By�B{JBy>BzDB{BzDBy	Bz�B{JBy>By�B{JB{JBy�ByrB{JB{By�BzxB{BzDBy>BzxB{JByrBzDB{Bz�By�BzxB{By�Bz�B{�By�ByrB{JB{ByrBy�B{�B{By�B{JB{�BzB{B|PBzDBzxB|PBzB{�B{�BzB{B|�B{�BzB|PB{JBzDB{�B{BzBz�B|�B{Bz�B|PB|PBzxB{JB|�B{BzxB{�B|PBz�BzxB|�B{JBz�B|�B|�Bz�B{JB|�B|�B{JB{�B}"B|Bz�B|�B}"B{B{B}VB{�B{B|�B|�B{JB|B}"B{�B{�B}VB|�B{�B|�B}�B{B}�B.B}�B}�B� B}�B~�B�4B.B~�B��B� B.B��B�GB��B��B��B�AB�4B��B��B�B��B�GB��B�uB�{B��B��B�{B��B��B�"B�.B�4B�VB��B�kB�oB��B�~B��B�B�1B�=B��B��B��B��B�AB�JBu%Bu�B�BiBc Ba|B]dBW
BY�BU2BW
BQ�BTaBTaBS&BR BT�BQ�B[WBT�B^�BEBF�BR�BJXB@�B@�B@�B>wB=�B;�B=�B<�B<�BHKBm�B��BzDB}�B|�B~]B�B�DB�"B��B��B��B�_B�FB�:B��B��B��B��B�!B�aB�FB�-B��B�*B�BB��B�?B�mBƨB�B�KB��BȴB��BŢBƨBǮB�BB�BѷB�dB�9B�^B��B��B�vB��B��B�XB��B�#B�RBȀBɆBȀBʌB�KB�B�B�B��B�B�B�B�B��B�ZB�>B 4B
�B�B �B!�B	B$BBPB�B%BxB�B:B�BbB�BB�B!B�B$B.�B&�B%zB�B�B�B	lB(B�BDB�B�B�>B�BYB�B  B�(B�B��B�B_B�B+B	B�B�B�BVBPB(B�B=B�BIB�B$tB�B$B�B�B�BE�B+B{B�B&LBB��B�B�JB�B�+B�`B��B�B��B�NB��B�&B�B�B�
B��B��B�BB�jB��B��B��B�pB�#B�BBбB��B�B��B�TBԕBҽB̘B�0BǮB�XB�BǮB�?B�zB��B��B��BȴB�KB�B�0BҽB՛B�gB�BB�3B��B� B��B��B�aB�UB�jB�6B��B�wB�wB��B��B�gB�B��B��B�FB�9B�B��B�0B�eB��B��B��B��B��B�zB�IB��B��B��B�xB�B�	B�B�+B�hB��Bu�BzBx�Bo5Bm]BkQBn�BT,Bb�BQ�BU�BL�BJ�B?BAUB;�B;0B7�B8B9�B6�BC-B)�B0�B"4BqB�BCBB
�B
	B
rB	B�B	B�B�B~B	7B_B%BYBB{BB�B�BBoB  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021111504390020211115043900IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111610010020211116100100QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111610010020211116100100QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365420220126093654IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                