CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-05-23T22:36:12Z creation; 2020-07-07T21:55:47Z DMQC;      
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
_FillValue        G�O�     �  cP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ΀   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � XP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20200523223612  20210429202812  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_134                 6810_008521_134                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���`k@���`k11  @��3333@��3333@3,�j~��@3,�j~���e+�XdE�e+�XdE11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@B�\@�  @�  @�  @�  @�(�A  A\)A,(�A@  A_\)A�  A�  A��A�  A�  A�  A�Q�A��B (�B  B�
B�
B   B'�
B0  B8  B?�
BH(�BP(�BX  B`  Bh  Bp  Bx  B�
B��
B��
B��
B��B��B��B�  B�  B�  B�{B��
B�  B�  B�{B�(�B�  B�{B�{B��B�  B�{B�  B��B��B�{B��B�  B�  B�{B�  B�  C 
=C  C  C{C
=C

=C
=C  C��C  C  C  C  C  C  C  C   C"  C$
=C&
=C(  C)��C+��C.  C0  C2  C4  C6  C8  C:
=C<
=C>
=C@{CB{CC��CF  CH  CI��CK��CM�CP
=CR
=CT  CV
=CX  CZ  C\
=C]��C`  Cb  Cd
=Ce��Ch
=Cj
=Cl
=Cm��Cp
=Cr  Ct  Cv  Cx  Cz  C|
=C~  C�C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�  C�  C�C�  C�  C���C���C�  C�  C�C�  C���C�  C�  C�  C�C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�C�  C���C���C�C�  C�
=C�
=C���C�C�  C�  C�C�  C�  C���C�  C�C���C�  C�C�C���C�C�C�C�  C���C�  C�C�C���C�  C�C�C�C�C���C�  C�  C�  C�  C�  C�  C���C���C�C�  C���C���C�  C�  C�C�C�C���C���C�
=C�  C�  C�C�C���C���C���C���C�C�C�C�
=C�C���C�  C�C�  C���C��C��C���C�  C�  C�
=C�
=D �D � D ��D� D�qD� D�D��DD�D�D��D  D� DD��D�D� D��D	}qD	�qD
� D�D�D�D� D�qD}qD  D��D�D��DD��D  D� D�qD}qD��D� D  D� D  D� D�qD}qD�D�D�D��D  D� D  D� D�D��D  D� D  D� D  D}qD  D�D �D ��D �qD!}qD"  D"� D#  D#� D$�D$}qD$�qD%}qD&  D&� D'�D'}qD(  D(� D)�D)��D)�qD*z�D*�qD+� D,  D,��D-D-��D.  D.z�D/  D/��D0  D0}qD0�qD1}qD2  D2��D3  D3� D4  D4� D5  D5��D6  D6� D6�qD7}qD8  D8� D8�qD9��D9�qD:� D;�D;}qD<�D<� D<�qD=� D>�D>��D?D?� D?�qD@� D@�qDA}qDA�qDB� DB�qDC� DD�DD��DE  DE}qDE�qDF� DF�qDG}qDG�qDH� DIDI� DI�qDJ� DK�DK�DK�qDL� DL�qDM}qDN�DN��DO  DO� DP�DP}qDP�qDQ� DR  DRz�DR��DS� DT�DT�DU  DU� DU�qDV� DWDW��DX�DX��DYDY� DY�qDZ��D[�D[� D[�qD\� D]  D]}qD^  D^��D_D_� D`�D`�Da  Da}qDb�Db�Dc�Dc� Dd  Dd�De�De� De�qDf�Dg  Dg}qDh�Dh��Di�Di�Di�qDj� Dk  Dk� Dk�qDl� Dm  Dm}qDn  Dnz�Dn�qDo�DpDp�Dq  Dq� Dr�Dr� Ds  Ds}qDs�qDt� Du  Du�Du�qDv� Dw�Dw}qDw�qDx��Dy�Dy��Dy�qDz}qD{�D{�D|  D|}qD}  D}��D~�D~}qD�D�D�HD�@ D��HD�� D�  D�AHD�� D��HD�  D�AHD�� D��qD���D�AHD���D���D�HD�>�D�� D�� D�HD�AHD�� D�� D���D�>�D�~�D���D�HD�AHD�� D��HD�  D�>�D�� D���D���D�AHD�~�D���D���D�@ D�� D���D�  D�@ D�� D��qD��qD�>�D�� D��HD�HD�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D��HD���D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ D�� D�� D���D�@ D�� D��HD�  D�@ D��HD���D�HD�AHD�� D�� D���D�=qD��HD��HD���D�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D�~�D���D�HD�B�D�� D�� D�HD�@ D�~�D���D���D�B�D���D��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD�� D��HD��D�@ D�}qD��qD���D�@ D�� D���D���D�AHD��HD��HD���D�>�D�}qD��qD�  D�AHD���D��HD���D�=qD�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D���D�  D�@ D�� D���D�  D�B�D��HD��qD��qD�>�D�~�D���D�  D�@ D�~�D�D�HD�>�D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D���D�=qD�~�D�� D�  D�AHD�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D���D�HD�AHD��HD�D��D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�~�D���D�HD�@ D��HD��HD���D�@ D�� D��qD��qD�@ D���D��HD�  D�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�~�D�� D�  D�>�DÀ D�� D���D�>�DĀ D�� D�HD�>�D�}qD��HD�HD�>�Dƀ D�� D��qD�@ DǁHD�� D��D�B�D�~�DȾ�D�  D�@ DɁHD��HD�HD�AHDʀ Dʾ�D�  D�@ Dˀ D˾�D�  D�AHD�~�D̾�D�  D�>�D�~�D;�D�  D�AHD΀ DνqD���D�@ D�~�D�� D��D�@ D�~�D�� D���D�@ D�~�D�� D�  D�AHDҀ DҾ�D���D�=qD�~�DӾ�D�  D�AHDԁHD��HD�  D�@ DՀ D�D�  D�>�Dր D�� D���D�>�D׀ D��HD�HD�AHD؀ D�� D�HD�AHDـ D�� D�  D�AHDڀ D�� D���D�@ Dۀ D�� D���D�@ D܂�D��HD�  D�AHD݀ Dݾ�D�  D�@ Dހ D�D�HD�AHD�~�D�� D�  D�@ D�� D�� D���D�AHD�HDᾸD���D�>�D� D�� D���D�@ D�~�D�� D�  D�@ D䂏D�� D���D�@ D�~�D徸D�HD�AHD� D��HD��D�@ D�}qD�� D���D�=qD� D�D���D�>�D� D龸D��qD�>�D� D��HD�HD�@ D�HD�D��D�@ D�~�D쾸D�  D�AHD�}qD���D���D�=qD� D�D�  D�>�D� D�� D�HD�@ D�� D�� D��qD�@ D� D��HD�HD�@ D� D�D���D�@ D�g�>�?8Q�?aG�?���?\?�ff?��H@z�@(��@5@J=q@^�R@p��@�  @���@���@�(�@��@��@�@�G�@�=q@У�@�(�@���@��@�Q�A   A�A�A��Az�A��A{A#33A(Q�A,��A0  A3�
A:=qA=p�AA�AFffAL(�AQG�AU�AXQ�A^�RAb�\AfffAk�AqG�AuAx��A~{A��A�(�A�{A�Q�A�33A�{A�  A��A���A�\)A�G�A�33A�{A���A��\A�z�A�
=A�=qA�(�A�{A���A�33A�p�A�\)A�G�A��
A��RA���A\A���A�
=A��A�(�A�{A�  A�33A�A�\)Aٙ�A��
A޸RA�G�A��HA��A�A�\A�z�A�ffA�G�A�(�A�{A��A��HA�p�A�\)B ��BB
=BQ�B��BffB�B��B
=qB33B(�B��B�HB  B��B{B\)Bz�B�B�RB�B��B=qB\)B(�Bp�B�RB   B ��B!�B#33B$z�B%B&�RB'�B(��B*=qB+�B,z�B-p�B.ffB/�
B1�B2{B333B4  B4��B6=qB7�B8��B9��B:�RB<(�B=p�B>�\B?�B@��BB=qBC�BD��BE�BG
=BHz�BI�BJ�HBK�
BMG�BN�RBP(�BQ�BR=qBS�BT��BV=qBW\)BXQ�BY��B[
=B\z�B]B^�\B_�BaG�Bb�\Bc�
Bd��Be�Bg33Bh��Bi�Bj�HBl(�Bmp�Bn�HBp(�BqG�Br=qBs�Bu�BvffBw�Bxz�By��Bz�HB|Q�B}��B~�RB�B�ffB��B�B�Q�B��HB�\)B��B�z�B�33B��
B�z�B���B�p�B�  B��RB�p�B�{B��\B��B�B�z�B�33B�B�Q�B��HB��B�Q�B���B��B�  B��RB�p�B�{B���B��B��B�ffB��B��B�=qB��RB�p�B�(�B��RB�G�B�B�ffB�
=B��B�=qB���B�
=B��B�=qB��RB��B�p�B�{B��\B��HB�33B���B�{B���B�
=B�p�B�B�=qB���B�G�B���B��B�ffB��HB�p�B��B�Q�B��\B�
=B��B�{B�z�B���B�33B�B�=qB���B�
=B�p�B��B�ffB���B�p�B�B�{B��\B�
=B���B�  B�ffB��RB��B��B�(�B��\B���B�G�B��B�(�B��RB��B�p�B�B�=qB���B�G�B��B�  B�ffB��HB�\)B��B�  B�ffB��HB�\)B��B�=qB��\B���B�p�B��B�ffB¸RB��B�p�B�  B�ffB���B�33BŅB�  B�ffB��HB�p�B��B�=qB�z�B��HB�\)B��B�ffB���B��B˅B��B�z�B���B�p�B��
B�(�BΏ\B�
=Bϙ�B�(�B�z�B��HB�G�BѮB�=qBҸRB�33B�p�B��B�=qB��HB�\)B�B�(�B֏\B���BׅB�  B�Q�B���B��BمB�{Bڣ�B�
=B�\)B�B�(�B܏\B�
=B݅B�  B�Q�Bޣ�B��Bߙ�B�{B��B���B�G�B�B�{B��B��B�B��
B�(�B�\B�
=B噚B�  B�Q�B��B���B�B�  B�z�B���B��B�B�{B�z�B��HB�G�B�B��B�Q�B�RB�33B��B��B�=qB��B�
=B�B��B�=qB��B��HB�G�B�B�(�B�\B�
=B�p�B��
B�=qB�z�B���B�33B��B�(�B���B��B�p�B��
B�(�B���B��B���B�{B�z�B��HB�33B��B�(�B���B��B���B�  B�Q�B��RB��B��C {C \)C ��C C  C33CffC��C�
C�C\)C��C��C  C(�CffC��C�HC�C\)C��CC�C�CffC��C�HC{C=qCp�C��C��C  C=qCz�C�RC�C(�CQ�C�C�RC�C	�C	\)C	��C	�
C
{C
Q�C
�C
�RC
�HC{CG�C�CC  C33CffC��CC��C�C\)C�CC  C33CffC��C�
C
=C=qCp�C��C�
C�C\)C��C�
C{C=qCz�C��C�
C
=CG�C�\C��C  C(�CQ�C�\CC
=C=qCz�C�C�
C  C33CffC��C��C{CQ�C�\CC�HC
=C=qCp�C��C�HC�CQ�Cz�C�C�HC  C33CffC�\C��C
=C=qCp�C��C��C��C(�C\)C�\C��C  CG�Cz�C��C��C
=CG�C�C�RC�HC{C=qCffC��C��C
=CG�C�C�RC�HC {C G�C �\C ��C!  C!(�C!\)C!�\C!��C"  C"G�C"�C"C#  C#33C#\)C#�\C#�
C${C$Q�C$�\C$�RC$�C%{C%Q�C%�\C%�
C&�C&G�C&p�C&�C'  C'=qC'ffC'��C'�
C(
=C(Q�C(��C(��C)  C)33C)ffC)��C)�HC*(�C*ffC*��C*�
C+  C+33C+p�C+�C+�C,33C,\)C,�\C,C-
=C-G�C-�C-�RC-�HC.�C.\)C.��C.�HC/�C/Q�C/�C/�RC/�C0=qC0z�C0C0��C1(�C1\)C1��C1�
C2�C2ffC2�C2�HC3
=C3=qC3�C3��C4�C4\)C4��C4C5
=C5G�C5��C5�HC6�C6Q�C6�\C6��C7  C7G�C7��C7�HC8�C8Q�C8�\C8��C9{C9p�C9�C9��C:33C:ffC:��C:��C;=qC;�\C;�
C<{C<Q�C<�C<C={C=\)C=�C=�C>�C>\)C>�\C>�
C?�C?ffC?�RC?��C@(�C@p�C@��C@�
CA�CA\)CA�CA��CBG�CB�CBCC  CC33CCp�CC�CC�CD=qCD�CD��CE  CE33CEp�CE�CE��CF33CFz�CF�CF�HCG�CG\)CG�\CG�
CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                  ?�  ?��H@B�\@�  @�  @�  @�  @�(�A  A\)A,(�A@  A_\)A�  A�  A��A�  A�  A�  A�Q�A��B (�B  B�
B�
B   B'�
B0  B8  B?�
BH(�BP(�BX  B`  Bh  Bp  Bx  B�
B��
B��
B��
B��B��B��B�  B�  B�  B�{B��
B�  B�  B�{B�(�B�  B�{B�{B��B�  B�{B�  B��B��B�{B��B�  B�  B�{B�  B�  C 
=C  C  C{C
=C

=C
=C  C��C  C  C  C  C  C  C  C   C"  C$
=C&
=C(  C)��C+��C.  C0  C2  C4  C6  C8  C:
=C<
=C>
=C@{CB{CC��CF  CH  CI��CK��CM�CP
=CR
=CT  CV
=CX  CZ  C\
=C]��C`  Cb  Cd
=Ce��Ch
=Cj
=Cl
=Cm��Cp
=Cr  Ct  Cv  Cx  Cz  C|
=C~  C�C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�C�C�  C�  C�  C�  C�C�  C�  C���C���C�  C�  C�C�  C���C�  C�  C�  C�C���C���C�  C�  C�  C�  C�  C�  C�C�  C���C���C�  C�C�  C���C���C�C�  C�
=C�
=C���C�C�  C�  C�C�  C�  C���C�  C�C���C�  C�C�C���C�C�C�C�  C���C�  C�C�C���C�  C�C�C�C�C���C�  C�  C�  C�  C�  C�  C���C���C�C�  C���C���C�  C�  C�C�C�C���C���C�
=C�  C�  C�C�C���C���C���C���C�C�C�C�
=C�C���C�  C�C�  C���C��C��C���C�  C�  C�
=C�
=D �D � D ��D� D�qD� D�D��DD�D�D��D  D� DD��D�D� D��D	}qD	�qD
� D�D�D�D� D�qD}qD  D��D�D��DD��D  D� D�qD}qD��D� D  D� D  D� D�qD}qD�D�D�D��D  D� D  D� D�D��D  D� D  D� D  D}qD  D�D �D ��D �qD!}qD"  D"� D#  D#� D$�D$}qD$�qD%}qD&  D&� D'�D'}qD(  D(� D)�D)��D)�qD*z�D*�qD+� D,  D,��D-D-��D.  D.z�D/  D/��D0  D0}qD0�qD1}qD2  D2��D3  D3� D4  D4� D5  D5��D6  D6� D6�qD7}qD8  D8� D8�qD9��D9�qD:� D;�D;}qD<�D<� D<�qD=� D>�D>��D?D?� D?�qD@� D@�qDA}qDA�qDB� DB�qDC� DD�DD��DE  DE}qDE�qDF� DF�qDG}qDG�qDH� DIDI� DI�qDJ� DK�DK�DK�qDL� DL�qDM}qDN�DN��DO  DO� DP�DP}qDP�qDQ� DR  DRz�DR��DS� DT�DT�DU  DU� DU�qDV� DWDW��DX�DX��DYDY� DY�qDZ��D[�D[� D[�qD\� D]  D]}qD^  D^��D_D_� D`�D`�Da  Da}qDb�Db�Dc�Dc� Dd  Dd�De�De� De�qDf�Dg  Dg}qDh�Dh��Di�Di�Di�qDj� Dk  Dk� Dk�qDl� Dm  Dm}qDn  Dnz�Dn�qDo�DpDp�Dq  Dq� Dr�Dr� Ds  Ds}qDs�qDt� Du  Du�Du�qDv� Dw�Dw}qDw�qDx��Dy�Dy��Dy�qDz}qD{�D{�D|  D|}qD}  D}��D~�D~}qD�D�D�HD�@ D��HD�� D�  D�AHD�� D��HD�  D�AHD�� D��qD���D�AHD���D���D�HD�>�D�� D�� D�HD�AHD�� D�� D���D�>�D�~�D���D�HD�AHD�� D��HD�  D�>�D�� D���D���D�AHD�~�D���D���D�@ D�� D���D�  D�@ D�� D��qD��qD�>�D�� D��HD�HD�AHD�� D�� D�HD�>�D�� D��HD�  D�@ D��HD���D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D�~�D��qD�  D�@ D�~�D�� D���D�@ D�� D�� D���D�@ D�� D��HD�  D�@ D��HD���D�HD�AHD�� D�� D���D�=qD��HD��HD���D�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D�~�D���D�HD�B�D�� D�� D�HD�@ D�~�D���D���D�B�D���D��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD�� D��HD��D�@ D�}qD��qD���D�@ D�� D���D���D�AHD��HD��HD���D�>�D�}qD��qD�  D�AHD���D��HD���D�=qD�~�D��HD�HD�AHD�� D�� D�HD�@ D�� D�� D�  D�>�D�� D���D�  D�@ D�� D���D�  D�B�D��HD��qD��qD�>�D�~�D���D�  D�@ D�~�D�D�HD�>�D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D���D�=qD�~�D�� D�  D�AHD�~�D���D�  D�@ D�� D���D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�@ D�~�D���D�HD�AHD��HD�D��D�AHD�� D�� D�  D�AHD��HD�� D���D�@ D�~�D���D�HD�@ D��HD��HD���D�@ D�� D��qD��qD�@ D���D��HD�  D�AHD��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�AHD�~�D�� D�  D�>�DÀ D�� D���D�>�DĀ D�� D�HD�>�D�}qD��HD�HD�>�Dƀ D�� D��qD�@ DǁHD�� D��D�B�D�~�DȾ�D�  D�@ DɁHD��HD�HD�AHDʀ Dʾ�D�  D�@ Dˀ D˾�D�  D�AHD�~�D̾�D�  D�>�D�~�D;�D�  D�AHD΀ DνqD���D�@ D�~�D�� D��D�@ D�~�D�� D���D�@ D�~�D�� D�  D�AHDҀ DҾ�D���D�=qD�~�DӾ�D�  D�AHDԁHD��HD�  D�@ DՀ D�D�  D�>�Dր D�� D���D�>�D׀ D��HD�HD�AHD؀ D�� D�HD�AHDـ D�� D�  D�AHDڀ D�� D���D�@ Dۀ D�� D���D�@ D܂�D��HD�  D�AHD݀ Dݾ�D�  D�@ Dހ D�D�HD�AHD�~�D�� D�  D�@ D�� D�� D���D�AHD�HDᾸD���D�>�D� D�� D���D�@ D�~�D�� D�  D�@ D䂏D�� D���D�@ D�~�D徸D�HD�AHD� D��HD��D�@ D�}qD�� D���D�=qD� D�D���D�>�D� D龸D��qD�>�D� D��HD�HD�@ D�HD�D��D�@ D�~�D쾸D�  D�AHD�}qD���D���D�=qD� D�D�  D�>�D� D�� D�HD�@ D�� D�� D��qD�@ D� D��HD�HD�@ D� D�D���D�@ G�O�>�?8Q�?aG�?���?\?�ff?��H@z�@(��@5@J=q@^�R@p��@�  @���@���@�(�@��@��@�@�G�@�=q@У�@�(�@���@��@�Q�A   A�A�A��Az�A��A{A#33A(Q�A,��A0  A3�
A:=qA=p�AA�AFffAL(�AQG�AU�AXQ�A^�RAb�\AfffAk�AqG�AuAx��A~{A��A�(�A�{A�Q�A�33A�{A�  A��A���A�\)A�G�A�33A�{A���A��\A�z�A�
=A�=qA�(�A�{A���A�33A�p�A�\)A�G�A��
A��RA���A\A���A�
=A��A�(�A�{A�  A�33A�A�\)Aٙ�A��
A޸RA�G�A��HA��A�A�\A�z�A�ffA�G�A�(�A�{A��A��HA�p�A�\)B ��BB
=BQ�B��BffB�B��B
=qB33B(�B��B�HB  B��B{B\)Bz�B�B�RB�B��B=qB\)B(�Bp�B�RB   B ��B!�B#33B$z�B%B&�RB'�B(��B*=qB+�B,z�B-p�B.ffB/�
B1�B2{B333B4  B4��B6=qB7�B8��B9��B:�RB<(�B=p�B>�\B?�B@��BB=qBC�BD��BE�BG
=BHz�BI�BJ�HBK�
BMG�BN�RBP(�BQ�BR=qBS�BT��BV=qBW\)BXQ�BY��B[
=B\z�B]B^�\B_�BaG�Bb�\Bc�
Bd��Be�Bg33Bh��Bi�Bj�HBl(�Bmp�Bn�HBp(�BqG�Br=qBs�Bu�BvffBw�Bxz�By��Bz�HB|Q�B}��B~�RB�B�ffB��B�B�Q�B��HB�\)B��B�z�B�33B��
B�z�B���B�p�B�  B��RB�p�B�{B��\B��B�B�z�B�33B�B�Q�B��HB��B�Q�B���B��B�  B��RB�p�B�{B���B��B��B�ffB��B��B�=qB��RB�p�B�(�B��RB�G�B�B�ffB�
=B��B�=qB���B�
=B��B�=qB��RB��B�p�B�{B��\B��HB�33B���B�{B���B�
=B�p�B�B�=qB���B�G�B���B��B�ffB��HB�p�B��B�Q�B��\B�
=B��B�{B�z�B���B�33B�B�=qB���B�
=B�p�B��B�ffB���B�p�B�B�{B��\B�
=B���B�  B�ffB��RB��B��B�(�B��\B���B�G�B��B�(�B��RB��B�p�B�B�=qB���B�G�B��B�  B�ffB��HB�\)B��B�  B�ffB��HB�\)B��B�=qB��\B���B�p�B��B�ffB¸RB��B�p�B�  B�ffB���B�33BŅB�  B�ffB��HB�p�B��B�=qB�z�B��HB�\)B��B�ffB���B��B˅B��B�z�B���B�p�B��
B�(�BΏ\B�
=Bϙ�B�(�B�z�B��HB�G�BѮB�=qBҸRB�33B�p�B��B�=qB��HB�\)B�B�(�B֏\B���BׅB�  B�Q�B���B��BمB�{Bڣ�B�
=B�\)B�B�(�B܏\B�
=B݅B�  B�Q�Bޣ�B��Bߙ�B�{B��B���B�G�B�B�{B��B��B�B��
B�(�B�\B�
=B噚B�  B�Q�B��B���B�B�  B�z�B���B��B�B�{B�z�B��HB�G�B�B��B�Q�B�RB�33B��B��B�=qB��B�
=B�B��B�=qB��B��HB�G�B�B�(�B�\B�
=B�p�B��
B�=qB�z�B���B�33B��B�(�B���B��B�p�B��
B�(�B���B��B���B�{B�z�B��HB�33B��B�(�B���B��B���B�  B�Q�B��RB��B��C {C \)C ��C C  C33CffC��C�
C�C\)C��C��C  C(�CffC��C�HC�C\)C��CC�C�CffC��C�HC{C=qCp�C��C��C  C=qCz�C�RC�C(�CQ�C�C�RC�C	�C	\)C	��C	�
C
{C
Q�C
�C
�RC
�HC{CG�C�CC  C33CffC��CC��C�C\)C�CC  C33CffC��C�
C
=C=qCp�C��C�
C�C\)C��C�
C{C=qCz�C��C�
C
=CG�C�\C��C  C(�CQ�C�\CC
=C=qCz�C�C�
C  C33CffC��C��C{CQ�C�\CC�HC
=C=qCp�C��C�HC�CQ�Cz�C�C�HC  C33CffC�\C��C
=C=qCp�C��C��C��C(�C\)C�\C��C  CG�Cz�C��C��C
=CG�C�C�RC�HC{C=qCffC��C��C
=CG�C�C�RC�HC {C G�C �\C ��C!  C!(�C!\)C!�\C!��C"  C"G�C"�C"C#  C#33C#\)C#�\C#�
C${C$Q�C$�\C$�RC$�C%{C%Q�C%�\C%�
C&�C&G�C&p�C&�C'  C'=qC'ffC'��C'�
C(
=C(Q�C(��C(��C)  C)33C)ffC)��C)�HC*(�C*ffC*��C*�
C+  C+33C+p�C+�C+�C,33C,\)C,�\C,C-
=C-G�C-�C-�RC-�HC.�C.\)C.��C.�HC/�C/Q�C/�C/�RC/�C0=qC0z�C0C0��C1(�C1\)C1��C1�
C2�C2ffC2�C2�HC3
=C3=qC3�C3��C4�C4\)C4��C4C5
=C5G�C5��C5�HC6�C6Q�C6�\C6��C7  C7G�C7��C7�HC8�C8Q�C8�\C8��C9{C9p�C9�C9��C:33C:ffC:��C:��C;=qC;�\C;�
C<{C<Q�C<�C<C={C=\)C=�C=�C>�C>\)C>�\C>�
C?�C?ffC?�RC?��C@(�C@p�C@��C@�
CA�CA\)CA�CA��CBG�CB�CBCC  CC33CCp�CC�CC�CD=qCD�CD��CE  CE33CEp�CE�CE��CF33CFz�CF�CF�HCG�CG\)CG�\CG�
CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�AӰ!A�n�A�jA�hsA�ffA�dZA�\)A�^5A�^5A�`BA�`BA�^5A�`BA�`BA�^5A�`BA�\)A�\)A�ZA�\)A�\)A�XA�K�A�E�A�A�A�A�A�?}A�=qA�=qA�9XA�7LA�-A� �A��A�1A��A���A�l�A�1A�(�A�r�A�M�A��A΁A��A�A�A��A�A͕�A��A���A̾wA�z�A�"�A���AˮA˛�A�r�A��/AʅA�O�A��#A�z�A�jA�5?A�%Aȝ�A��yAǋDA�dZA�M�A�{A��A��A��`A��A�|�A�|�A�^5A�\)A���A�dZA��uA��DA�(�A��wA��A���A���A�+A���A�M�A���A�/A���A��!A�33A�bA��-A�ffA��A��!A���A�~�A�VA�=qA�x�A��A���A���A���A���A��
A�
=A�{A��A��A���A��A�?}A�-A���A�S�A��A�?}A��A���A�(�A~$�A{�-Av=qAu�At�uAs�Ap�!Akt�Ah�`AgK�Afn�AbJA\1A[?}AZ^5AW��AV=qAT �AP^5AO�TAOl�AOANz�AMhsAJ��AH��AG�#AG��AGK�AE��AC��A>1'A;�A:�`A:�jA9�A8 �A7K�A6�`A5��A4VA3��A3A3�7A2��A1��A1;dA.�A-�hA,�A+/A*��A)��A(�9A(5?A(9XA(1A'7LA&JA%�A#��A#S�A"Q�A!C�A r�A�Ap�A^5A+A�A|�Ar�A�#A�A�9Av�A(�A�wA(�A�A��A�A��AƨA�AffA$�A`BA/A
5?A	C�A	VA�HAȴA��A�!Ar�A��AI�A�-A+A �+@��;@�?}@�A�@��w@�|�@�;d@��+@��7@�V@��9@�z�@�A�@�1@��
@���@���@���@��@�j@�1'@�
=@�~�@�@�p�@��`@�D@�bN@�9X@��m@�F@�|�@�
=@�\@���@�`B@�u@�ȴ@�$�@噚@�Ĝ@�j@��@�hs@�u@�X@�G�@�@�|�@��/@ާ�@��@ݩ�@�7L@�7L@��@ܓu@�b@�b@�b@�S�@ڧ�@ڰ!@�n�@�1'@��@Չ7@Չ7@Չ7@�p�@�V@�O�@д9@υ@Χ�@�b@�"�@��y@ʏ\@�Q�@��
@�dZ@�\)@�l�@�|�@���@���@�p�@�`B@�`B@��@��`@��@ļj@�(�@�  @å�@�S�@���@�ȴ@°!@�v�@�=q@�@���@�7L@�%@�ƨ@�K�@�;d@��@�
=@�ȴ@�v�@���@�`B@��@���@�33@�M�@��@��j@�bN@�  @��m@�S�@�"�@��H@�~�@�v�@�^5@�J@���@��@���@�z�@�bN@�I�@�I�@��@��;@��
@�+@���@�-@�J@�M�@�@�X@���@��@���@��;@���@�|�@�t�@�l�@�v�@�{@��@�G�@��D@�(�@��@��F@���@�K�@�C�@�C�@�C�@�C�@�;d@��@��y@�v�@��@��T@��h@�p�@�&�@��`@��j@��u@�j@�  @���@�|�@�|�@�t�@�l�@��@��H@���@��T@��-@�x�@�X@���@��u@��m@�K�@��@���@�n�@�M�@�=q@��@�V@�9X@��
@�S�@�33@��@���@��@�ȴ@�5?@��T@���@�x�@�7L@�%@��@�I�@���@�l�@�K�@�;d@���@���@�V@�@�X@�7L@�V@�bN@�I�@��@���@��@�t�@�\)@�33@��y@���@�-@��h@�O�@�G�@�/@�&�@��@�V@���@�Ĝ@��@��u@�z�@�Z@�b@��@��w@���@�S�@���@���@�M�@�J@��#@���@���@�hs@�&�@��`@���@��@��D@�Q�@� �@��@�@�=q@��@���@��h@���@��j@�j@� �@�ƨ@��@�;d@��@��@��H@��@��@�^5@���@�p�@�7L@��@�%@���@��`@��j@���@��D@�j@��m@���@�K�@�"�@��@�~�@�5?@���@�@��h@�G�@�V@��j@��D@�r�@�Z@�A�@��;@��w@��F@��P@�dZ@��H@�ff@���@�hs@�V@��9@��@��D@�z�@�z�@�r�@�bN@�1'@�w@~��@}�T@|��@{�m@{t�@z��@zM�@y%@x�u@xA�@v��@v5?@u��@up�@u/@uV@t�/@t��@tz�@t�@t1@r�H@q��@q��@p�u@o�;@o�@o�P@o
=@n��@nff@n5?@n$�@m�h@l�/@lI�@k�
@k�@kC�@k"�@j�@j��@j~�@jn�@jM�@j-@i��@i��@i�^@i��@i��@h��@f��@f��@f�+@fff@fE�@f$�@e�@e�h@d�/@dz�@d�@c��@cƨ@c�@ct�@cS�@b�@b��@b^5@a��@ax�@`��@`r�@`Q�@`  @_|�@_;d@_;d@_;d@_+@_+@_�@_
=@^��@^�y@^�R@^@]�@\z�@\Z@\I�@[��@[��@[�m@[ƨ@[��@[��@[S�@["�@Z�@Z��@Z~�@Z^5@Z-@Y�#@YX@XĜ@X�@W�@W|�@Wl�@W\)@WK�@W;d@W�@W�@W�@W�@W�@W
=@V��@V�@V��@Vv�@V@T�j@T�@S��@S��@S33@S@R��@R~�@R-@Qhs@P�`@P �@O�P@O|�@Ol�@O+@N��@Nȴ@NE�@M�h@M�@LZ@L1@K�m@KS�@J��@J�\@J�\@Jn�@J=q@J=q@J�@J�@J�@JJ@JJ@I��@I��@I�^@I��@Ix�@I�@H��@H�9@HA�@G�P@Fȴ@E�-@Ep�@E`B@EO�@EO�@E/@D�/@D�@Dj@C�m@C��@CC�@B��@B~�@B=q@A�#@@�9@@Q�@@ �@?�@?��@?�@?�P@?\)@?;d@?
=@>�@>��@>E�@=�@=O�@<z�@<9X@<1@;��@;dZ@;C�@;33@;o@:�H@:��@:�@9G�@97L@9&�@8��@8�9@8b@7��@7\)@6��@6�R@6�R@6�R@65?@5�@5�h@4��@4�@4Z@3�m@3�F@3��@3��@3t�@3t�@3S�@333@3"�@3@2�@2��@2��@2n�@2=q@1�^@17L@0bN@/l�@.�@-�@-p�@-`B@-`B@-O�@-?}@-O�@-?}@-?}@-?}@-�@,��@,�/@,�D@,j@,I�@,(�@+��@+ƨ@+�F@+�@+S�@+33@+o@*�H@*��@*~�@*-@)��@)G�@)&�@(��@(A�@(b@'�@'�w@'�@'�@'�P@'�P@'|�@'K�@';d@&��@&�+@&ff@&5?@%�T@%�-@%p�@$��@$j@#S�@"-@!�^@!&�@ �u@ 1'@�w@l�@�@��@��@�y@�@�y@��@V@$�@@�T@@�-@�-@�h@p�@`B@/@V@�@�@o@��@�7@x�@�@�`@Ĝ@�u@A�@ �@�@l�@;d@+@+@;d@;d@;d@;d@+@�@+@��@��@�+@@�@��@��@�@`B@?}@?}@�@�@��@z�@j@9X@t�@@��@�H@��@�\@�\@~�@~�@n�@n�@n�@~�@~�@~�@~�@~�@n�@^5@�@��@��@hs@%@b@ �@b@  @�@��AӲ-AӶFAӸRA���A�ȴA���A�ĜA�v�A�r�A�l�A�jA�l�A�n�A�l�A�ffA�ffA�jA�jA�dZA�dZA�hsA�hsA�bNA�dZA�dZA�`BA�\)A�\)A�\)A�`BA�`BA�^5A�\)A�\)A�`BA�`BA�`BA�\)A�\)A�bNA�`BA�\)A�^5A�bNA�bNA�^5A�\)A�`BA�`BA�\)A�^5A�bNA�`BA�\)A�^5A�bNA�bNA�^5A�\)A�`BA�bNA�^5A�\)A�`BA�bNA�`BA�\)A�bNA�bNA�^5A�\)A�^5A�bNA�bNA�^5A�^5A�dZA�bNA�^5A�\)A�^5A�bNA�`BA�\)A�ZA�ZA�^5A�^5A�ZA�ZA�\)A�^5A�ZA�XA�ZA�\)A�^5A�ZA�XA�ZA�^5A�\)A�ZA�\)A�`BA�^5A�ZA�^5A�`BA�^5A�ZA�XA�XA�\)A�ZA�VA�VA�ZA�ZA�S�A�Q�A�Q�A�O�A�G�A�E�A�E�A�E�A�G�A�G�A�C�A�A�A�A�A�G�A�E�A�?}A�A�A�C�A�E�A�A�A�?}A�A�A�C�A�E�A�A�A�=qA�?}A�?}A�?}A�?}A�;dA�;dA�?}A�?}A�?}A�?}A�;dA�;dA�=qA�A�A�A�A�=qA�;dA�?}A�?}A�?}A�9XA�;dA�?}A�?}A�;dA�7LA�7LA�;dA�;dA�7LA�5?A�5?A�;dA�;dA�5?A�1'A�7LA�7LA�33A�/A�(�A��A�$�A�$�A�"�A��A��A� �A� �A�"�A��A��A��A��A��A��A��A��A�{A�bA�1A���A��yA��`A��HA��#A���A���A���A���A�ȴA�ĜAҾwAҺ^AҺ^AҺ^AҸRAҮAҡ�A�p�A�=qA��A��A���A�`BA��A���AЏ\A�|�A�`BA�O�A�7LA�1'A�+A�%A�ȴAϋDA�x�A�l�A�hsA�jA�bNA�XA�XA�XA�S�A�M�A�=qA�7LA�/A�-A�(�A��A�%A���A��A��Aδ9AΑhA�K�A�+A�+A�+A�(�A�&�A�&�A�$�A��A�JA�1A�1A�
=A�1A�A�A�  A�%A�1A�  A���A���A���A�  A�%A�  A�  A�A�1A�JA�bA�bA��A��A� �A��A��A��A��A��A��A��A�bA�JA�A���A��A���A�Aͺ^Aʹ9AͮAͧ�A͝�A͑hA�n�A�;dA�-A�(�A�"�A��A��A��A��A��A�bA�1A�%A�1A�%A�A���A���A���A���A��A��HA��A���A���A���A�ȴA̸RẠ�A̓uẢ7A�|�A�v�A�t�A�t�A�x�A�|�A�|�A�~�A�|�A�z�A�z�A�x�A�`BA�JA���A���A��A��A��yA��mA��;A���A�ƨA�ĜA�ƨA�ĜA�A���A˶FA˲-A˲-A˴9A˲-AˬA˥�Aˡ�A˟�Aˡ�Aˣ�Aˣ�A˟�A˝�A˙�A˗�A˗�A˕�AˑhAˏ\Aˇ+Aˉ7AˁA�p�A�jA�\)A�=qA�-A�%A��A��`A��A���A�ĜA���AʸRAʧ�Aʟ�Aʏ\AʋDAʋDAʉ7AʁA�t�A�r�A�jA�dZA�dZA�ffA�^5A�VA�O�A�G�A�;dA�7LA�1'A�$�A�{A�1A���A��TAɲ-Aɣ�Aɟ�AɑhAɅA�~�A�|�A�z�A�x�A�x�A�x�A�x�A�z�A�z�A�x�A�v�A�t�A�t�A�v�A�t�A�l�A�ffA�ZA�XA�S�A�VA�S�A�Q�A�M�A�5?A�1'A�&�A��A��A��A��A��A��A�{A�%A���A���A���A���A��/A��A��
A�ȴAȮA�p�Aȉ7AȅA�`BA�C�A�33A�/A�$�A���A��yA���Aǧ�AǙ�AǑhAǏ\AǍPAǋDAǋDAǋDAǉ7AǋDAǉ7AǇ+AǅA�z�A�\)A�ZA�\)A�ZA�ZA�VA�S�A�Q�A�S�A�S�A�S�A�Q�A�O�A�O�A�K�A�C�A�;dA�7LA�33A�33A�33A�"�A�{A�  A��/AƲ-A�p�A�A�ffAĺ^A�n�A�ZA�I�A�;dA�/A�(�A�"�A�"�A�"�A��A�{A�bA�VA�1A�A���A���A��A��A��A��;A��/A��A���A�Aß�AÁA�Q�A�A�A²-A�dZA�=qA�oA��TA���A��-A��PA��A�jA�XA�I�A�A�A�/A�JA��TA�ĜA���A�`BA�E�A�7LA�"�A�1A��mA��9A��uA��A�t�A�`BA�Q�A�7LA�{A��A�x�A�%A���A��+A�=qA�VA�%A�  A���A��A��mA��`A��HA��;A��/A��#A���A�ĜA�A��^A���A���A���A���A��DA�n�A�\)A�C�A�"�A���A��`A�A���A���A���A��A�z�A�v�A�t�A�n�A�jA�\)A��A�(�A��A�K�A�A�S�A�ĜA�bA�v�A�Q�A�7LA�"�A�oA�VA�A���A��A��mA��HA��#A�A��RA��-A��A���A��\A�r�A�C�A�1'A�/A�$�A��A�A��mA�A���A�v�A�G�A��HA��9A���A���A��A�`BA�1'A��FA��A��+A��A�|�A�v�A�n�A�`BA�XA�M�A�K�A�G�A�;dA� �A���A�ȴA��hA�Q�A��9A��A��RA�~�A�S�A�5?A��A�  A���A���A��A�ffA�"�A��A��A���A�l�A�33A�bA���A��A�ȴA��FA���A�ffA�+A�VA��A��FA�+A��hA�A�A�&�A� �A��A�A��yA��A���A���A���A��RA��^A��^A��9A��A���A���A�|�A�M�A�ƨA�G�A���A��A�E�A�9XA�7LA�7LA�5?A�&�A���A��`A���A���A�A��^A��FA��!A���A���A���A���A���A��hA�t�A�jA�ZA�M�A�VA���A��PA�\)A�1A��;A��#A���A��^A��-A��!A��A��A��!A��!A��A��!A��A���A���A���A���A���A���A��uA�ZA��
A��HA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                  A�AӰ!A�n�A�jA�hsA�ffA�dZA�\)A�^5A�^5A�`BA�`BA�^5A�`BA�`BA�^5A�`BA�\)A�\)A�ZA�\)A�\)A�XA�K�A�E�A�A�A�A�A�?}A�=qA�=qA�9XA�7LA�-A� �A��A�1A��A���A�l�A�1A�(�A�r�A�M�A��A΁A��A�A�A��A�A͕�A��A���A̾wA�z�A�"�A���AˮA˛�A�r�A��/AʅA�O�A��#A�z�A�jA�5?A�%Aȝ�A��yAǋDA�dZA�M�A�{A��A��A��`A��A�|�A�|�A�^5A�\)A���A�dZA��uA��DA�(�A��wA��A���A���A�+A���A�M�A���A�/A���A��!A�33A�bA��-A�ffA��A��!A���A�~�A�VA�=qA�x�A��A���A���A���A���A��
A�
=A�{A��A��A���A��A�?}A�-A���A�S�A��A�?}A��A���A�(�A~$�A{�-Av=qAu�At�uAs�Ap�!Akt�Ah�`AgK�Afn�AbJA\1A[?}AZ^5AW��AV=qAT �AP^5AO�TAOl�AOANz�AMhsAJ��AH��AG�#AG��AGK�AE��AC��A>1'A;�A:�`A:�jA9�A8 �A7K�A6�`A5��A4VA3��A3A3�7A2��A1��A1;dA.�A-�hA,�A+/A*��A)��A(�9A(5?A(9XA(1A'7LA&JA%�A#��A#S�A"Q�A!C�A r�A�Ap�A^5A+A�A|�Ar�A�#A�A�9Av�A(�A�wA(�A�A��A�A��AƨA�AffA$�A`BA/A
5?A	C�A	VA�HAȴA��A�!Ar�A��AI�A�-A+A �+@��;@�?}@�A�@��w@�|�@�;d@��+@��7@�V@��9@�z�@�A�@�1@��
@���@���@���@��@�j@�1'@�
=@�~�@�@�p�@��`@�D@�bN@�9X@��m@�F@�|�@�
=@�\@���@�`B@�u@�ȴ@�$�@噚@�Ĝ@�j@��@�hs@�u@�X@�G�@�@�|�@��/@ާ�@��@ݩ�@�7L@�7L@��@ܓu@�b@�b@�b@�S�@ڧ�@ڰ!@�n�@�1'@��@Չ7@Չ7@Չ7@�p�@�V@�O�@д9@υ@Χ�@�b@�"�@��y@ʏ\@�Q�@��
@�dZ@�\)@�l�@�|�@���@���@�p�@�`B@�`B@��@��`@��@ļj@�(�@�  @å�@�S�@���@�ȴ@°!@�v�@�=q@�@���@�7L@�%@�ƨ@�K�@�;d@��@�
=@�ȴ@�v�@���@�`B@��@���@�33@�M�@��@��j@�bN@�  @��m@�S�@�"�@��H@�~�@�v�@�^5@�J@���@��@���@�z�@�bN@�I�@�I�@��@��;@��
@�+@���@�-@�J@�M�@�@�X@���@��@���@��;@���@�|�@�t�@�l�@�v�@�{@��@�G�@��D@�(�@��@��F@���@�K�@�C�@�C�@�C�@�C�@�;d@��@��y@�v�@��@��T@��h@�p�@�&�@��`@��j@��u@�j@�  @���@�|�@�|�@�t�@�l�@��@��H@���@��T@��-@�x�@�X@���@��u@��m@�K�@��@���@�n�@�M�@�=q@��@�V@�9X@��
@�S�@�33@��@���@��@�ȴ@�5?@��T@���@�x�@�7L@�%@��@�I�@���@�l�@�K�@�;d@���@���@�V@�@�X@�7L@�V@�bN@�I�@��@���@��@�t�@�\)@�33@��y@���@�-@��h@�O�@�G�@�/@�&�@��@�V@���@�Ĝ@��@��u@�z�@�Z@�b@��@��w@���@�S�@���@���@�M�@�J@��#@���@���@�hs@�&�@��`@���@��@��D@�Q�@� �@��@�@�=q@��@���@��h@���@��j@�j@� �@�ƨ@��@�;d@��@��@��H@��@��@�^5@���@�p�@�7L@��@�%@���@��`@��j@���@��D@�j@��m@���@�K�@�"�@��@�~�@�5?@���@�@��h@�G�@�V@��j@��D@�r�@�Z@�A�@��;@��w@��F@��P@�dZ@��H@�ff@���@�hs@�V@��9@��@��D@�z�@�z�@�r�@�bN@�1'@�w@~��@}�T@|��@{�m@{t�@z��@zM�@y%@x�u@xA�@v��@v5?@u��@up�@u/@uV@t�/@t��@tz�@t�@t1@r�H@q��@q��@p�u@o�;@o�@o�P@o
=@n��@nff@n5?@n$�@m�h@l�/@lI�@k�
@k�@kC�@k"�@j�@j��@j~�@jn�@jM�@j-@i��@i��@i�^@i��@i��@h��@f��@f��@f�+@fff@fE�@f$�@e�@e�h@d�/@dz�@d�@c��@cƨ@c�@ct�@cS�@b�@b��@b^5@a��@ax�@`��@`r�@`Q�@`  @_|�@_;d@_;d@_;d@_+@_+@_�@_
=@^��@^�y@^�R@^@]�@\z�@\Z@\I�@[��@[��@[�m@[ƨ@[��@[��@[S�@["�@Z�@Z��@Z~�@Z^5@Z-@Y�#@YX@XĜ@X�@W�@W|�@Wl�@W\)@WK�@W;d@W�@W�@W�@W�@W�@W
=@V��@V�@V��@Vv�@V@T�j@T�@S��@S��@S33@S@R��@R~�@R-@Qhs@P�`@P �@O�P@O|�@Ol�@O+@N��@Nȴ@NE�@M�h@M�@LZ@L1@K�m@KS�@J��@J�\@J�\@Jn�@J=q@J=q@J�@J�@J�@JJ@JJ@I��@I��@I�^@I��@Ix�@I�@H��@H�9@HA�@G�P@Fȴ@E�-@Ep�@E`B@EO�@EO�@E/@D�/@D�@Dj@C�m@C��@CC�@B��@B~�@B=q@A�#@@�9@@Q�@@ �@?�@?��@?�@?�P@?\)@?;d@?
=@>�@>��@>E�@=�@=O�@<z�@<9X@<1@;��@;dZ@;C�@;33@;o@:�H@:��@:�@9G�@97L@9&�@8��@8�9@8b@7��@7\)@6��@6�R@6�R@6�R@65?@5�@5�h@4��@4�@4Z@3�m@3�F@3��@3��@3t�@3t�@3S�@333@3"�@3@2�@2��@2��@2n�@2=q@1�^@17L@0bN@/l�@.�@-�@-p�@-`B@-`B@-O�@-?}@-O�@-?}@-?}@-?}@-�@,��@,�/@,�D@,j@,I�@,(�@+��@+ƨ@+�F@+�@+S�@+33@+o@*�H@*��@*~�@*-@)��@)G�@)&�@(��@(A�@(b@'�@'�w@'�@'�@'�P@'�P@'|�@'K�@';d@&��@&�+@&ff@&5?@%�T@%�-@%p�@$��@$j@#S�@"-@!�^@!&�@ �u@ 1'@�w@l�@�@��@��@�y@�@�y@��@V@$�@@�T@@�-@�-@�h@p�@`B@/@V@�@�@o@��@�7@x�@�@�`@Ĝ@�u@A�@ �@�@l�@;d@+@+@;d@;d@;d@;d@+@�@+@��@��@�+@@�@��@��@�@`B@?}@?}@�@�@��@z�@j@9X@t�@@��@�H@��@�\@�\@~�@~�@n�@n�@n�@~�@~�@~�@~�@~�@n�@^5@�@��@��@hs@%@b@ �@b@  @�G�O�AӲ-AӶFAӸRA���A�ȴA���A�ĜA�v�A�r�A�l�A�jA�l�A�n�A�l�A�ffA�ffA�jA�jA�dZA�dZA�hsA�hsA�bNA�dZA�dZA�`BA�\)A�\)A�\)A�`BA�`BA�^5A�\)A�\)A�`BA�`BA�`BA�\)A�\)A�bNA�`BA�\)A�^5A�bNA�bNA�^5A�\)A�`BA�`BA�\)A�^5A�bNA�`BA�\)A�^5A�bNA�bNA�^5A�\)A�`BA�bNA�^5A�\)A�`BA�bNA�`BA�\)A�bNA�bNA�^5A�\)A�^5A�bNA�bNA�^5A�^5A�dZA�bNA�^5A�\)A�^5A�bNA�`BA�\)A�ZA�ZA�^5A�^5A�ZA�ZA�\)A�^5A�ZA�XA�ZA�\)A�^5A�ZA�XA�ZA�^5A�\)A�ZA�\)A�`BA�^5A�ZA�^5A�`BA�^5A�ZA�XA�XA�\)A�ZA�VA�VA�ZA�ZA�S�A�Q�A�Q�A�O�A�G�A�E�A�E�A�E�A�G�A�G�A�C�A�A�A�A�A�G�A�E�A�?}A�A�A�C�A�E�A�A�A�?}A�A�A�C�A�E�A�A�A�=qA�?}A�?}A�?}A�?}A�;dA�;dA�?}A�?}A�?}A�?}A�;dA�;dA�=qA�A�A�A�A�=qA�;dA�?}A�?}A�?}A�9XA�;dA�?}A�?}A�;dA�7LA�7LA�;dA�;dA�7LA�5?A�5?A�;dA�;dA�5?A�1'A�7LA�7LA�33A�/A�(�A��A�$�A�$�A�"�A��A��A� �A� �A�"�A��A��A��A��A��A��A��A��A�{A�bA�1A���A��yA��`A��HA��#A���A���A���A���A�ȴA�ĜAҾwAҺ^AҺ^AҺ^AҸRAҮAҡ�A�p�A�=qA��A��A���A�`BA��A���AЏ\A�|�A�`BA�O�A�7LA�1'A�+A�%A�ȴAϋDA�x�A�l�A�hsA�jA�bNA�XA�XA�XA�S�A�M�A�=qA�7LA�/A�-A�(�A��A�%A���A��A��Aδ9AΑhA�K�A�+A�+A�+A�(�A�&�A�&�A�$�A��A�JA�1A�1A�
=A�1A�A�A�  A�%A�1A�  A���A���A���A�  A�%A�  A�  A�A�1A�JA�bA�bA��A��A� �A��A��A��A��A��A��A��A�bA�JA�A���A��A���A�Aͺ^Aʹ9AͮAͧ�A͝�A͑hA�n�A�;dA�-A�(�A�"�A��A��A��A��A��A�bA�1A�%A�1A�%A�A���A���A���A���A��A��HA��A���A���A���A�ȴA̸RẠ�A̓uẢ7A�|�A�v�A�t�A�t�A�x�A�|�A�|�A�~�A�|�A�z�A�z�A�x�A�`BA�JA���A���A��A��A��yA��mA��;A���A�ƨA�ĜA�ƨA�ĜA�A���A˶FA˲-A˲-A˴9A˲-AˬA˥�Aˡ�A˟�Aˡ�Aˣ�Aˣ�A˟�A˝�A˙�A˗�A˗�A˕�AˑhAˏ\Aˇ+Aˉ7AˁA�p�A�jA�\)A�=qA�-A�%A��A��`A��A���A�ĜA���AʸRAʧ�Aʟ�Aʏ\AʋDAʋDAʉ7AʁA�t�A�r�A�jA�dZA�dZA�ffA�^5A�VA�O�A�G�A�;dA�7LA�1'A�$�A�{A�1A���A��TAɲ-Aɣ�Aɟ�AɑhAɅA�~�A�|�A�z�A�x�A�x�A�x�A�x�A�z�A�z�A�x�A�v�A�t�A�t�A�v�A�t�A�l�A�ffA�ZA�XA�S�A�VA�S�A�Q�A�M�A�5?A�1'A�&�A��A��A��A��A��A��A�{A�%A���A���A���A���A��/A��A��
A�ȴAȮA�p�Aȉ7AȅA�`BA�C�A�33A�/A�$�A���A��yA���Aǧ�AǙ�AǑhAǏ\AǍPAǋDAǋDAǋDAǉ7AǋDAǉ7AǇ+AǅA�z�A�\)A�ZA�\)A�ZA�ZA�VA�S�A�Q�A�S�A�S�A�S�A�Q�A�O�A�O�A�K�A�C�A�;dA�7LA�33A�33A�33A�"�A�{A�  A��/AƲ-A�p�A�A�ffAĺ^A�n�A�ZA�I�A�;dA�/A�(�A�"�A�"�A�"�A��A�{A�bA�VA�1A�A���A���A��A��A��A��;A��/A��A���A�Aß�AÁA�Q�A�A�A²-A�dZA�=qA�oA��TA���A��-A��PA��A�jA�XA�I�A�A�A�/A�JA��TA�ĜA���A�`BA�E�A�7LA�"�A�1A��mA��9A��uA��A�t�A�`BA�Q�A�7LA�{A��A�x�A�%A���A��+A�=qA�VA�%A�  A���A��A��mA��`A��HA��;A��/A��#A���A�ĜA�A��^A���A���A���A���A��DA�n�A�\)A�C�A�"�A���A��`A�A���A���A���A��A�z�A�v�A�t�A�n�A�jA�\)A��A�(�A��A�K�A�A�S�A�ĜA�bA�v�A�Q�A�7LA�"�A�oA�VA�A���A��A��mA��HA��#A�A��RA��-A��A���A��\A�r�A�C�A�1'A�/A�$�A��A�A��mA�A���A�v�A�G�A��HA��9A���A���A��A�`BA�1'A��FA��A��+A��A�|�A�v�A�n�A�`BA�XA�M�A�K�A�G�A�;dA� �A���A�ȴA��hA�Q�A��9A��A��RA�~�A�S�A�5?A��A�  A���A���A��A�ffA�"�A��A��A���A�l�A�33A�bA���A��A�ȴA��FA���A�ffA�+A�VA��A��FA�+A��hA�A�A�&�A� �A��A�A��yA��A���A���A���A��RA��^A��^A��9A��A���A���A�|�A�M�A�ƨA�G�A���A��A�E�A�9XA�7LA�7LA�5?A�&�A���A��`A���A���A�A��^A��FA��!A���A���A���A���A���A��hA�t�A�jA�ZA�M�A�VA���A��PA�\)A�1A��;A��#A���A��^A��-A��!A��A��A��!A��!A��A��!A��A���A���A���A���A���A���A��uA�ZA��
A��HA���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
h>B
jB
h>B
h>B
h
B
h
B
hsB
iB
h
B
iDB
h�B
iB
jB
i�B
i�B
jKB
i�B
kQB
kB
l"B
k�B
m�B
p�B
r|B
tB
u�B
wfB
y�B
zB
z�B
}�B
}�B
}"B
z�B
{B
� B
~�B
{JB
x�B
|PB
�PB
�"B
�B
�lB
�B
��B
�qB
��B
�hB
�zB
��B
��B
��B
�XB
��B
�?B
��B
�HB
��B
��B
��B
��B
�)B
�vB
�B
��B
�2B�B5?BX�B`BBg�BkBrB�kB��B�B�B��B�B�$BѷB��B�
B�`B�`B�BٴB��B��BٴB�B�;B�B�2BܒB�<BʌB�BB�-B�UB��B�tB��B�EBԕB��B��B�MBu�B]�BZ�BZ�BZBTaBJXB@�B*0B%FB+BB
�GB
��B
��B
�kB
��B
w2B
a|B
^jB
W
B
A B
:*B
�B
SB	�cB	�B	��B	�B	�KB	��B	�B	��B	w�B	m)B	gB	e�B	W�B	T�B	K)B	FB	EB	D3B	EmB	B�B	<�B	1'B	,=B	)_B	&LB	IB	�B	B��B��B�B�5B�/B�B�B�yB�B��B�B�B�,B��B��B��B��B�sB՛B��B�[B�0B�6B�,B�yB��B��BʌB�B��B�}B�HB��B��B�B�NB�aB�yB�#BٴB�9BרB�QBیB��BںB��B�B�B�B�5B�B��B�#B�QB�pB��BߤB��B�#BچB��B�/B�|B�+B��B�>B�|B�B�B��B�
B�B�"B��B��B�oB�B��B��B�B��B��B��B�xB��B�VB��B�"B��B��B�xB�8B�2B��B��B��B��B��B��B��B�	B�VB	AB	AB	�B	~B	\B	.B	.B	�B	�B	�B	CB	#:B	$B	#nB	�B	qB	:B	bB	\B	(B	B	�B	�B	�B	_B	�B	�B	~B	 �B	$B	$�B	�B	�B	'B	)�B	-B	49B	1�B	49B	2�B	9$B	>BB	<B	:�B	;�B	<6B	<�B	?�B	@�B	C�B	HB	M�B	NB	OBB	PHB	T�B	UgB	Y�B	\)B	^5B	d&B	ffB	h�B	jB	m)B	m�B	n�B	oiB	p;B	p�B	q�B	q�B	q�B	z�B	|�B	}VB	}�B	�B	��B	��B	��B	�7B	��B	�bB	��B	��B	��B	�B	�xB	��B	�B	��B	��B	�wB	��B	�aB	�hB	��B	�B	�FB	�RB	�$B	��B	��B	��B	��B	�wB	��B	�UB	��B	ƨB	�EB	�)B	͟B	��B	�pB	��B	�HB	��B	бB	�B	�B	��B	�aB	ӏB	��B	�9B	��B	��B	خB	��B	�QB	�/B	��B	�pB	�;B	�;B	�B	�BB	�B	��B	�ZB	�&B	��B	��B	�fB	�B	�B	��B	��B	�B	�)B	�cB	� B	�5B	�B	��B	�|B	�MB	��B	�B	��B	��B	�`B	��B	��B	�rB	�B	�xB	�B	��B	��B	�JB	��B	��B	��B	�(B	�.B
  B
 iB
 iB
 �B
B
AB
�B
�B
B
GB
B
�B
�B
�B
�B
�B
SB
�B
�B
�B
1B
�B
fB
	�B
	B

rB

�B

�B

�B

�B
B
DB
�B
�B
"B
�B
�B
"B
�B
"B
"B
"B
�B
�B
(B
(B
�B
�B
bB
bB
�B
4B
uB
�B
MB
�B
�B
�B
�B
�B
B
SB
�B
�B
�B
�B
�B
YB
�B
7B
�B
�B
=B
CB
xB
�B
B
�B
OB
�B
�B
�B
�B
�B
�B
VB
�B
�B
 \B
 \B
 �B
 �B
!-B
!�B
!�B
!�B
!�B
#B
#:B
#�B
#�B
$B
$�B
%FB
%zB
%�B
&LB
&�B
&�B
'RB
'�B
'RB
'�B
'�B
(XB
(XB
($B
'�B
'�B
(�B
)�B
*�B
+kB
,qB
,�B
,�B
-B
-B
-B
-B
-B
-wB
-�B
-�B
.�B
/OB
/�B
0�B
0�B
0�B
2aB
1�B
1�B
2�B
33B
3hB
3�B
4B
3�B
3�B
3�B
3�B
3�B
33B
5B
5tB
5�B
6FB
5�B
5�B
5�B
6FB
6FB
6FB
6B
6B
6zB
7LB
7�B
7�B
8RB
8RB
8RB
8�B
8�B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:*B
9�B
;�B
;�B
;dB
;dB
;0B
;0B
;0B
;dB
;�B
<6B
<jB
<�B
<�B
<�B
=B
=<B
=<B
=�B
=�B
>B
>wB
>�B
?HB
?�B
?HB
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A B
@�B
@�B
@�B
AUB
B'B
C�B
C�B
D3B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E9B
EmB
EmB
E�B
E�B
E�B
FB
F�B
GB
GEB
G�B
HKB
HKB
HB
HKB
HKB
HKB
HKB
H�B
HKB
H�B
HKB
HKB
HKB
H�B
HKB
H�B
J�B
J�B
J�B
J�B
K)B
K)B
K^B
K^B
K�B
L0B
L�B
M6B
M6B
MjB
MB
MjB
M6B
MjB
NB
N<B
N�B
O�B
O�B
OvB
PB
P�B
P�B
P�B
P�B
QB
P�B
QNB
QB
QB
P�B
P�B
QB
QB
QNB
QNB
QNB
Q�B
Q�B
Q�B
R B
R�B
S�B
T�B
TaB
T�B
TaB
TaB
T�B
T�B
T�B
T�B
UgB
U�B
VB
V9B
V9B
VmB
V�B
XB
XB
XB
XEB
XEB
XEB
XyB
X�B
X�B
X�B
X�B
X�B
YKB
YKB
ZB
ZB
ZQB
Z�B
Z�B
[#B
[#B
[#B
[WB
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]dB
^jB
^jB
_B
_pB
_pB
_;B
_;B
_�B
_�B
`B
`vB
`vB
`�B
aB
aB
aHB
aHB
aHB
a|B
a|B
a|B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bB
b�B
c�B
c�B
e,B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
e�B
ffB
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
gmB
g�B
g�B
g�B
g�B
h
B
hsB
h�B
iB
iB
i�B
i�B
jB
jB
jB
jKB
jB
jB
j�B
jB
j�B
j�B
j�B
k�B
kQB
k�B
k�B
k�B
k�B
l�B
l�B
m�B
o5B
n�B
o�B
pB
poB
p�B
p�B
qvB
qvB
qvB
qvB
qvB
qvB
rB
rB
rGB
rGB
r|B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
uZB
v+B
v+B
v+B
v�B
v�B
v�B
v�B
wfB
wfB
w�B
xlB
xlB
xlB
xlB
xlB
xlB
x�B
xlB
x�B
x�B
x8B
y>B
x�B
y	B
y�B
y�B
zB
y�B
y�B
zDB
zDB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
|B
|�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}VB
}�B
}�B
�B
~�B
~�B
~�B
cB
� B
aB
gmB
j�B
jB
jB
iB
l�B
hsB
i�B
h
B
hsB
h�B
f�B
gmB
h�B
iB
g�B
gmB
h�B
h�B
gmB
h
B
iyB
h
B
g�B
g�B
iyB
i�B
h�B
gmB
gmB
iB
jB
i�B
h>B
gmB
h>B
jB
i�B
h>B
iDB
jKB
iyB
hsB
hsB
jB
j�B
iDB
i�B
kB
jB
h�B
iyB
j�B
jB
h�B
iyB
j�B
kB
i�B
iB
jKB
j�B
iyB
iDB
jB
j�B
h�B
iB
jB
kQB
j�B
iDB
iyB
j�B
j�B
h�B
iDB
kQB
l"B
kB
i�B
j�B
k�B
lWB
k�B
i�B
j�B
l"B
l"B
j�B
j�B
k�B
l�B
l�B
kQB
j�B
k�B
l�B
l"B
kB
l"B
l�B
k�B
kB
k�B
m)B
l�B
k�B
m]B
n�B
o5B
o�B
n�B
oiB
qvB
q�B
pB
poB
rGB
r�B
q�B
qAB
rB
r�B
s�B
sB
sMB
r�B
tTB
uZB
u�B
s�B
tB
v`B
u�B
t�B
t�B
u�B
v�B
v`B
uZB
t�B
v�B
x8B
xlB
y>B
x�B
y�B
z�B
y�B
x�B
y	B
y>B
y>B
z�B
{B
zDB
x�B
x�B
zxB
z�B
y�B
zDB
zDB
|PB
{B
zDB
zxB
{�B
~(B
}�B
|�B
|�B
~�B
~�B
~(B
{�B
}"B
~]B
.B
|�B
|PB
|�B
}�B
~�B
|�B
z�B
z�B
{B
{�B
{�B
z�B
y�B
y�B
{JB
{�B
{B
zxB
zB
{�B
}�B
~]B
}�B
~�B
�;B
�uB
��B
�iB
}�B
}�B
~�B
~�B
~]B
|�B
{B
{B
|B
{�B
zxB
y	B
w�B
x�B
zDB
}�B
z�B
v`B
p�B
tB
�B
xlB
�oB
��B
�;B
��B
��B
��B
�VB
��B
�uB
��B
�B
��B
�JB
�rB
�~B
��B
��B
��B
�JB
�B
�~B
�xB
�VB
�xB
��B
��B
�lB
��B
��B
��B
��B
�B
�bB
��B
�B
�FB
��B
��B
�CB
�CB
��B
�~B
�B
�	B
��B
�1B
��B
�	B
��B
�	B
��B
�xB
��B
��B
��B
��B
�OB
�4B
��B
�tB
�B
��B
�FB
��B
�kB
�IB
��B
��B
��B
��B
��B
��B
�RB
�B
�B
�RB
�B
��B
�FB
�'B
��B
�'B
��B
�wB
��B
��B
��B
��B
��B
�_B
��B
�nB
�nB
��B
��B
�-B
��B
��B
��B
�4B
��B
�-B
��B
��B
��B
��B
��B
�B
�tB
�LB
��B
��B
��B
�RB
�XB
��B
�B
��B
�XB
�_B
��B
�0B
�qB
�kB
��B
�kB
�CB
�B
��B
�IB
�B
�nB
�B
�FB
�B
��B
��B
�tB
��B
��B
�B
��B
��B
��B
��B
�LB
��B
��B
�jB
�<B
��B
�<B
�}B
��B
�aB
�tB
��B
ŢB
�mB
ǮB
ɆB
��B
ɆB
�B
��B
ȀB
�RB
ɺB
ǮB
�EB
�?B
�EB
��B
��B
�)B
�^B
�^B
�dB
�vB
�pB
�vB
��B
ΥB
ϫB
͟B
�B
�pB
�6B
�B
�dB
�<B
��B
��B
��B
�XB
��B
˒B
�XB
�^B
�6B
��B
��B
ɺB
�jB
��B
ɺB
��B
��B
��B
��B
�TB
�B
خB
רB
��B
�B
�WB
��B
�#B
�WB
�#B
��B
��B
ݘB
�5B
�5B
ݘB
��B
��B
�B
�B
�sB
�B
�KB
�KB
�B
��B
��B
�DB
�PB
�cB�B�BGBMB{B�B�B!�B%FB$B%FB(�B)_B)�B0�B2aB/OB0�BB�BJ�BN<BPBP�BS�BZQBVmB\�B]�B_BaB`�BaBa|B`vB`B`BB_;B_;B_�B`�Bb�BjBi�Bi�Bi�Bi�Bj�Bk�Bl"Bj�Bj�BjKBjBi�BjBjBm)Bm�BncBm�Bm�Bn�Bs�Bq�Bu�Bx�B{B�iB��B�XB��B��B��B��B�IB�B��B��B�=B��B��B�=B��B��B�B��B�7B��B�B�qB�CB��B��B�B��B��B��B��B��B�B�0B��B�tB�*B�$B�nB�B��B��B�'B��B�!B�!B�-B��B��B��B��B��B�B�qB�B�!B�-B��B�B��B��B��B��B��B��B�[B�0B��B��B˒B�B��B��B��B�mB�sB�B�B��BچB�#B�WB��B�5BޞB�BB�B� B�ZB��B�8B�B�mB��B�QB�)B�B��B��B��B��B��B�B�B�|B�NB�pB��B�B��B�fB�/BںB�B�dB�B�BB�2B�B�B��B�yBרB��BרB�B�yB�B�BB�yB�EB�BچB�WB�/B�pB�B�yB�yBخB��B�BB��BרB�dB��B��BںB�9B��B�
B֡B��B�B��B��B�?B֡B��B�mBרB�gB�9B�yBרBרB�#BܒB��BߤB�5B�WB��B�B��B��B�QB��B�EB�QB��B��B�QB�QB��B��B��B�EB��B�yB�&B��B�aB��BуB�KB҉B� B�,B��B�mB@B��BϫB�pB�BбB͟B��B��B�6B�0B͟BɆBɆB��B�#BɺB�0B��B�6B�yB��B�vB��B��B�-BB� B��B��B�tB��B��B�-B��B��B��B��BB��B��B�B�OB��B�aB�qB�B�BĜBɆBŢBÖB��B�BÖB�KB��B�jB�6B�B�0B�)BʌB�#B��B��B��BȴB�tBŢB�BĜB��B��B��B�WBѷB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202007072154312020070721543120200707215431202007072154312020070721543120200707215431SI  SI  ARFMARFM                                                                                                                                                2020052322361220200523223612IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020060300004720200603000047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020060300004720200603000047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020070612390520200706123905IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200707214333              CF      PSAL                            ?�  G�O�D�g�G�O�?�  G�O�Bad PSAL Drift                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20200707214830    CF                  PSAL            G�O�>�G�O�CH�G�O�?�                  Bad PSAL Drift  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                