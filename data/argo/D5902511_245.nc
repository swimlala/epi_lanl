CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2023-01-16T00:04:07Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20230116000407  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_245                 6810_008521_245                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�|۵��@�|۵��11  @�}��v@�}��v@2���[W?@2���[W?�d����[-�d����[-11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@G�@�  @�  @�  @�p�@�(�A�RA ��A+�A>�RA_\)A�  A�  A��A��A�Q�AϮA�  A�A��B(�B(�B(�B   B'�
B0  B8Q�B@Q�BG�
BO�
BW�
B`  Bh  Bo�
Bx  B�{B��B��
B��B�{B�{B�  B�  B��B��B��
B��B�{B�{B�{B�  B��B�{B�{B�  B�  B�  B�  B�  B��B��
B�  B�{B��B�  B�=qB�  C   C��C��C�C��C

=C  C
=C
=C  C�C��C��C��C  C  C�C!��C$
=C&  C(  C*  C,  C.
=C0  C2  C4
=C6
=C8
=C:
=C<
=C=��C@  CA��CC�CE��CG��CI��CL  CN  CO��CQ��CT  CV  CX
=CZ
=C\  C^  C`
=Cb
=Cc��Ce�HCg��Cj  Cl  Cm��Cp
=Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~
=C�  C�C�C�C�  C���C�C�  C�  C���C�  C�C�  C���C�C�  C���C�  C�  C���C�  C�C�C�
=C�C�C�
=C�
=C�  C�C���C�C�C���C���C���C���C�  C�  C�  C�  C�C�  C�  C���C���C���C���C�  C�  C���C���C�C�C�  C���C�  C�
=C�  C���C�C�  C���C���C���C�  C�  C���C�C�  C�  C�  C�C���C�  C�  C�C�  C�  C�
=C�C�  C�  C�C�  C���C���C���C���C���C�  C�
=C�  C���C�  C�  C���C���C�  C�  C�C�\C�
=C�C�C�C�C�C�  C���C���C�
=C�C���C���C���C�  C�C�  C���C���C���C�C�C�C�  C���C�D �D � D  D� D�D��D  D}qD��D� D  D}qD�D��D  D}qD  D�D	�D	� D
  D
��D  D}qD  D�DD��DD��D  D� D�D� D�D�DD��D  D� D�D��D�D��DD��D  D��D  D}qD  D��D  Dz�D�qD� D  D�D�D� D  D� D�D� D�qD � D!D!��D"�D"��D#  D#}qD#�qD$��D%�D%��D&D&� D&�qD'�D(�D(}qD(�qD)}qD)�qD*� D+  D+��D,�D,}qD,�qD-� D-�qD.}qD/�D/��D0�D0��D1  D1� D2  D2� D2�qD3� D4  D4}qD5  D5� D6�D6� D6�qD7� D8  D8� D8��D9� D:�D:� D;�D;��D<  D<� D=  D=� D>  D>� D?  D?z�D?�qD@� DA�DA� DB�DB�DC�DC��DD  DD}qDE  DE� DF  DF}qDF�qDG��DG�qDHz�DH��DI� DJ  DJz�DJ��DK}qDL  DL� DM  DM��DN  DN}qDN�qDO� DP�DP��DQ  DQ��DR�DR}qDR�qDS� DT  DT��DU�DU��DV�DV� DW  DW� DW�qDX��DY�DY� DY�qDZ� D[�D[��D\  D\� D]�D]��D^�D^� D_  D_}qD_��D`z�D`�qDa}qDa�qDb}qDc  Dc� Dd�Dd�De�De�De�qDf}qDf�qDg}qDg�qDh� Di�Di�DjDj}qDj�qDk� Dk�qDl� DmDm��Dn  Dn}qDn�qDo��Dp�Dp��Dq�Dq� Dq�qDr��DsDs� Ds��Dt}qDu  Du� Du�qDvz�Dw  Dw��Dx�Dx}qDx�RDyxRDy�qDz� D{�D{�D|�D|� D|�qD}z�D}�qD~z�D~�qD� D��D�AHD�~�D���D�HD�@ D�~�D�D��D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D�}qD�� D�  D�=qD�~�D��HD�  D�@ D��HD��HD���D�=qD�~�D���D��qD�@ D�~�D���D���D�>�D�}qD���D�  D�AHD��HD���D���D�@ D�}qD��HD�HD�=qD�}qD��)D��)D�>�D�� D�� D�  D�AHD���D�� D�HD�B�D��HD�� D��qD�>�D��HD��HD�  D�@ D�� D���D�HD�AHD�~�D��qD���D�@ D��HD���D���D�@ D��HD���D��qD�>�D�~�D��HD�  D�>�D�� D���D�  D�AHD�~�D��HD��D�@ D�~�D��qD��)D�@ D�� D���D���D�>�D�� D���D�  D�@ D�� D��HD���D�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D���D��qD�>�D�~�D��qD��qD�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D���D���D�@ D�� D��HD�HD�AHD�� D���D���D�@ D�� D��HD��D�@ D�� D��HD�  D�>�D�~�D��HD�HD�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD�� D�� D���D�@ D�� D���D���D�>�D�}qD�� D�  D�>�D�� D�� D�  D�@ D��HD���D���D�>�D�}qD��HD��D�AHD�� D���D�  D�B�D��HD��HD�HD�@ D�~�D���D�  D�>�D�}qD���D�  D�@ D�~�D���D��qD�=qD�� D��HD���D�@ D�� D�� D���D�=qD�� D�� D�  D�B�D��HD��HD�HD�@ D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�}qD�� D�HD�@ DHD�� D��qD�@ DÂ�D�� D�  D�AHDāHD�� D�HD�@ Dŀ Dž�D���D�AHDƂ�D�� D���D�@ Dǀ D�� D�HD�>�D�}qD�� D�HD�AHD�~�DɽqD�  D�B�DʁHD��HD�  D�AHDˁHD��HD�HD�AHD́HD�� D���D�>�D̀ D��HD�HD�@ D΀ D��HD�HD�>�D�}qDϾ�D�  D�@ D�~�D�� D���D�>�Dр DѾ�D���D�>�D�}qDҽqD���D�@ DӁHDӾ�D���D�=qD�~�D�� D���D�>�DՂ�Dվ�D���D�@ Dր D�� D�HD�@ D�}qD�� D�HD�AHD؁HDؾ�D���D�@ DفHD�� D�  D�@ Dڀ D�� D���D�AHDہHD��HD�HD�>�D�~�D�� D�  D�AHD݀ Dݾ�D�  D�AHD�~�D�� D��D�B�D߀ D߾�D��qD�@ D�� D�� D�  D�@ D�HD��HD�  D�@ D�HD�D�HD�>�D�}qD�� D�HD�AHD� D�� D���D�>�D� D徸D���D�AHD悏D�� D���D�AHD�HD�� D�  D�AHD� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D꾸D�  D�AHD�HD�� D���D�>�D�~�D쾸D�  D�@ D�~�D��qD��)D�=qD�}qDD�  D�AHDD�D��D�AHD�~�D�� D�HD�AHD�HD��HD�  D�AHD� D�� D���D�>�D�HD�� D�HD�AHD�}qD�� D�HD�@ D�� D���D���D�>�D�~�D�� D��D�B�D�~�D���D���>u>���?W
=?�=q?�33?�@�@�R@:�H@J=q@^�R@z�H@��@���@�p�@��@�\)@�(�@��
@У�@�p�@�\@��@��HA�AQ�A�AG�A
=A�HA!G�A%A)��A0��A5A:=qA@��AFffAI��AP��AVffAZ�HAaG�Ag
=Ak�AqG�Aw�Az�HA���A��A�A�G�A��A�A�G�A�33A�A�G�A�33A�ffA�G�A�33A��RA�G�A��A�
=A���A��
A�ffA���A��A�ffA�Q�A��
A�{A�  A˅A�A�  AӅA�A�Q�AۅA�A��A�A�A��A�A�p�A��A�33A��A�Q�A��HA��B Q�B��B�RBz�BB�HB��B	�B
=B��B{B33B��B=qB\)B��B�\B�BG�B�RB�
B�B�HB   B!p�B#
=B$  B%��B&�HB(  B)��B+33B,(�B-B/33B0(�B1B3
=B4z�B6{B7\)B8z�B:{B;�B<z�B>=qB?�B@��BB�\BC�BD��BF�RBH  BH��BJ�RBL(�BMG�BN�HBPQ�BQG�BR�HBTz�BUBW
=BX��BY�B[33B\��B^{B_�BaG�Bb=qBc�
Bep�BfffBh  Bi��Bj�RBl(�BmBo
=Bp��Br{Bs33Bt��BvffBw�Bx��Bz�\B{�B}G�B~�HB�{B���B��B�(�B���B��B�=qB��HB�B�ffB�
=B��B���B�33B�{B��RB�G�B�=qB��RB�p�B�=qB���B��B�(�B���B��B�=qB���B��
B�z�B���B��
B��\B�
=B�B���B��B�B��\B�G�B��B���B�\)B�{B��HB��B�(�B�
=B�B�Q�B���B��
B�z�B�
=B��B�z�B��B�  B��\B�33B�  B�z�B��B�  B���B�33B��B���B�33B��B���B�G�B�B���B�\)B�B��\B�G�B�B�z�B�G�B�B��\B�\)B��
B��\B�p�B��B��\B�p�B�  B���B�p�B�=qB���B�p�B�Q�B���B�p�B�(�B�
=Bř�B�(�B���BǮB�(�B���Bə�B�{B��HB˙�B�  B���BͅB�  B��HBυB�  BиRBљ�B�(�BҸRBӅB�=qB���B�p�B�Q�B��HB�p�B�Q�B��HB�\)B�=qB��HB�p�B�  B���B݅B�  B޸RB߅B�(�B��B�p�B�(�B�\B�\)B�(�B��B�33B�  B�RB�33B��B��B�\)B��
B��B�G�B�B�\B�\)B��
B�z�B�G�B�  B��B�33B�  B�RB�G�B��B���B�\)B��B���B�\)B��B��RB�p�B�  B��\B�\)B�(�B��RB�G�B�(�B��HB�p�C   C ffC C
=C\)C��C
=C\)CC
=CQ�C�RC�C\)C�C�Cp�CC
=Cp�C�
C�CffC�
C33Cz�C��C	33C	��C	�HC
(�C
��C
�HC=qC��C�HCG�C�C�CG�C�C�C=qC�C��CG�CC  CffC��C{CffC�
C�C�C�C(�C��C  CG�C��C{Cp�CC�C�\C�C33C��C  CQ�C��C  Cp�C��C{C�C�C=qC�\C  CffCC
=Cp�C�
C=qC�\C�HC33C��C��C =qC ��C!  C!Q�C!��C"{C"p�C"C#{C#ffC#��C$33C$�\C$C%{C%p�C%�
C&{C&Q�C&�C'
=C'G�C'z�C'��C(�C(ffC(�\C(��C)(�C)\)C)�C)��C*{C*33C*ffC*�C*��C+33C+Q�C+��C+�
C,�C,Q�C,z�C,�C,�C-33C-z�C-�RC-�C.�C.ffC.�C.�HC/{C/Q�C/��C/�HC0�C0G�C0z�C0�RC1
=C1G�C1p�C1��C1��C233C2ffC2�\C2��C3{C3Q�C3�\C3C3�C433C4z�C4�C4�HC5{C5=qC5�C5��C6
=C633C6\)C6��C6�HC7{C7=qC7p�C7�C7��C8{C8G�C8�\C8C8�HC9{C9ffC9��C9�RC9�C:33C:p�C:�\C:�RC;  C;33C;ffC;��C;��C;��C<{C<Q�C<�\C<��C=  C=�C=G�C=�C=C>  C>=qC>\)C>�\C>C?
=C?G�C?z�C?��C?��C@{C@Q�C@�C@��C@�
CA
=CA=qCAz�CA�RCA�CB�CB=qCBp�CB��CB�HCC�CCQ�CCffCC�CC�CD(�CDQ�CDp�CD��CD�CE(�CEG�CEp�CE��CE�HCF�CF=qCFffCF��CF�
CG
=CG33CGQ�CG�\CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                   ?��@�\@G�@�  @�  @�  @�p�@�(�A�RA ��A+�A>�RA_\)A�  A�  A��A��A�Q�AϮA�  A�A��B(�B(�B(�B   B'�
B0  B8Q�B@Q�BG�
BO�
BW�
B`  Bh  Bo�
Bx  B�{B��B��
B��B�{B�{B�  B�  B��B��B��
B��B�{B�{B�{B�  B��B�{B�{B�  B�  B�  B�  B�  B��B��
B�  B�{B��B�  B�=qB�  C   C��C��C�C��C

=C  C
=C
=C  C�C��C��C��C  C  C�C!��C$
=C&  C(  C*  C,  C.
=C0  C2  C4
=C6
=C8
=C:
=C<
=C=��C@  CA��CC�CE��CG��CI��CL  CN  CO��CQ��CT  CV  CX
=CZ
=C\  C^  C`
=Cb
=Cc��Ce�HCg��Cj  Cl  Cm��Cp
=Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~
=C�  C�C�C�C�  C���C�C�  C�  C���C�  C�C�  C���C�C�  C���C�  C�  C���C�  C�C�C�
=C�C�C�
=C�
=C�  C�C���C�C�C���C���C���C���C�  C�  C�  C�  C�C�  C�  C���C���C���C���C�  C�  C���C���C�C�C�  C���C�  C�
=C�  C���C�C�  C���C���C���C�  C�  C���C�C�  C�  C�  C�C���C�  C�  C�C�  C�  C�
=C�C�  C�  C�C�  C���C���C���C���C���C�  C�
=C�  C���C�  C�  C���C���C�  C�  C�C�\C�
=C�C�C�C�C�C�  C���C���C�
=C�C���C���C���C�  C�C�  C���C���C���C�C�C�C�  C���C�D �D � D  D� D�D��D  D}qD��D� D  D}qD�D��D  D}qD  D�D	�D	� D
  D
��D  D}qD  D�DD��DD��D  D� D�D� D�D�DD��D  D� D�D��D�D��DD��D  D��D  D}qD  D��D  Dz�D�qD� D  D�D�D� D  D� D�D� D�qD � D!D!��D"�D"��D#  D#}qD#�qD$��D%�D%��D&D&� D&�qD'�D(�D(}qD(�qD)}qD)�qD*� D+  D+��D,�D,}qD,�qD-� D-�qD.}qD/�D/��D0�D0��D1  D1� D2  D2� D2�qD3� D4  D4}qD5  D5� D6�D6� D6�qD7� D8  D8� D8��D9� D:�D:� D;�D;��D<  D<� D=  D=� D>  D>� D?  D?z�D?�qD@� DA�DA� DB�DB�DC�DC��DD  DD}qDE  DE� DF  DF}qDF�qDG��DG�qDHz�DH��DI� DJ  DJz�DJ��DK}qDL  DL� DM  DM��DN  DN}qDN�qDO� DP�DP��DQ  DQ��DR�DR}qDR�qDS� DT  DT��DU�DU��DV�DV� DW  DW� DW�qDX��DY�DY� DY�qDZ� D[�D[��D\  D\� D]�D]��D^�D^� D_  D_}qD_��D`z�D`�qDa}qDa�qDb}qDc  Dc� Dd�Dd�De�De�De�qDf}qDf�qDg}qDg�qDh� Di�Di�DjDj}qDj�qDk� Dk�qDl� DmDm��Dn  Dn}qDn�qDo��Dp�Dp��Dq�Dq� Dq�qDr��DsDs� Ds��Dt}qDu  Du� Du�qDvz�Dw  Dw��Dx�Dx}qDx�RDyxRDy�qDz� D{�D{�D|�D|� D|�qD}z�D}�qD~z�D~�qD� D��D�AHD�~�D���D�HD�@ D�~�D�D��D�@ D�~�D���D���D�@ D�� D��HD�HD�@ D�}qD�� D�  D�=qD�~�D��HD�  D�@ D��HD��HD���D�=qD�~�D���D��qD�@ D�~�D���D���D�>�D�}qD���D�  D�AHD��HD���D���D�@ D�}qD��HD�HD�=qD�}qD��)D��)D�>�D�� D�� D�  D�AHD���D�� D�HD�B�D��HD�� D��qD�>�D��HD��HD�  D�@ D�� D���D�HD�AHD�~�D��qD���D�@ D��HD���D���D�@ D��HD���D��qD�>�D�~�D��HD�  D�>�D�� D���D�  D�AHD�~�D��HD��D�@ D�~�D��qD��)D�@ D�� D���D���D�>�D�� D���D�  D�@ D�� D��HD���D�@ D��HD��HD�  D�@ D��HD��HD�  D�>�D�~�D���D��qD�>�D�~�D��qD��qD�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D���D���D�@ D�� D��HD�HD�AHD�� D���D���D�@ D�� D��HD��D�@ D�� D��HD�  D�>�D�~�D��HD�HD�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD�� D�� D���D�@ D�� D���D���D�>�D�}qD�� D�  D�>�D�� D�� D�  D�@ D��HD���D���D�>�D�}qD��HD��D�AHD�� D���D�  D�B�D��HD��HD�HD�@ D�~�D���D�  D�>�D�}qD���D�  D�@ D�~�D���D��qD�=qD�� D��HD���D�@ D�� D�� D���D�=qD�� D�� D�  D�B�D��HD��HD�HD�@ D�~�D���D�  D�@ D�~�D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�}qD�� D�HD�@ DHD�� D��qD�@ DÂ�D�� D�  D�AHDāHD�� D�HD�@ Dŀ Dž�D���D�AHDƂ�D�� D���D�@ Dǀ D�� D�HD�>�D�}qD�� D�HD�AHD�~�DɽqD�  D�B�DʁHD��HD�  D�AHDˁHD��HD�HD�AHD́HD�� D���D�>�D̀ D��HD�HD�@ D΀ D��HD�HD�>�D�}qDϾ�D�  D�@ D�~�D�� D���D�>�Dр DѾ�D���D�>�D�}qDҽqD���D�@ DӁHDӾ�D���D�=qD�~�D�� D���D�>�DՂ�Dվ�D���D�@ Dր D�� D�HD�@ D�}qD�� D�HD�AHD؁HDؾ�D���D�@ DفHD�� D�  D�@ Dڀ D�� D���D�AHDہHD��HD�HD�>�D�~�D�� D�  D�AHD݀ Dݾ�D�  D�AHD�~�D�� D��D�B�D߀ D߾�D��qD�@ D�� D�� D�  D�@ D�HD��HD�  D�@ D�HD�D�HD�>�D�}qD�� D�HD�AHD� D�� D���D�>�D� D徸D���D�AHD悏D�� D���D�AHD�HD�� D�  D�AHD� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D꾸D�  D�AHD�HD�� D���D�>�D�~�D쾸D�  D�@ D�~�D��qD��)D�=qD�}qDD�  D�AHDD�D��D�AHD�~�D�� D�HD�AHD�HD��HD�  D�AHD� D�� D���D�>�D�HD�� D�HD�AHD�}qD�� D�HD�@ D�� D���D���D�>�D�~�D�� D��D�B�D�~�D���G�O�>u>���?W
=?�=q?�33?�@�@�R@:�H@J=q@^�R@z�H@��@���@�p�@��@�\)@�(�@��
@У�@�p�@�\@��@��HA�AQ�A�AG�A
=A�HA!G�A%A)��A0��A5A:=qA@��AFffAI��AP��AVffAZ�HAaG�Ag
=Ak�AqG�Aw�Az�HA���A��A�A�G�A��A�A�G�A�33A�A�G�A�33A�ffA�G�A�33A��RA�G�A��A�
=A���A��
A�ffA���A��A�ffA�Q�A��
A�{A�  A˅A�A�  AӅA�A�Q�AۅA�A��A�A�A��A�A�p�A��A�33A��A�Q�A��HA��B Q�B��B�RBz�BB�HB��B	�B
=B��B{B33B��B=qB\)B��B�\B�BG�B�RB�
B�B�HB   B!p�B#
=B$  B%��B&�HB(  B)��B+33B,(�B-B/33B0(�B1B3
=B4z�B6{B7\)B8z�B:{B;�B<z�B>=qB?�B@��BB�\BC�BD��BF�RBH  BH��BJ�RBL(�BMG�BN�HBPQ�BQG�BR�HBTz�BUBW
=BX��BY�B[33B\��B^{B_�BaG�Bb=qBc�
Bep�BfffBh  Bi��Bj�RBl(�BmBo
=Bp��Br{Bs33Bt��BvffBw�Bx��Bz�\B{�B}G�B~�HB�{B���B��B�(�B���B��B�=qB��HB�B�ffB�
=B��B���B�33B�{B��RB�G�B�=qB��RB�p�B�=qB���B��B�(�B���B��B�=qB���B��
B�z�B���B��
B��\B�
=B�B���B��B�B��\B�G�B��B���B�\)B�{B��HB��B�(�B�
=B�B�Q�B���B��
B�z�B�
=B��B�z�B��B�  B��\B�33B�  B�z�B��B�  B���B�33B��B���B�33B��B���B�G�B�B���B�\)B�B��\B�G�B�B�z�B�G�B�B��\B�\)B��
B��\B�p�B��B��\B�p�B�  B���B�p�B�=qB���B�p�B�Q�B���B�p�B�(�B�
=Bř�B�(�B���BǮB�(�B���Bə�B�{B��HB˙�B�  B���BͅB�  B��HBυB�  BиRBљ�B�(�BҸRBӅB�=qB���B�p�B�Q�B��HB�p�B�Q�B��HB�\)B�=qB��HB�p�B�  B���B݅B�  B޸RB߅B�(�B��B�p�B�(�B�\B�\)B�(�B��B�33B�  B�RB�33B��B��B�\)B��
B��B�G�B�B�\B�\)B��
B�z�B�G�B�  B��B�33B�  B�RB�G�B��B���B�\)B��B���B�\)B��B��RB�p�B�  B��\B�\)B�(�B��RB�G�B�(�B��HB�p�C   C ffC C
=C\)C��C
=C\)CC
=CQ�C�RC�C\)C�C�Cp�CC
=Cp�C�
C�CffC�
C33Cz�C��C	33C	��C	�HC
(�C
��C
�HC=qC��C�HCG�C�C�CG�C�C�C=qC�C��CG�CC  CffC��C{CffC�
C�C�C�C(�C��C  CG�C��C{Cp�CC�C�\C�C33C��C  CQ�C��C  Cp�C��C{C�C�C=qC�\C  CffCC
=Cp�C�
C=qC�\C�HC33C��C��C =qC ��C!  C!Q�C!��C"{C"p�C"C#{C#ffC#��C$33C$�\C$C%{C%p�C%�
C&{C&Q�C&�C'
=C'G�C'z�C'��C(�C(ffC(�\C(��C)(�C)\)C)�C)��C*{C*33C*ffC*�C*��C+33C+Q�C+��C+�
C,�C,Q�C,z�C,�C,�C-33C-z�C-�RC-�C.�C.ffC.�C.�HC/{C/Q�C/��C/�HC0�C0G�C0z�C0�RC1
=C1G�C1p�C1��C1��C233C2ffC2�\C2��C3{C3Q�C3�\C3C3�C433C4z�C4�C4�HC5{C5=qC5�C5��C6
=C633C6\)C6��C6�HC7{C7=qC7p�C7�C7��C8{C8G�C8�\C8C8�HC9{C9ffC9��C9�RC9�C:33C:p�C:�\C:�RC;  C;33C;ffC;��C;��C;��C<{C<Q�C<�\C<��C=  C=�C=G�C=�C=C>  C>=qC>\)C>�\C>C?
=C?G�C?z�C?��C?��C@{C@Q�C@�C@��C@�
CA
=CA=qCAz�CA�RCA�CB�CB=qCBp�CB��CB�HCC�CCQ�CCffCC�CC�CD(�CDQ�CDp�CD��CD�CE(�CEG�CEp�CE��CE�HCF�CF=qCFffCF��CF�
CG
=CG33CGQ�CG�\CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                   @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A��A���A��yA���AήA΋DA΅A΁A�\)A�=qA�33A�JA�%A�  A���A���A���A��A�ĜA�ffA��A̴9Ạ�Ả7A�l�A�^5A�XA�$�A�VA�A˲-Aˏ\A�t�A�n�A�VA�7LA�+A�%A��#AʶFA�`BA�5?A��A�VA�A��A��`A���A�AɾwAɶFAɛ�A�~�A�x�A�l�A�dZA�O�A��A�
=A��A���Aș�AȑhAȉ7A�r�A�VA�E�A�=qA��A���A���A�=qAƸRA�ĜA��;A��TA�%A�(�A���A�VA���A��A���A���A�r�A��TA�JA�n�A�A�A�jA��A�Q�A�-A�A��7A��;A�&�A�Q�A��A�{A��A�9XA���A��HA�(�A���A��9A��mA���A�A���A���A��;A��A�"�A�VA��+A�G�A��^A��uA~ȴA}O�Az�!Aw��At  Aq�Ao�Am+AjQ�Ag�TAgl�Ael�Aa��A\�9AY�AX�AU�ASAQ%AO�AK��AH�/AF�RAC��AAl�A?�A>~�A> �A=oA8v�A4-A3/A0r�A.�9A.v�A-�A)�A't�A&��A&�A$��A"ĜA!�#A!&�A VA;dA��A��A�`Ar�A�A��A �A
=AI�A��A��A��A��A��A(�A7LAp�A�wA\)A�FA~�A��A�hAl�A33A5?A?}A�A
��A	��A	|�A	x�A	hsA	G�A	�A��A��A��A�A��A��A{A�FA��A1'A�DA�DA��A��A�A��A��A��AbNA�RA$�A �A ~�A r�@��@�"�@�o@��R@���@���@�r�@� �@�@�!@�^5@�-@�/@�X@�@@�33@�ƨ@�+@�1'@�w@��@�5?@�=q@�^@�9@� �@�dZ@�n�@�$�@��@�-@�O�@��@���@�|�@�S�@�+@��@���@��@�-@���@�G�@�I�@��m@ۍP@�
=@ڧ�@ٙ�@أ�@׮@���@�v�@�=q@�@Դ9@�|�@Ұ!@�=q@�Z@�  @Ϯ@��;@�@�{@ͩ�@�x�@��/@�K�@ʟ�@�=q@�@��#@�7L@�r�@�bN@�A�@���@�+@���@�I�@�"�@°!@°!@���@��j@�bN@��;@�"�@���@�ȴ@�ȴ@�M�@���@��@��@���@�z�@�Z@�bN@���@�|�@�
=@�^5@�ff@�E�@��@�j@�b@��
@�C�@���@�^5@��^@���@�&�@�j@�  @��F@�dZ@�o@���@�V@�=q@�@���@���@�X@��@��9@���@��@�dZ@�K�@�C�@�o@�ȴ@�J@��7@��7@�p�@�/@�Ĝ@��j@�bN@��
@�S�@�+@�o@��@�ff@�J@�x�@���@��/@��/@���@���@��@�Z@�A�@�(�@��@�b@�  @��F@���@�^5@��T@��7@���@�r�@�b@��F@�K�@�
=@��y@���@��@�@���@��7@�X@�/@�%@��D@� �@��w@���@�;d@�o@�@��\@��@�J@��T@��T@��#@���@��h@�V@��9@�I�@�1@��m@�ƨ@��F@�t�@�33@���@��@��!@�ff@�-@��#@��^@�x�@�X@��@���@���@��@��@�I�@��
@�o@��y@���@�
=@�
=@��y@�ȴ@���@�M�@�=q@��@��^@��7@�p�@�p�@�X@�?}@���@��@�Ĝ@��u@�bN@�9X@���@�l�@�33@�"�@�@�ȴ@�n�@�$�@���@�@��@�/@���@�V@�%@���@���@�Z@��w@��P@��@�33@��y@���@��+@�^5@��@��T@���@�`B@�G�@���@��u@��D@��u@�r�@�1'@�1@�S�@��!@���@���@��\@�5?@��@�=q@�$�@���@��@�O�@�G�@���@��9@��D@��@�1'@��;@���@���@��w@�|�@�33@�
=@�@��R@�M�@�{@���@��T@���@��@�/@��`@��@�I�@�b@���@��m@��@��m@�ƨ@�|�@�;d@�+@��@�n�@��@��^@���@�X@�?}@��@�Ĝ@�bN@�1@�;@
=@~$�@}p�@}?}@|��@|j@|�@{�F@{o@z�@z��@z^5@zJ@y�@y��@y�@x��@x �@w��@wl�@w\)@wK�@v��@v�@vV@u�T@uO�@tZ@s��@s�F@st�@so@r�@r�\@r-@q��@q�#@q�#@q&�@p�u@pbN@o�;@o�@o|�@o+@o
=@n�@n��@n5?@m/@l��@l�/@l��@l�/@l��@lz�@l1@kS�@jn�@jM�@jJ@i�@i��@i�7@h�`@h  @g�w@g
=@f�@f��@f��@f��@fE�@e@e`B@e?}@eV@eV@d�j@d�D@dZ@d9X@d�@c�m@c�F@c�@cS�@b�@bM�@aG�@`��@`A�@_�@^�y@^��@^ff@^@]`B@\��@\�@\9X@[�F@[dZ@[S�@[33@Z��@Y�#@Y��@Y�7@Y�@X��@X �@W�w@W��@W\)@V�y@V�R@V��@V�+@V$�@U`B@UV@T�/@T�j@T�j@T�@T(�@S�m@St�@R�@RM�@Q�#@Q��@Qx�@Qhs@Q�@PĜ@PbN@PA�@P  @O�@N��@N�R@Nff@N{@M`B@L�/@L�j@L�@L�D@Lj@K��@K��@K�@K�@KdZ@K"�@J��@J~�@J-@I��@I7L@HĜ@Hr�@G��@G�P@G+@F��@F{@E�h@EO�@E?}@EO�@E�@EV@D�@DI�@DI�@DI�@C�m@C��@C"�@C@B�H@Bn�@B-@BJ@A�@A�#@Ahs@A%@@��@@��@@1'@?�;@?��@?K�@>�y@>�@>��@>E�@>{@=�T@=�h@<��@<9X@<�@;��@:�@:-@9hs@9%@8�`@8Ĝ@8 �@7\)@6��@5��@5`B@4�@4j@3�
@3S�@3C�@2�@2�!@2^5@2-@1��@1��@0��@0�u@01'@/�;@/�@/;d@.�@.�R@.��@.ff@.@-@-/@,�@,��@,��@,I�@,(�@,1@+�
@+�F@+t�@+C�@+o@*�!@*=q@)��@)�@)��@)��@)x�@)hs@)&�@(�`@(�u@(1'@(  @'��@'�@'l�@';d@'�@&�@&�R@&v�@&ff@&ff@%�@%�-@%�h@%`B@%?}@%V@$�@$�D@$I�@$�@#��@#t�@#C�@#33@#@"�!@"=q@"=q@"=q@"J@!��@!hs@!X@!G�@!7L@!7L@!7L@!&�@ ��@ ��@ Q�@�w@|�@l�@\)@;d@��@�@�R@��@�+@V@@�@O�@?}@V@�j@Z@1@��@ƨ@S�@33@�@��@�!@�\@M�@J@��@x�@7L@�@��@��@�u@ �@�@l�@;d@�@
=@�@�R@��@v�@5?@�@@��@�h@p�@`B@O�@/@V@��@�/@�@��@Z@1@�m@ƨ@��@t�@33@o@o@��@�!@�!@�!@��@~�@-@��@�#@hs@&�@�@��@��@��@Q�@1'@ �@b@  @�w@|�@|�@l�@K�@
=@�y@�@�@��@�+@�+@v�@ff@V@5?@$�@@�@p�@?}@V@�/@z�@Z@(�@��@t�@S�@C�A�oA�bA�33A�5?A�&�A�A��yA��A��A���A���A���A��;A��
A��
A���A���Aβ-AΣ�AΕ�AΉ7A΅AΉ7A΅A΁AΏ\A΁A�t�A�z�A�n�A�\)A�G�A�;dA�A�A�I�A�C�A�=qA�33A�
=A�JA��A��A�A�%A�A�%A�1A�A�%A�1A�A�%A�  A���A�  A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A��A��A��yA��/A��/A���A�ȴAͺ^A͡�A͛�A͗�ÁA�hsA�K�A�(�A�$�A��A�A���A��A���A�ȴA�ĜA̰!A̰!A̬Ḁ�A̧�A̧�Ạ�Ḁ�Ạ�A̙�A̙�A̓uA̋DÃA�~�A�x�A�p�A�p�A�jA�ffA�ffA�`BA�bNA�dZA�^5A�ZA�\)A�VA�\)A�\)A�XA�\)A�Q�A�C�A�9XA��A��A��A��A��A��A��A�{A�%A��A���A�ĜA˾wA˼jA˼jA˾wA˶FA˸RA˰!A˩�A˧�Aˣ�A˙�A˓uAˏ\A˃A�z�A�x�A�p�A�r�A�t�A�p�A�r�A�r�A�l�A�p�A�p�A�jA�^5A�ZA�VA�XA�XA�E�A�?}A�?}A�5?A�1'A�1'A�-A�-A�/A�-A�(�A�(�A��A��A�oA�  A��A��A��yA��TA��/A���A�ȴA�ȴAʾwAʸRAʺ^Aʰ!Aʥ�AʑhA�r�A�jA�VA�M�A�G�A�A�A�?}A�=qA�5?A�&�A�"�A��A��A��A�{A�bA�bA�{A�oA�VA�JA�1A�%A�1A�A���A���A���A��A��A��A��A��mA��A��mA��mA��yA��TA��HA��TA��#A��A��
A���A�ȴA�ƨA�ĜA���A�A�ĜA���AɼjA�AɾwAɺ^AɼjAɾwAɺ^Aɺ^AɼjAɶFAɸRAɶFAɮAɧ�Aɥ�Aɕ�Aɕ�AɑhAɅA�~�A�~�AɁA�|�A�|�AɃA�~�A�t�A�v�A�z�A�r�A�r�A�p�A�p�A�jA�jA�hsA�dZA�ffA�ffA�`BA�bNA�bNA�\)A�bNA�^5A�S�A�C�A�;dA�1'A�(�A��A��A�JA�JA�VA�JA�1A�VA�1A�A�A���A��A��`A��TA��;A���A��
A���A���A�ƨAȾwAȲ-Aȥ�AȓuAȕ�AȑhAȏ\AȓuAȓuAȑhAȑhAȓuAȑhAȍPAȏ\AȍPAȉ7Aȉ7Aȇ+AȁA�|�A�z�A�x�A�n�A�bNA�bNA�`BA�ZA�VA�VA�M�A�I�A�I�A�G�A�C�A�E�A�G�A�C�A�A�A�A�A�A�A�=qA�9XA�5?A�1'A��A�1A�%A�A���A���A���A��A��A���A��A��yA��`A��HA�ȴA�x�A�p�A�bNA�S�A�33A�bA���A��`A�A�A�AƲ-A�v�A�Q�A��A���Aş�AŇ+A�r�A�VA�?}A�{A��A�M�A��`A��;A�XA�/A���A� �A�ZA��\A�v�A��DA���A��\A�^5A�5?A�-A�$�A��A���A���A��^A���A��\A�p�A�O�A��A���A�JA��A�A�A� �A���A��-A�dZA�"�A�JA��HA���A�ZA���A��^A�\)A��`A��hA�-A��`A��7A� �A��A�M�A�5?A��A��
A���A��DA�XA��yA��A�{A� �A�ƨA�9XA�|�A��A�K�A�(�A�A���A���A�C�A���A���A�`BA�oA�A��hA�bNA�9XA�33A�7LA�C�A�/A��A�bA�%A���A��+A�hsA�(�A�{A���A��wA�v�A�1'A���A���A��A���A���A���A���A�|�A�\)A��`A�O�A��A��A�p�A�bNA�XA�O�A�9XA�/A�"�A��A�{A�bA�oA�oA�VA�JA�VA�JA���A��A��mA��HA��A��FA���A��7A��A�jA�C�A�/A�"�A�A�A��A��mA��
A�ĜA��jA��A���A��A�ZA�VA�I�A�&�A�VA��A��
A��9A���A���A��DA�t�A�hsA�`BA�?}A�"�A�
=A���A�5?A��FA�5?A��A�XA�+A��A���A��TA�ȴA�\)A��^A��DA�G�A�bA�ƨA�p�A�K�A�VA��`A��^A�|�A�A���A��^A���A��7A�v�A�p�A�n�A�hsA�Q�A�K�A�I�A�E�A�?}A�1'A�oA��A�O�A��HA���A�G�A�VA��-A�  A��hA�^5A�/A�JA��A��`A��mA��mA��mA��HA��/A���A��FA���A���A���A���A��7A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                   A�/A��A���A��yA���AήA΋DA΅A΁A�\)A�=qA�33A�JA�%A�  A���A���A���A��A�ĜA�ffA��A̴9Ạ�Ả7A�l�A�^5A�XA�$�A�VA�A˲-Aˏ\A�t�A�n�A�VA�7LA�+A�%A��#AʶFA�`BA�5?A��A�VA�A��A��`A���A�AɾwAɶFAɛ�A�~�A�x�A�l�A�dZA�O�A��A�
=A��A���Aș�AȑhAȉ7A�r�A�VA�E�A�=qA��A���A���A�=qAƸRA�ĜA��;A��TA�%A�(�A���A�VA���A��A���A���A�r�A��TA�JA�n�A�A�A�jA��A�Q�A�-A�A��7A��;A�&�A�Q�A��A�{A��A�9XA���A��HA�(�A���A��9A��mA���A�A���A���A��;A��A�"�A�VA��+A�G�A��^A��uA~ȴA}O�Az�!Aw��At  Aq�Ao�Am+AjQ�Ag�TAgl�Ael�Aa��A\�9AY�AX�AU�ASAQ%AO�AK��AH�/AF�RAC��AAl�A?�A>~�A> �A=oA8v�A4-A3/A0r�A.�9A.v�A-�A)�A't�A&��A&�A$��A"ĜA!�#A!&�A VA;dA��A��A�`Ar�A�A��A �A
=AI�A��A��A��A��A��A(�A7LAp�A�wA\)A�FA~�A��A�hAl�A33A5?A?}A�A
��A	��A	|�A	x�A	hsA	G�A	�A��A��A��A�A��A��A{A�FA��A1'A�DA�DA��A��A�A��A��A��AbNA�RA$�A �A ~�A r�@��@�"�@�o@��R@���@���@�r�@� �@�@�!@�^5@�-@�/@�X@�@@�33@�ƨ@�+@�1'@�w@��@�5?@�=q@�^@�9@� �@�dZ@�n�@�$�@��@�-@�O�@��@���@�|�@�S�@�+@��@���@��@�-@���@�G�@�I�@��m@ۍP@�
=@ڧ�@ٙ�@أ�@׮@���@�v�@�=q@�@Դ9@�|�@Ұ!@�=q@�Z@�  @Ϯ@��;@�@�{@ͩ�@�x�@��/@�K�@ʟ�@�=q@�@��#@�7L@�r�@�bN@�A�@���@�+@���@�I�@�"�@°!@°!@���@��j@�bN@��;@�"�@���@�ȴ@�ȴ@�M�@���@��@��@���@�z�@�Z@�bN@���@�|�@�
=@�^5@�ff@�E�@��@�j@�b@��
@�C�@���@�^5@��^@���@�&�@�j@�  @��F@�dZ@�o@���@�V@�=q@�@���@���@�X@��@��9@���@��@�dZ@�K�@�C�@�o@�ȴ@�J@��7@��7@�p�@�/@�Ĝ@��j@�bN@��
@�S�@�+@�o@��@�ff@�J@�x�@���@��/@��/@���@���@��@�Z@�A�@�(�@��@�b@�  @��F@���@�^5@��T@��7@���@�r�@�b@��F@�K�@�
=@��y@���@��@�@���@��7@�X@�/@�%@��D@� �@��w@���@�;d@�o@�@��\@��@�J@��T@��T@��#@���@��h@�V@��9@�I�@�1@��m@�ƨ@��F@�t�@�33@���@��@��!@�ff@�-@��#@��^@�x�@�X@��@���@���@��@��@�I�@��
@�o@��y@���@�
=@�
=@��y@�ȴ@���@�M�@�=q@��@��^@��7@�p�@�p�@�X@�?}@���@��@�Ĝ@��u@�bN@�9X@���@�l�@�33@�"�@�@�ȴ@�n�@�$�@���@�@��@�/@���@�V@�%@���@���@�Z@��w@��P@��@�33@��y@���@��+@�^5@��@��T@���@�`B@�G�@���@��u@��D@��u@�r�@�1'@�1@�S�@��!@���@���@��\@�5?@��@�=q@�$�@���@��@�O�@�G�@���@��9@��D@��@�1'@��;@���@���@��w@�|�@�33@�
=@�@��R@�M�@�{@���@��T@���@��@�/@��`@��@�I�@�b@���@��m@��@��m@�ƨ@�|�@�;d@�+@��@�n�@��@��^@���@�X@�?}@��@�Ĝ@�bN@�1@�;@
=@~$�@}p�@}?}@|��@|j@|�@{�F@{o@z�@z��@z^5@zJ@y�@y��@y�@x��@x �@w��@wl�@w\)@wK�@v��@v�@vV@u�T@uO�@tZ@s��@s�F@st�@so@r�@r�\@r-@q��@q�#@q�#@q&�@p�u@pbN@o�;@o�@o|�@o+@o
=@n�@n��@n5?@m/@l��@l�/@l��@l�/@l��@lz�@l1@kS�@jn�@jM�@jJ@i�@i��@i�7@h�`@h  @g�w@g
=@f�@f��@f��@f��@fE�@e@e`B@e?}@eV@eV@d�j@d�D@dZ@d9X@d�@c�m@c�F@c�@cS�@b�@bM�@aG�@`��@`A�@_�@^�y@^��@^ff@^@]`B@\��@\�@\9X@[�F@[dZ@[S�@[33@Z��@Y�#@Y��@Y�7@Y�@X��@X �@W�w@W��@W\)@V�y@V�R@V��@V�+@V$�@U`B@UV@T�/@T�j@T�j@T�@T(�@S�m@St�@R�@RM�@Q�#@Q��@Qx�@Qhs@Q�@PĜ@PbN@PA�@P  @O�@N��@N�R@Nff@N{@M`B@L�/@L�j@L�@L�D@Lj@K��@K��@K�@K�@KdZ@K"�@J��@J~�@J-@I��@I7L@HĜ@Hr�@G��@G�P@G+@F��@F{@E�h@EO�@E?}@EO�@E�@EV@D�@DI�@DI�@DI�@C�m@C��@C"�@C@B�H@Bn�@B-@BJ@A�@A�#@Ahs@A%@@��@@��@@1'@?�;@?��@?K�@>�y@>�@>��@>E�@>{@=�T@=�h@<��@<9X@<�@;��@:�@:-@9hs@9%@8�`@8Ĝ@8 �@7\)@6��@5��@5`B@4�@4j@3�
@3S�@3C�@2�@2�!@2^5@2-@1��@1��@0��@0�u@01'@/�;@/�@/;d@.�@.�R@.��@.ff@.@-@-/@,�@,��@,��@,I�@,(�@,1@+�
@+�F@+t�@+C�@+o@*�!@*=q@)��@)�@)��@)��@)x�@)hs@)&�@(�`@(�u@(1'@(  @'��@'�@'l�@';d@'�@&�@&�R@&v�@&ff@&ff@%�@%�-@%�h@%`B@%?}@%V@$�@$�D@$I�@$�@#��@#t�@#C�@#33@#@"�!@"=q@"=q@"=q@"J@!��@!hs@!X@!G�@!7L@!7L@!7L@!&�@ ��@ ��@ Q�@�w@|�@l�@\)@;d@��@�@�R@��@�+@V@@�@O�@?}@V@�j@Z@1@��@ƨ@S�@33@�@��@�!@�\@M�@J@��@x�@7L@�@��@��@�u@ �@�@l�@;d@�@
=@�@�R@��@v�@5?@�@@��@�h@p�@`B@O�@/@V@��@�/@�@��@Z@1@�m@ƨ@��@t�@33@o@o@��@�!@�!@�!@��@~�@-@��@�#@hs@&�@�@��@��@��@Q�@1'@ �@b@  @�w@|�@|�@l�@K�@
=@�y@�@�@��@�+@�+@v�@ff@V@5?@$�@@�@p�@?}@V@�/@z�@Z@(�@��@t�@S�G�O�A�oA�bA�33A�5?A�&�A�A��yA��A��A���A���A���A��;A��
A��
A���A���Aβ-AΣ�AΕ�AΉ7A΅AΉ7A΅A΁AΏ\A΁A�t�A�z�A�n�A�\)A�G�A�;dA�A�A�I�A�C�A�=qA�33A�
=A�JA��A��A�A�%A�A�%A�1A�A�%A�1A�A�%A�  A���A�  A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A��A��A��yA��/A��/A���A�ȴAͺ^A͡�A͛�A͗�ÁA�hsA�K�A�(�A�$�A��A�A���A��A���A�ȴA�ĜA̰!A̰!A̬Ḁ�A̧�A̧�Ạ�Ḁ�Ạ�A̙�A̙�A̓uA̋DÃA�~�A�x�A�p�A�p�A�jA�ffA�ffA�`BA�bNA�dZA�^5A�ZA�\)A�VA�\)A�\)A�XA�\)A�Q�A�C�A�9XA��A��A��A��A��A��A��A�{A�%A��A���A�ĜA˾wA˼jA˼jA˾wA˶FA˸RA˰!A˩�A˧�Aˣ�A˙�A˓uAˏ\A˃A�z�A�x�A�p�A�r�A�t�A�p�A�r�A�r�A�l�A�p�A�p�A�jA�^5A�ZA�VA�XA�XA�E�A�?}A�?}A�5?A�1'A�1'A�-A�-A�/A�-A�(�A�(�A��A��A�oA�  A��A��A��yA��TA��/A���A�ȴA�ȴAʾwAʸRAʺ^Aʰ!Aʥ�AʑhA�r�A�jA�VA�M�A�G�A�A�A�?}A�=qA�5?A�&�A�"�A��A��A��A�{A�bA�bA�{A�oA�VA�JA�1A�%A�1A�A���A���A���A��A��A��A��A��mA��A��mA��mA��yA��TA��HA��TA��#A��A��
A���A�ȴA�ƨA�ĜA���A�A�ĜA���AɼjA�AɾwAɺ^AɼjAɾwAɺ^Aɺ^AɼjAɶFAɸRAɶFAɮAɧ�Aɥ�Aɕ�Aɕ�AɑhAɅA�~�A�~�AɁA�|�A�|�AɃA�~�A�t�A�v�A�z�A�r�A�r�A�p�A�p�A�jA�jA�hsA�dZA�ffA�ffA�`BA�bNA�bNA�\)A�bNA�^5A�S�A�C�A�;dA�1'A�(�A��A��A�JA�JA�VA�JA�1A�VA�1A�A�A���A��A��`A��TA��;A���A��
A���A���A�ƨAȾwAȲ-Aȥ�AȓuAȕ�AȑhAȏ\AȓuAȓuAȑhAȑhAȓuAȑhAȍPAȏ\AȍPAȉ7Aȉ7Aȇ+AȁA�|�A�z�A�x�A�n�A�bNA�bNA�`BA�ZA�VA�VA�M�A�I�A�I�A�G�A�C�A�E�A�G�A�C�A�A�A�A�A�A�A�=qA�9XA�5?A�1'A��A�1A�%A�A���A���A���A��A��A���A��A��yA��`A��HA�ȴA�x�A�p�A�bNA�S�A�33A�bA���A��`A�A�A�AƲ-A�v�A�Q�A��A���Aş�AŇ+A�r�A�VA�?}A�{A��A�M�A��`A��;A�XA�/A���A� �A�ZA��\A�v�A��DA���A��\A�^5A�5?A�-A�$�A��A���A���A��^A���A��\A�p�A�O�A��A���A�JA��A�A�A� �A���A��-A�dZA�"�A�JA��HA���A�ZA���A��^A�\)A��`A��hA�-A��`A��7A� �A��A�M�A�5?A��A��
A���A��DA�XA��yA��A�{A� �A�ƨA�9XA�|�A��A�K�A�(�A�A���A���A�C�A���A���A�`BA�oA�A��hA�bNA�9XA�33A�7LA�C�A�/A��A�bA�%A���A��+A�hsA�(�A�{A���A��wA�v�A�1'A���A���A��A���A���A���A���A�|�A�\)A��`A�O�A��A��A�p�A�bNA�XA�O�A�9XA�/A�"�A��A�{A�bA�oA�oA�VA�JA�VA�JA���A��A��mA��HA��A��FA���A��7A��A�jA�C�A�/A�"�A�A�A��A��mA��
A�ĜA��jA��A���A��A�ZA�VA�I�A�&�A�VA��A��
A��9A���A���A��DA�t�A�hsA�`BA�?}A�"�A�
=A���A�5?A��FA�5?A��A�XA�+A��A���A��TA�ȴA�\)A��^A��DA�G�A�bA�ƨA�p�A�K�A�VA��`A��^A�|�A�A���A��^A���A��7A�v�A�p�A�n�A�hsA�Q�A�K�A�I�A�E�A�?}A�1'A�oA��A�O�A��HA���A�G�A�VA��-A�  A��hA�^5A�/A�JA��A��`A��mA��mA��mA��HA��/A���A��FA���A���A���A���A��7A�x�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                   ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�GB��B�SB��B�_B�xB�	B�7B��B�B�xB��B�~B��B��B�(B��B�.B��B��B�qB��B��B��B�-B�B��B�zB��B�B�#B�B�B��BچB�B�oB�B�lB�B�VB�B�BYB%B�BYB�B
=BBDBJB�B�B�B�B�B(B(B�B�B
�B	B�B�B	B�BfB�B�BSB�B�]B�2B�cB�&B�KB�)B�bB�~B�B��B.Bd�BVmBL�BMB2�B(�B@B�B�B�JB�B��B��B�QBҽBŢB�B��B�Bs�B\�B9$B33B�B	�BB
��B
یB
֡B
�jB
��B
��B
��B
�\B
�SB
k�B
]�B
YKB
F�B
A�B
2aB
%FB
�B	��B	�|B	�8B	�EB	�B	�wB	�$B	��B	��B	y	B	n�B	h>B	TaB	G�B	?B	.�B	 \B	:B	�B��B��B��B��B�BߤB�B�[B�B�,BѷB�vB˒B�?B��B��B͟BںBޞB�B�B�8B�B�B�B�oB�MB�B	 �B	~B	
=B	fB	�B	�B	�B	�B	�B		B	�B	SB	B	oB	B	_B	�B	�B	�B	oB�VB	�B	�B	1B		7B	B	 \B	"4B	$@B	&�B	(XB	,�B	4nB	5�B	9XB	:�B	7�B	7�B	CaB	L0B	QB	S&B	M6B	GEB	GzB	G�B	G�B	F�B	J#B	Y�B	P�B	N�B	MB	H�B	.�B	0!B	.�B	+6B	"hB	qB	#nB	#nB	#�B	'RB	*eB	-�B	;0B	E�B	WsB	`B	jKB	qAB	iDB	l"B	i�B	kB	rGB	u�B	�iB	�MB	�B	�JB	�JB	��B	�~B	�B	��B	��B	�B	�=B	�	B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�IB	��B	�-B	�4B	�B	��B	�@B	�@B	��B	��B	�B	��B	�_B	�$B	��B	��B	��B	�B	��B	�6B	�qB	��B	�LB	��B	��B	�dB	�6B	�*B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�qB	�B	��B	��B	��B	��B	�dB	�B	�B	��B	��B	��B	�tB	�?B	ɆB	�B	�B	�B	��B	��B	˒B	�dB	�^B	͟B	�B	�pB	�<B	�vB	�B	�NB	��B	�[B	��B	�2B	�
B	�KB	یB	��B	ܒB	ߤB	�pB	�B	��B	ߤB	�;B	ޞB	�jB	�B	��B	�B	�BB	��B	�HB	�HB	�NB	��B	�,B	��B	�yB	��B	�B	��B	��B	��B	�]B	�B	�]B	��B	�]B	�B	�)B	�)B	��B	��B	��B	�B	�GB	�MB	��B	��B	�ZB	��B	��B	��B	��B	�ZB	��B	��B	��B	��B	�fB	�2B	�2B	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
 iB
 4B
 iB
 �B
 4B
 �B
�B
AB
{B
�B
�B
�B
SB
�B
SB
�B
�B
%B
YB
+B
�B
1B
�B
fB
�B
�B
�B
	�B
	�B

�B

=B

rB

�B
xB
�B
�B
�B
VB
"B
(B
\B
�B
�B
�B
�B
.B
bB
bB
�B
hB
�B
:B
�B
B
�B
B
B
@B
@B
�B
B
uB
uB
{B
�B
B
�B
�B
�B
�B
�B
�B
�B
_B
�B
+B
�B
+B
eB
7B
	B
=B
=B
CB
�B
�B
xB
�B
!B
�B
 'B
�B
!B
�B
�B
�B
 �B
!�B
$�B
$�B
%FB
$�B
$�B
%FB
%B
$�B
$tB
&B
&B
%�B
%�B
&B
&B
&�B
&�B
&LB
&�B
&LB
&�B
&B
&�B
'B
'�B
(�B
(�B
)�B
*�B
+6B
+6B
+kB
+6B
+�B
,qB
-B
,qB
,�B
-�B
.B
.�B
.IB
.�B
/�B
/�B
/�B
0�B
1�B
2-B
1�B
1�B
1[B
0�B
0�B
0�B
1[B
1'B
1�B
2�B
2�B
3hB
3�B
5B
5�B
6B
6FB
6B
6�B
7�B
7LB
7�B
7�B
7�B
7LB
8B
7�B
8�B
9$B
9XB
9�B
:*B
:�B
:�B
;dB
;�B
<6B
<6B
<6B
>B
>BB
>�B
?HB
?B
?HB
?B
>�B
>�B
>BB
>B
>wB
>B
>B
>�B
?HB
?B
?HB
?B
?�B
?�B
?�B
@OB
@�B
@�B
@OB
A B
AUB
A�B
B[B
B[B
B�B
B�B
B[B
B�B
B�B
B�B
C-B
CaB
C-B
CaB
CaB
C�B
C�B
C�B
D�B
E9B
EmB
FB
FtB
GB
GzB
G�B
G�B
H�B
H�B
IB
IB
I�B
I�B
JXB
JXB
J�B
K)B
K^B
K^B
K^B
K�B
L�B
LdB
L�B
M6B
M�B
M�B
NB
NB
NB
NpB
NpB
N<B
N<B
N�B
OBB
OBB
O�B
O�B
OvB
O�B
OvB
O�B
PHB
QB
QNB
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T,B
T�B
UgB
U�B
U�B
U�B
U�B
U�B
VmB
V�B
V�B
VmB
VmB
VmB
V�B
V�B
W?B
W?B
XB
XEB
X�B
YB
YB
YB
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
]�B
^5B
^�B
^jB
^�B
_B
_B
_B
_;B
_B
_B
_B
`B
`BB
`BB
`�B
aB
`�B
a|B
bNB
a|B
a�B
b�B
cTB
c B
c�B
d&B
dZB
c�B
cTB
c B
c B
c�B
dZB
dZB
e,B
e`B
e�B
ffB
f�B
f�B
f�B
g�B
gmB
g8B
g8B
h
B
g�B
g�B
g�B
g�B
h>B
hsB
h�B
iB
iB
iDB
iyB
i�B
i�B
i�B
jB
jKB
j�B
j�B
j�B
kQB
k�B
k�B
l"B
l"B
l"B
l"B
l"B
l�B
l�B
l�B
m�B
m�B
n/B
n/B
n�B
n�B
n�B
o B
o5B
p;B
pB
p;B
p�B
p�B
p�B
poB
poB
p;B
p;B
p;B
p�B
qB
qAB
qvB
qvB
qvB
q�B
r|B
r�B
r�B
r|B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
uZB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v�B
w2B
w2B
w2B
w2B
w�B
x8B
x8B
x8B
x�B
y	B
y	B
y>B
y>B
yrB
y�B
zB
zB
zB
z�B
z�B
{B
{B
{B
{JB
{�B
|�B
|�B
|�B
|�B
|�B
}VB
}VB
}VB
}�B
}�B
~]B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
.B
.B
cB
cB
cB
� B
�4B
�4B
�iB
�iB
��B
��B
�B
��B
�oB
�oB
�oB
�oB
�oB
�oB
��B
�B
�B
��B
��B
��B
�B
��B
�GB
�{B
��B
��B
��B
��B
�B
�B
�B
�B
�MB
��B
��B
��B
��B
�SB
�SB
�B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
��B
��B
�B
��B
�fB��B��B��B�B��B�B��B��B��B��B��B��B�B��B�B��B��B�"B��B�lB��B�rB�1B�	B��B��B��B��B�rB��B��B�PB�DB�=B��B�lB��B��B��B��B�xB�"B��B�JB��B�B�JB�VB��B�~B�VB��B��B�\B��B��B��B�"B��B�\B�VB��B��B��B�\B��B�.B��B��B��B�\B�VB��B��B��B�oB��B�4B�B�{B��B��B�qB��B�\B�hB�XB�B�B��B�B��B�B��B�OB�OB��B��B��B�B�OB��B��B�!B�'B��B��B��B�OB�B��B��B�UB��B�3B�-B�9B�?B��B��B��B��B�B�B��B��B�B�tB�LB�tB�?B��B�?B��B��B�XB�dB��B�0B�B�B��B�jB�B� B��B͟BȀB�RB�XB�RB�B��BȀBǮBɺBɆB�RB�^B�^B�0B��B� B��BԕB�,B�[B՛BԕB�9B��B�B�QB�B�NB�|B�TB�ZB��B�|B��B�B�vB�oB��B�GB��B�vB�GB�MB�B��B��B��B��B�B�B�B��B�PB�PB�(B��B��B�]B�B�]B�.BBMBuB+BB�BYB�BB�BxBfB+B�B%BB�B�BB�B�BSBYBYB�BYB�B�B�B%B�BB�B�B%B+B�B�B�B	B�B
�B
=B	�B	lB�B
	B
�BB
�B	�B
�B~B
=B
�B~BDB
�BJBBBB�BxB�B�B�BVBVB(B�B�B(B�B�BbB�B\BhB�BVB�B�BVBB�B�B\B�B\B"B�B"B�B.BPB�B�BB�B\B B.BB\B"B~B�BVBJB�B�BB�B�B�B�BB�B
rB	�B
=BDBB�B
rBB�B1B	B�B�B	B�B_B�B	B�B�B	7B�B�BfB	B�B�B	lB
rB1B_B	�B	�B_B	B	�B�B1B	lB�B_B�B	lB�B�BfB	�B�B�B
	B
�B�B�B+B+B�BBSBB�B�BGBAB�B  B��B��B�]B�B��B�lB��B�DB��B��B�ZB��B�B��B�KB�vB��B�B�`B�HB�&B�;B�TB�B��B�&B�<B�[B�#B�B \B��B҉B��B��B�B�B�B��B��B��B�bB�OB�B��B�~B�=B�SB��B��B��B�bB�xB��B��B��B�uB�;B|PB��B�!Bm)Bj�Bl�Bc�Bg�B_;B^B`�B`B]/BQBJ#BN�BNBL0BF?BK�BK�BM�BN�B_�BEmBQNBEBO�B2�B0UB1[B)�B49BJ#B$�B.IB!�B)�BB�B#:B�BMB�BbBBuBJBB@BuB�B�B�B	lB�B�B
�B�BB�B�B��B�B��B�"B��B\B 4B��B�	B� B�QB��B�WB�B�yB�mB�mB�B�
B�2B��B�8B��B�B��B�B� B�ZB�B�NB�fB��B��B��B��B��BܒBݘB�dB�WB�5B��BרBیB֡BרB�gB��B��B�NB� B��BуB�B�}B͟B�vB��B��B�aB�B��B��B��B��B͟B��B�^B��B��B��B��B��B��B��B��B��B�nB�$B��B�B�kB��B�hB�FB�DB��B�~B�7B�oB�4B|PB|By	Bw�BwfBv�Bv+Bq�Bp;Bn�Bo Bm)Bo�B��B��BffB\�B\�BQ�B^5Bc�BLdBG�B@�B>B=<B9$B8�B7LB6�B7�B6B7�B7LB3hB2�B3�B0�B1[B/�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2023011600040720230116000407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023012520014420230125200144QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023012520014420230125200144QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194920230210131949IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                