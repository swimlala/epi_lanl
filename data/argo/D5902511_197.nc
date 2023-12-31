CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-16T03:04:53Z creation; 2022-02-04T23:30:07Z DMQC;      
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
_FillValue        G�O�     �  c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �P   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � <D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � [4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � b�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220116030453  20220204223522  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_197                 6810_008521_197                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٲB����@ٲB����11  @ٲB��C@ٲB��C@0��mq�@0��mq��d�|ᛑ�d�|ᛑ11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @��\@�G�@�G�@�\A ��A  A�RA+�A@��A`  A�Q�A���A�  A�\)A�\)A�\)A�\)A�  A��B�B�B  B�
B'�
B/�
B7�B@  BG�
BO�
BW�
B_�Bg�
Bo�Bw�
B�{B�(�B�=qB�(�B�{B�{B�  B��B��B�{B�(�B��B��B�  B��
B��B�{B�(�B�(�B�  B�  B�  B��B�  B�  B��B�  B�{B�(�B��B�{B�  B��C��C  C  C  C

=C
=C
=C  C��C  C
=C
=C��C��C  C   C"
=C$  C&  C({C)��C+��C.  C0  C2  C4
=C6  C8  C:
=C<  C>  C@{CB
=CC��CE��CG��CJ
=CL  CM��CP
=CR  CS��CU��CX
=CZ
=C\
=C^
=C`  Cb
=Cd  Ce�Ch  Cj
=Cl{Cn  Cp
=Cr  Cs�Cu��Cw�Cz  C|
=C~  C�  C��C�  C�  C�
=C�  C���C���C���C�  C���C���C�  C���C���C�  C�
=C�C�  C�  C�C�  C�  C�  C���C�C�C�
=C�C�C�  C�C���C�  C�C�  C�  C���C���C�  C�  C�C���C���C�  C�  C�C���C�  C�  C���C���C�C�  C�C���C�  C�  C���C���C���C�  C�C�C�  C���C�  C���C�C�
=C���C�  C�C���C�  C�  C�C�
=C�
=C�  C�
=C�  C�  C���C���C�  C�  C�  C�
=C�  C���C�  C�  C�C�C�  C�  C�
=C�
=C�C���C���C���C�  C�
=C�C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C�C�
=C�C�  C�C�  C���C�  C�C���D   D � D  D}qD�qD� D�qD� D  D� D�D� D  D� D�D� D�D� D	  D	��D
�D
��D  D� D�D��D  D� D��D� DD� D�qDz�D  D��D�D��D  D}qD��D}qD�D��D�D��D  D� D�D� D�qD}qD�D��D�D}qD  D� D�D��D�qD� DD��D   D ��D!  D!�D"�D"� D#  D#}qD#��D$}qD$�qD%� D&D&��D'�D'�D'�qD(z�D(�qD)��D*�D*}qD+  D+�D,D,�D-  D-}qD.�D.� D.�qD/� D/�qD0� D1  D1� D2�D2� D2�qD3��D4  D4� D5  D5}qD5�qD6� D6�qD7� D8�D8��D8�qD9}qD:�D:� D;  D;��D<�D<� D<�qD=}qD>D>� D>�qD?}qD@�D@��DA  DA}qDB  DB��DC�DC��DD  DD��DE�DE� DF  DF� DG  DG� DH  DH� DI�DI� DI�qDJz�DJ��DK� DK�qDL� DM  DM}qDM��DN� DO  DOz�DO�qDP��DQ�DQ}qDQ�qDRz�DR�qDS��DT�DT�DT�qDUxRDU��DV� DW�DW� DW�qDX� DY  DY��DZ�DZ��D[�D[� D\�D\��D]�D]��D^  D^}qD^�qD_��D`  D`}qD`�qDaz�Da��Db}qDc  Dc��Dc�qDd}qDe�De��Df�Df}qDf��Dgz�Dg�qDh� Di  Di� Dj  Dj� Dk  Dk��Dk�qDlz�Dl�qDm� Dm�qDn� Do  Do� Dp�Dp��Dq�Dq� Dq�qDr� Ds  Ds}qDt  Dt� Du  Du}qDu�qDv� Dw  Dwz�Dw�qDx� Dy�Dy}qDy�qDz}qDz�qD{��D|  D|� D}�D}� D~  D~� D�D��D�  D�AHD��HD��HD�  D�=qD�}qD�� D�HD�AHD�� D���D�  D�@ D��HD�D��D�AHD�� D�� D�  D�B�D���D��HD���D�@ D���D�D�HD�@ D�~�D��HD�HD�AHD�� D���D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D���D���D�@ D�� D��qD��qD�=qD�� D��HD���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D���D��HD���D�AHD��HD���D���D�@ D��HD��HD�  D�>�D�~�D�� D��qD�@ D��HD�� D�HD�B�D���D��HD�  D�@ D���D�D�  D�@ D�� D�� D���D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D���D���D�@ D��HD�D�HD�@ D�� D�� D�  D�>�D�}qD��)D���D�AHD���D��HD���D�@ D�� D���D��qD�@ D�� D�� D�HD�@ D�� D�� D���D�>�D�� D���D���D�@ D��HD�� D���D�@ D�~�D��qD�  D�@ D�� D�D��D�AHD�� D��HD�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�AHD���D�� D�HD�@ D�}qD��qD���D�@ D�� D�� D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD�~�D�� D��D�B�D�� D�� D�  D�>�D�}qD���D�HD�@ D�� D���D���D�AHD���D��HD�  D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D�~�D���D�HD�@ D�~�D�� D���D�=qD�� D��HD�  D�=qD�}qD���D���D�>�D�~�D���D�HD�AHD�� D�� D�HD�AHD�~�D���D�  D�@ D�~�D��HD��D�AHD D��HD�  D�>�DÁHD��HD�  D�@ DāHD��HD�  D�@ Dŀ Dž�D���D�@ DƁHD��HD�  D�>�D�~�DǾ�D�HD�AHDȀ D��HD�  D�>�Dɂ�D�� D��qD�@ DʁHD�� D�  D�AHD�~�D�� D�HD�AHD́HD�� D���D�@ D́HD��HD���D�@ D΁HD�� D�  D�@ D�}qDϽqD�  D�AHDЀ Dо�D��qD�AHDсHDѾ�D��qD�>�DҀ D�� D�HD�AHDӀ D�� D�HD�B�DԂ�D�� D�  D�@ D�~�Dվ�D���D�@ Dր D��HD��D�AHD׀ D�� D�  D�@ D؀ D�� D��qD�=qDـ D�D�HD�@ DځHD��HD���D�AHDۂ�D�� D��qD�@ D܂�D�D��D�>�D�|)DݽqD��D�C�Dނ�D޾�D��)D�=qD�}qD߾�D�  D�@ D��HD�� D�  D�B�D�HDᾸD���D�@ D� D�� D�HD�>�D�|)D�)D��)D�>�D䂏D���D�HD�@ D�~�D徸D�HD�@ D�HD��HD���D�=qD� D���D��D�B�D肏D�� D��qD�AHD��D�D�HD�>�D� D�� D�  D�>�D�~�D�� D���D�@ D�~�D�qD���D�>�D� D���D��qD�=qD� D��HD��D�AHD� D�� D�  D�@ D��HD�� D�  D�AHD�D�� D���D�@ D�HD�� D�  D�=qD�}qD�� D���D�@ D�HD�� D�HD�@ D�� D���D��{?�?\)?W
=?�\)?�Q�?��@�@�R@&ff@B�\@W
=@fff@�  @�=q@��@�  @���@��@��R@˅@��@�G�@�=q@�z�AG�AA
�HAG�AffA�A!G�A(Q�A,(�A1�A8��A<��AB�\AH��AN�RAS33AX��A_\)Ac33Aj=qAn�RAs�
Az�HA�  A��A�A�  A��\A�A��A�33A�{A��A�33A�p�A�  A�33A�z�A�Q�A��\A���A�  A�=qA�p�A�  A��\A�A��A\A�A�Q�Aʏ\A�A�Q�A�=qA�A�Q�A�=qA�p�A�  A�=qA�A�Q�A�\A�{A�Q�A��HA�ffA���A�33A��RB z�B�B�B��B=qB�B��B
�\B  B�B�HB  BG�B
=B  Bp�B33B  B��B\)Bz�BB�B ��B!B#�B$��B%�B'\)B)�B*{B+�
B-�B.ffB0  B1G�B2ffB4  B5p�B6ffB8(�B9��B:�\B<Q�B=��B>�RB@z�BABB�HBD��BEBF�HBH��BIBK�BL��BM�BO�BP��BR{BT  BT��BVffBX  BY�BZ�\B\(�B]G�B^�RB`Q�Bap�Bb�RBdz�Bep�Bg33Bhz�Bi��Bk�Bl��Bn{Bo�
Bp��Br�\Bt(�BuG�Bv�RBxz�Byp�Bz�HB|��B~{B33B�ffB��B�B���B�G�B��B��RB��B�  B��RB���B�(�B��HB�B�=qB�
=B��B�z�B��B�  B���B�G�B�  B��HB��B�(�B�
=B��B�Q�B�G�B��B��\B�p�B�{B���B�p�B�Q�B��HB�p�B�ffB�
=B���B�z�B�
=B��B�z�B�33B�B�z�B�G�B��
B���B�\)B��B��HB�p�B�{B���B��B�{B���B���B�(�B���B�B�Q�B���B��
B�z�B��B�  B�z�B�33B�{B���B�\)B�(�B��RB���B�Q�B��HB��B�ffB���B�B��\B��B�B��\B�G�B�B���B�G�B�B\B�33BÙ�B�Q�B�
=B�p�B��Bƣ�B���B�p�B�(�Bȏ\B��HBɅB��B�(�B���B�
=B�p�B��B�Q�B�z�B�
=B�p�B͙�B�{B�z�BΣ�B��BυBϮB�(�B�z�BиRB��Bљ�B�B�(�Bҏ\BҸRB��Bә�B��
B�{Bԏ\B��HB��Bՙ�B��B�(�B֣�B���B�33B�B�{B�Q�B���B�
=B�G�B��B�{B�ffB��HB�G�B�p�B��
B�ffB܏\B���B�\)B�B��B�Q�B���B�
=B�\)B��
B�{B�Q�B���B�33B�\)B�B�=qB�Q�B�RB�33B�p�B㙚B�(�B�ffB��B��B�p�B噚B�{B�ffB�\B�
=B�\)B�B��
B�Q�B�z�B���B�G�B�B�B�(�B�z�B��B��B�B�B�  B�ffB��B��HB�G�B��B��
B�=qB��B�RB��BB�B�{B��\B���B���B�\)B�B�  B�(�B��B��HB�
=B�B��B�  B�Q�B���B��B�G�B��B�(�B�ffB���B��HB�\)B��B��B�(�B���B��HB��B���B�B�  B�z�B���B��HB�33B���B��
B�  B�z�B���B�
=B�G�B��B�  B�(�B�ffB���B�33B�p�B���B��C 33C \)C ffC ��C ��C ��C  C33CffCz�C��C�
C�C  C33CffCz�C��C��C  C�C(�CffC�\C��CC  C�C33CffC�C��C�
C��C  C33C\)C\)C��CC��C�C�C=qCQ�Cp�C�C�
C�C  C�CQ�Cz�C�\C�RC�C{C33CG�CffC��C�
C��C	
=C	33C	ffC	�\C	��C	�
C

=C
(�C
G�C
z�C
�C
��C
�C{CG�Cz�C��C�RC��C(�C=qCffC�CC�C33CffCz�C��C�
C{C(�CQ�C�\CC�C
=C33Cp�C�C�
C��C33Cp�C�C�RC  C{CG�C�C�C��C{CQ�CffC�\C�
C{C33CffC�C�
C��C=qCz�C��C�
C�CG�Cp�C��C�HC{C33Cp�C�C�HC  C=qCz�CC�HC
=CQ�C�\C�C�HC33C\)C�C��C{C=qCffC��C�HC�C=qC�CC�C(�Cp�C�\CC{CQ�Cz�C��C�C33Cp�C��C��C
=C\)C�\C�RC��C =qC �\C �C �C!33C!z�C!��C!�
C"33C"p�C"��C"�
C#(�C#\)C#�C#�
C$�C$G�C$z�C$��C%{C%=qC%p�C%C%��C&(�C&p�C&�RC&�
C'{C'ffC'��C'�
C(
=C(G�C(�\C(�RC(�C)33C)z�C)�C)�
C*(�C*p�C*�\C*�
C+(�C+\)C+�C+�HC,�C,G�C,�\C,�
C-�C-G�C-�C-��C.{C.G�C.z�C.��C/{C/=qC/z�C/��C0
=C0(�C0z�C0C0��C1�C1p�C1�C1��C2�C2ffC2�\C2C3{C3G�C3z�C3��C3��C4(�C4p�C4�RC4�
C5(�C5p�C5�\C5��C6�C6G�C6z�C6��C7  C7(�C7p�C7�RC7�HC8{C8\)C8��C8��C8��C933C9z�C9�C9�
C:{C:Q�C:��C:C:��C;=qC;p�C;��C;��C<�C<Q�C<p�C<�C<�C=
=C=33C=p�C=�RC=�
C=��C>=qC>z�C>��C>C?  C?G�C?ffC?�C?C@
=C@(�C@Q�C@��C@��C@�CA�CA\)CA�\CA�CA�
CB�CB\)CBp�CB��CB�HCC  CC(�CC\)CC�\CC��CD
=CD(�CDQ�CDp�CD��CD�CE�CE=qCE\)CE��CE��CF
=CF33CFQ�CF�CFCF�CG{CGG�CG�CGCH  CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                    ?�  ?��H@@  @��\@�G�@�G�@�\A ��A  A�RA+�A@��A`  A�Q�A���A�  A�\)A�\)A�\)A�\)A�  A��B�B�B  B�
B'�
B/�
B7�B@  BG�
BO�
BW�
B_�Bg�
Bo�Bw�
B�{B�(�B�=qB�(�B�{B�{B�  B��B��B�{B�(�B��B��B�  B��
B��B�{B�(�B�(�B�  B�  B�  B��B�  B�  B��B�  B�{B�(�B��B�{B�  B��C��C  C  C  C

=C
=C
=C  C��C  C
=C
=C��C��C  C   C"
=C$  C&  C({C)��C+��C.  C0  C2  C4
=C6  C8  C:
=C<  C>  C@{CB
=CC��CE��CG��CJ
=CL  CM��CP
=CR  CS��CU��CX
=CZ
=C\
=C^
=C`  Cb
=Cd  Ce�Ch  Cj
=Cl{Cn  Cp
=Cr  Cs�Cu��Cw�Cz  C|
=C~  C�  C��C�  C�  C�
=C�  C���C���C���C�  C���C���C�  C���C���C�  C�
=C�C�  C�  C�C�  C�  C�  C���C�C�C�
=C�C�C�  C�C���C�  C�C�  C�  C���C���C�  C�  C�C���C���C�  C�  C�C���C�  C�  C���C���C�C�  C�C���C�  C�  C���C���C���C�  C�C�C�  C���C�  C���C�C�
=C���C�  C�C���C�  C�  C�C�
=C�
=C�  C�
=C�  C�  C���C���C�  C�  C�  C�
=C�  C���C�  C�  C�C�C�  C�  C�
=C�
=C�C���C���C���C�  C�
=C�C�  C�  C�  C�C�  C���C���C���C�  C�  C���C���C�C�
=C�C�  C�C�  C���C�  C�C���D   D � D  D}qD�qD� D�qD� D  D� D�D� D  D� D�D� D�D� D	  D	��D
�D
��D  D� D�D��D  D� D��D� DD� D�qDz�D  D��D�D��D  D}qD��D}qD�D��D�D��D  D� D�D� D�qD}qD�D��D�D}qD  D� D�D��D�qD� DD��D   D ��D!  D!�D"�D"� D#  D#}qD#��D$}qD$�qD%� D&D&��D'�D'�D'�qD(z�D(�qD)��D*�D*}qD+  D+�D,D,�D-  D-}qD.�D.� D.�qD/� D/�qD0� D1  D1� D2�D2� D2�qD3��D4  D4� D5  D5}qD5�qD6� D6�qD7� D8�D8��D8�qD9}qD:�D:� D;  D;��D<�D<� D<�qD=}qD>D>� D>�qD?}qD@�D@��DA  DA}qDB  DB��DC�DC��DD  DD��DE�DE� DF  DF� DG  DG� DH  DH� DI�DI� DI�qDJz�DJ��DK� DK�qDL� DM  DM}qDM��DN� DO  DOz�DO�qDP��DQ�DQ}qDQ�qDRz�DR�qDS��DT�DT�DT�qDUxRDU��DV� DW�DW� DW�qDX� DY  DY��DZ�DZ��D[�D[� D\�D\��D]�D]��D^  D^}qD^�qD_��D`  D`}qD`�qDaz�Da��Db}qDc  Dc��Dc�qDd}qDe�De��Df�Df}qDf��Dgz�Dg�qDh� Di  Di� Dj  Dj� Dk  Dk��Dk�qDlz�Dl�qDm� Dm�qDn� Do  Do� Dp�Dp��Dq�Dq� Dq�qDr� Ds  Ds}qDt  Dt� Du  Du}qDu�qDv� Dw  Dwz�Dw�qDx� Dy�Dy}qDy�qDz}qDz�qD{��D|  D|� D}�D}� D~  D~� D�D��D�  D�AHD��HD��HD�  D�=qD�}qD�� D�HD�AHD�� D���D�  D�@ D��HD�D��D�AHD�� D�� D�  D�B�D���D��HD���D�@ D���D�D�HD�@ D�~�D��HD�HD�AHD�� D���D�  D�@ D�~�D��HD�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D���D���D�@ D�� D��qD��qD�=qD�� D��HD���D�>�D�~�D�� D�  D�>�D�� D��HD�HD�@ D�� D��HD�HD�@ D���D��HD���D�AHD��HD���D���D�@ D��HD��HD�  D�>�D�~�D�� D��qD�@ D��HD�� D�HD�B�D���D��HD�  D�@ D���D�D�  D�@ D�� D�� D���D�@ D�� D���D�  D�AHD��HD�� D�  D�AHD�� D���D���D�@ D��HD�D�HD�@ D�� D�� D�  D�>�D�}qD��)D���D�AHD���D��HD���D�@ D�� D���D��qD�@ D�� D�� D�HD�@ D�� D�� D���D�>�D�� D���D���D�@ D��HD�� D���D�@ D�~�D��qD�  D�@ D�� D�D��D�AHD�� D��HD�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�AHD���D�� D�HD�@ D�}qD��qD���D�@ D�� D�� D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD�~�D�� D��D�B�D�� D�� D�  D�>�D�}qD���D�HD�@ D�� D���D���D�AHD���D��HD�  D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D�~�D���D�HD�@ D�~�D�� D���D�=qD�� D��HD�  D�=qD�}qD���D���D�>�D�~�D���D�HD�AHD�� D�� D�HD�AHD�~�D���D�  D�@ D�~�D��HD��D�AHD D��HD�  D�>�DÁHD��HD�  D�@ DāHD��HD�  D�@ Dŀ Dž�D���D�@ DƁHD��HD�  D�>�D�~�DǾ�D�HD�AHDȀ D��HD�  D�>�Dɂ�D�� D��qD�@ DʁHD�� D�  D�AHD�~�D�� D�HD�AHD́HD�� D���D�@ D́HD��HD���D�@ D΁HD�� D�  D�@ D�}qDϽqD�  D�AHDЀ Dо�D��qD�AHDсHDѾ�D��qD�>�DҀ D�� D�HD�AHDӀ D�� D�HD�B�DԂ�D�� D�  D�@ D�~�Dվ�D���D�@ Dր D��HD��D�AHD׀ D�� D�  D�@ D؀ D�� D��qD�=qDـ D�D�HD�@ DځHD��HD���D�AHDۂ�D�� D��qD�@ D܂�D�D��D�>�D�|)DݽqD��D�C�Dނ�D޾�D��)D�=qD�}qD߾�D�  D�@ D��HD�� D�  D�B�D�HDᾸD���D�@ D� D�� D�HD�>�D�|)D�)D��)D�>�D䂏D���D�HD�@ D�~�D徸D�HD�@ D�HD��HD���D�=qD� D���D��D�B�D肏D�� D��qD�AHD��D�D�HD�>�D� D�� D�  D�>�D�~�D�� D���D�@ D�~�D�qD���D�>�D� D���D��qD�=qD� D��HD��D�AHD� D�� D�  D�@ D��HD�� D�  D�AHD�D�� D���D�@ D�HD�� D�  D�=qD�}qD�� D���D�@ D�HD�� D�HD�@ D�� D���G�O�?�?\)?W
=?�\)?�Q�?��@�@�R@&ff@B�\@W
=@fff@�  @�=q@��@�  @���@��@��R@˅@��@�G�@�=q@�z�AG�AA
�HAG�AffA�A!G�A(Q�A,(�A1�A8��A<��AB�\AH��AN�RAS33AX��A_\)Ac33Aj=qAn�RAs�
Az�HA�  A��A�A�  A��\A�A��A�33A�{A��A�33A�p�A�  A�33A�z�A�Q�A��\A���A�  A�=qA�p�A�  A��\A�A��A\A�A�Q�Aʏ\A�A�Q�A�=qA�A�Q�A�=qA�p�A�  A�=qA�A�Q�A�\A�{A�Q�A��HA�ffA���A�33A��RB z�B�B�B��B=qB�B��B
�\B  B�B�HB  BG�B
=B  Bp�B33B  B��B\)Bz�BB�B ��B!B#�B$��B%�B'\)B)�B*{B+�
B-�B.ffB0  B1G�B2ffB4  B5p�B6ffB8(�B9��B:�\B<Q�B=��B>�RB@z�BABB�HBD��BEBF�HBH��BIBK�BL��BM�BO�BP��BR{BT  BT��BVffBX  BY�BZ�\B\(�B]G�B^�RB`Q�Bap�Bb�RBdz�Bep�Bg33Bhz�Bi��Bk�Bl��Bn{Bo�
Bp��Br�\Bt(�BuG�Bv�RBxz�Byp�Bz�HB|��B~{B33B�ffB��B�B���B�G�B��B��RB��B�  B��RB���B�(�B��HB�B�=qB�
=B��B�z�B��B�  B���B�G�B�  B��HB��B�(�B�
=B��B�Q�B�G�B��B��\B�p�B�{B���B�p�B�Q�B��HB�p�B�ffB�
=B���B�z�B�
=B��B�z�B�33B�B�z�B�G�B��
B���B�\)B��B��HB�p�B�{B���B��B�{B���B���B�(�B���B�B�Q�B���B��
B�z�B��B�  B�z�B�33B�{B���B�\)B�(�B��RB���B�Q�B��HB��B�ffB���B�B��\B��B�B��\B�G�B�B���B�G�B�B\B�33BÙ�B�Q�B�
=B�p�B��Bƣ�B���B�p�B�(�Bȏ\B��HBɅB��B�(�B���B�
=B�p�B��B�Q�B�z�B�
=B�p�B͙�B�{B�z�BΣ�B��BυBϮB�(�B�z�BиRB��Bљ�B�B�(�Bҏ\BҸRB��Bә�B��
B�{Bԏ\B��HB��Bՙ�B��B�(�B֣�B���B�33B�B�{B�Q�B���B�
=B�G�B��B�{B�ffB��HB�G�B�p�B��
B�ffB܏\B���B�\)B�B��B�Q�B���B�
=B�\)B��
B�{B�Q�B���B�33B�\)B�B�=qB�Q�B�RB�33B�p�B㙚B�(�B�ffB��B��B�p�B噚B�{B�ffB�\B�
=B�\)B�B��
B�Q�B�z�B���B�G�B�B�B�(�B�z�B��B��B�B�B�  B�ffB��B��HB�G�B��B��
B�=qB��B�RB��BB�B�{B��\B���B���B�\)B�B�  B�(�B��B��HB�
=B�B��B�  B�Q�B���B��B�G�B��B�(�B�ffB���B��HB�\)B��B��B�(�B���B��HB��B���B�B�  B�z�B���B��HB�33B���B��
B�  B�z�B���B�
=B�G�B��B�  B�(�B�ffB���B�33B�p�B���B��C 33C \)C ffC ��C ��C ��C  C33CffCz�C��C�
C�C  C33CffCz�C��C��C  C�C(�CffC�\C��CC  C�C33CffC�C��C�
C��C  C33C\)C\)C��CC��C�C�C=qCQ�Cp�C�C�
C�C  C�CQ�Cz�C�\C�RC�C{C33CG�CffC��C�
C��C	
=C	33C	ffC	�\C	��C	�
C

=C
(�C
G�C
z�C
�C
��C
�C{CG�Cz�C��C�RC��C(�C=qCffC�CC�C33CffCz�C��C�
C{C(�CQ�C�\CC�C
=C33Cp�C�C�
C��C33Cp�C�C�RC  C{CG�C�C�C��C{CQ�CffC�\C�
C{C33CffC�C�
C��C=qCz�C��C�
C�CG�Cp�C��C�HC{C33Cp�C�C�HC  C=qCz�CC�HC
=CQ�C�\C�C�HC33C\)C�C��C{C=qCffC��C�HC�C=qC�CC�C(�Cp�C�\CC{CQ�Cz�C��C�C33Cp�C��C��C
=C\)C�\C�RC��C =qC �\C �C �C!33C!z�C!��C!�
C"33C"p�C"��C"�
C#(�C#\)C#�C#�
C$�C$G�C$z�C$��C%{C%=qC%p�C%C%��C&(�C&p�C&�RC&�
C'{C'ffC'��C'�
C(
=C(G�C(�\C(�RC(�C)33C)z�C)�C)�
C*(�C*p�C*�\C*�
C+(�C+\)C+�C+�HC,�C,G�C,�\C,�
C-�C-G�C-�C-��C.{C.G�C.z�C.��C/{C/=qC/z�C/��C0
=C0(�C0z�C0C0��C1�C1p�C1�C1��C2�C2ffC2�\C2C3{C3G�C3z�C3��C3��C4(�C4p�C4�RC4�
C5(�C5p�C5�\C5��C6�C6G�C6z�C6��C7  C7(�C7p�C7�RC7�HC8{C8\)C8��C8��C8��C933C9z�C9�C9�
C:{C:Q�C:��C:C:��C;=qC;p�C;��C;��C<�C<Q�C<p�C<�C<�C=
=C=33C=p�C=�RC=�
C=��C>=qC>z�C>��C>C?  C?G�C?ffC?�C?C@
=C@(�C@Q�C@��C@��C@�CA�CA\)CA�\CA�CA�
CB�CB\)CBp�CB��CB�HCC  CC(�CC\)CC�\CC��CD
=CD(�CDQ�CDp�CD��CD�CE�CE=qCE\)CE��CE��CF
=CF33CFQ�CF�CFCF�CG{CGG�CG�CGCH  CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                    @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AсA�z�A�^5A�Q�A�G�A�JA���A�ȴA�p�A�S�A�M�A�K�A�5?A�&�A��A��A��A�oA�VA�JA�JA�
=A�
=A�
=A�1A�%A�A�A�A�A�A�A�A�A�A�%A�%A�1A�1A�%A�A�%A�1A�1A�1A�
=A�1A�
=A�1A�%A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�1A�oA�1'A�&�A�
=A�JA�A�"�A���A���A˓uAʇ+A���AœuA��TA�K�A��yA�XA�M�A�hsA���A�oA�E�A��A�+A���A���A���A�%A�/A��-A���A���A���A�A�I�A��yA�`BA���A�x�A��A�`BA�E�A���A��A���A��+A���A�A��
A���A�ffA�E�A�&�A�A�-A���A�\)A��A�Q�A��yA��;A�{A}��Az~�Aw��Au��ArAi7LAc�7AaA_t�A\��AZ�9AY\)AS��AS�ARE�AQ�FANr�AF��AA�A?
=A<z�A8�\A6��A6  A4��A3�A2n�A0��A/|�A.�A.bNA.{A-��A*�DA(��A&ȴA%�FA$ZA"^5A!�A!�hA!�A!`BA n�AG�AQ�A{A�A-A�hA%A�DAE�AbA�7A��A��AoA�A�jA5?Al�A��A�A�AQ�A(�AXA�A=qA�mAO�AȴA-A�A�^A�A/A�9Az�A-A��A�;A  A�A$�AJA�#A�-A��AhsA
=A
�jA
�DA
��A	�A	"�A�Al�A�HA��A�AI�AbA�A�
AƨA��A�Ap�A?}A"�AA�`AĜA�\A=qA��A7LA��A��A�mA�A j@���@��@�5?@��/@�\)@�$�@��j@��m@�33@�5?@���@�`B@�Ĝ@�@�@�V@�-@��@�F@�O�@�D@�bN@��@�dZ@�$�@��@�X@���@���@��@�Z@�|�@�+@��#@�&�@�D@� �@�@��@�!@��@ᙚ@�h@�O�@��@�j@�j@� �@߾w@޸R@��#@ݡ�@�V@ܣ�@�;d@�@�G�@���@أ�@�A�@���@�dZ@�o@�ff@���@ղ-@�z�@�ƨ@��@��@�  @�
=@ΰ!@Ώ\@͡�@�bN@�Z@�1@��@�ƨ@˝�@˅@�l�@�\)@�S�@�K�@�"�@�o@���@��H@�M�@�V@ț�@�A�@ǅ@�;d@�@�p�@�Q�@�1@��
@��@�V@�{@���@��-@�hs@�`B@�G�@�V@�A�@���@���@���@�X@��@��u@�t�@��!@�v�@�~�@�^5@���@�@�%@���@�Z@���@�o@�@�~�@�J@���@��/@�bN@��@���@�V@�^5@���@��@��@���@��@��@��^@�5?@��@��h@�7L@��j@��/@�j@��@���@��R@�5?@��@�V@���@�9X@��@��F@�o@��\@���@�/@��@��j@��@��@�ff@�M�@�O�@���@���@�b@�@��R@��\@�v�@�^5@�@��@��@��@�A�@���@�t�@�l�@�l�@�\)@�S�@�33@�"�@��@�ȴ@���@��+@�E�@�-@�{@��@���@�/@�V@���@�r�@��;@���@�\)@�33@��y@�^5@�-@�{@���@��@��T@���@�@�@��-@��h@�x�@��@���@��@��D@�j@�A�@�  @�l�@���@�n�@���@�x�@�X@�7L@�&�@��@��@�Ĝ@��9@��9@��@��@���@�r�@��@���@�;d@���@��+@�{@��7@�%@���@���@��u@�Q�@� �@���@��
@��@�dZ@�K�@�
=@���@�M�@�-@�@��#@���@�p�@�X@�/@�&�@���@���@�Z@���@�|�@�+@��y@��R@��+@�n�@�M�@��T@��h@��@���@���@�Z@�1'@� �@� �@�(�@� �@� �@�b@��m@��
@��
@��w@��P@�dZ@�K�@�"�@���@�n�@�M�@��@���@���@���@��@�7L@���@��@�(�@��m@���@���@�dZ@�C�@�+@�@��H@��+@�5?@�J@��@���@�`B@�G�@�&�@�%@��@���@���@�j@�@~�@~V@~$�@}��@|�@|��@|Z@{��@{"�@z=q@y��@yx�@yhs@x��@xr�@w�@w�P@v�+@v{@u@u�@uV@t�@t�@s�@r�@q��@qx�@q�@pb@o;d@n�@n��@n�+@nV@m/@l�j@l�j@l��@l�@l(�@k�m@kƨ@k��@k��@kt�@kS�@kC�@k33@k"�@k@j^5@j-@j�@i��@h�`@h��@h��@hĜ@hA�@g�P@f��@fȴ@f�+@f{@e��@ep�@d�j@d�D@d(�@c��@cdZ@cC�@c@b��@bn�@bM�@b-@bJ@a�^@ahs@aG�@a7L@`��@`bN@_�@_�@_l�@^��@^5?@]�h@]�@\9X@[�m@[dZ@Z��@Z=q@Y��@Y��@Y�#@Y�^@Y&�@Xr�@W�w@W+@V��@V�y@VE�@U��@UO�@T�/@S�m@Sƨ@S�@SC�@R��@R-@Q�7@P��@PbN@Pb@O�@O
=@N5?@M��@M�@M/@L�D@L1@K�F@K33@J�\@Jn�@J-@I�@I��@I%@H�u@H1'@Hb@G|�@F��@F�@F�+@FV@E�T@EV@C�
@CC�@Co@B��@B=q@A�@Ahs@A%@@�u@?�w@?��@?l�@?�@>�@>ȴ@>�R@>�+@>5?@=p�@<9X@;ƨ@;��@;dZ@;@:^5@9��@9&�@8��@8Ĝ@81'@7��@7��@7|�@7;d@6�y@6�R@6��@6v�@6V@5�@5�h@5p�@5�@4��@4��@4�D@4z�@4j@4�@3�m@3�@3C�@3"�@3o@2��@2n�@2=q@2-@2�@1��@1�^@1��@1x�@1X@1X@1X@17L@0�`@0�9@0bN@0b@/�@/\)@/
=@.��@.��@.�y@.ff@.{@-@-�h@-`B@-/@,�@,�j@,�D@,I�@+�m@+t�@+33@+@*�@*�H@*��@*�!@*n�@*J@)��@)x�@)hs@)&�@(��@(�9@(r�@(A�@(b@'�w@'l�@'K�@'+@&��@&�y@&�R@&ff@&E�@&$�@%�@%��@$��@$�@$��@$z�@$I�@$9X@$�@#��@#��@#dZ@#33@"��@"M�@!�@!��@!�^@!��@!�7@!G�@!%@ ��@ 1'@��@|�@K�@��@�R@��@ff@$�@��@�h@O�@V@�@�D@�@�m@ƨ@�F@��@��@�@dZ@33@@�H@��@�\@~�@n�@M�@J@��@hs@&�@%@��@��@��@��@��@Q�@�@��@��@|�@l�@K�@�@
=@�y@�@�R@v�@E�@@��@�-@?}@/@�@��@1@t�@t�@t�@t�@C�@"�@�H@��@�!@��@��@~�@n�@=q@�#@�^@��@��@�7@X@�@��@��@�@�@��@��@��@\)@K�@;d@�@�y@ȴ@��@��@��@��@�+@5?@�T@�-@p�@/@�@�/@��@�j@��AсAуA�v�Aч+AсA�z�A�z�A�x�A�l�A�VA�S�A�M�A�VA�S�A�Q�A�?}A�K�A�-A�(�A��
A���A�ȴA���A�ĜA���AЮA�t�A�p�A�ZA�S�A�S�A�Q�A�K�A�M�A�O�A�K�A�I�A�M�A�E�A�;dA�9XA�5?A�1'A�33A�-A�+A�-A�(�A�"�A�$�A��A��A��A��A��A� �A��A� �A��A��A��A��A��A��A�{A��A�{A��A��A�oA�{A�VA�bA�oA�bA�JA�oA�VA�JA�oA�bA�JA�VA�JA�
=A�VA�VA�
=A�bA�
=A�
=A�VA�
=A�
=A�VA�
=A�1A�JA�
=A�JA�JA�1A�JA�
=A�1A�JA�1A�1A�
=A�%A�1A�JA�%A�1A�JA�%A�%A�1A�A�A�%A�%A�  A�A�%A�A�%A�A�A�%A�A�A�%A�A�  A�A�%A�A�A�A�  A�%A�%A�A�%A�A�A�1A�A�A�A�  A�%A�A�  A�A�  A�A�%A�  A�A�%A�A�A�%A�A�A�1A�A�%A�%A�A�1A�A�A�1A�A�%A�1A�A�1A�
=A�%A�A�
=A�
=A�%A�
=A�
=A�%A�
=A�1A�A�%A�
=A�%A�%A�1A�A�%A�1A�A�A�%A�A�A�%A�1A�A�A�
=A�%A�%A�
=A�1A�%A�
=A�1A�%A�
=A�1A�%A�1A�JA�1A�%A�JA�1A�%A�JA�1A�1A�
=A�
=A�%A�1A�
=A�%A�
=A�
=A�1A�JA�%A�%A�JA�
=A�%A�
=A�%A�A�%A�
=A�%A�%A�1A�A�A�%A�A�A�%A�  A�A�A�  A�A�%A�  A�A�A�A�A�1A�A�A�%A�A�A�1A�A�A�%A�%A�A�%A�1A�A�%A�1A�A�A�1A�%A�A�1A�%A�A�1A�A�A�1A�%A�A�%A�1A�A�%A�1A�A�1A�1A�A�1A�
=A�A�%A�1A�A�%A�1A�A�%A�
=A�%A�A�1A�%A�A�1A�1A�A�1A�%A�A�1A�%A�A�1A�A�A�1A�%A�A�%A�1A�A�A�1A�%A�A�1A�1A�A�%A�1A�A�%A�1A�%A�A�
=A�JA�1A�1A�JA�1A�
=A�bA�VA�JA�oA�bA�{A��A��A��A��A� �A��A�&�A�(�A�$�A�(�A�7LA�7LA�?}A�E�A�E�A�A�A�E�A�E�A�A�A�E�A�E�A�=qA�+A��A�bA�VA�VA�
=A�A�A�A���A�A�1A�1A�
=A�bA�VA�
=A�VA�oA�bA�bA�{A�bA�VA��A��A��A�VA�JA���A���A���A�  A��A��`A���A���A�ȴAϾwAϴ9Aϴ9AϮAϥ�Aϥ�Aϟ�Aϗ�Aϙ�AύPA�~�A�ffA�S�A��A���A��`A��TA��#A�AμjAΉ7A�O�A�+A�-A�&�A��A�1A���A���A��`AͶFAͰ!AͮAͥ�A͇+A�n�A�K�A�33A�{A��A̺^A̛�A̋DA̅A�~�A�hsA�dZA�Q�A�oA���A��
A���AˮA˧�A˕�AˍPA˅A�z�A�XA�&�A��A�VA�1A�
=A�%A��A��mA���AʾwAʩ�A�z�A�XA�I�A�-A� �A���AɼjA�K�A��A���Aȥ�Aȉ7A�t�A��A���A�ZA�A�A���Aƺ^A�x�A�l�A�ffA�?}A��A�ĜAŰ!Aŝ�AőhAŁA�r�A�^5A�=qA�&�A���A�A�;dA�r�A�C�A�A�ȴAPA�bNA�Q�A�K�A�%A���A���A�x�A�Q�A�O�A�;dA�7LA�/A�-A�-A�+A�"�A��A�bA�oA�JA���A��mA���A���A�ĜA��^A��RA��-A��A��A���A��\A�n�A�A�A�{A��mA���A��A�\)A�7LA���A�`BA��A��yA���A��A�&�A�ȴA���A���A��A�|�A�p�A�ZA�E�A�33A�&�A�%A�ƨA���A�~�A�;dA��A�ȴA��A�&�A��FA��DA�r�A�^5A�E�A�VA���A��A���A���A��A�l�A�dZA�XA�G�A�;dA�/A�{A�%A��A��^A�l�A�G�A�{A���A��wA��FA���A�t�A�`BA�G�A�C�A�5?A�&�A�VA��`A���A��^A���A���A��hA���A���A��PA��+A��7A��A�~�A�\)A�ȴA���A��+A�ffA�&�A��A���A��9A��A��\A��A�r�A�`BA�O�A�?}A�/A��A�%A��A��`A���A��FA���A�jA�G�A�-A��A�A��HA��^A���A��DA�`BA�?}A���A�9XA��9A�?}A���A�ȴA��-A��A���A��uA�z�A�r�A�jA�M�A�7LA�&�A�{A���A���A��A�;dA�oA�1A���A��TA�A���A�|�A�I�A�oA���A��yA���A��RA��A���A���A���A��\A�z�A�n�A�dZA�S�A�1'A�VA�JA�%A���A���A���A���A���A���A��A��^A���A���A��DA�|�A�jA�VA�G�A�A�A�5?A�33A�/A�$�A�JA��A�ȴA��-A��uA�x�A�`BA�A�A��A�A��A���A��A�~�A�A�A�A�r�A��A�jA�ZA�9XA��A���A��A�K�A�&�A�
=A��A��/A���A��RA��PA�v�A�`BA�33A�{A�A��A��
A��RA���A�~�A�ZA�I�A�"�A�{A��mA���A��FA���A��DA�l�A�C�A��A�  A��jA�p�A� �A���A��A�hsA�S�A�1A��#A��A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                    AсA�z�A�^5A�Q�A�G�A�JA���A�ȴA�p�A�S�A�M�A�K�A�5?A�&�A��A��A��A�oA�VA�JA�JA�
=A�
=A�
=A�1A�%A�A�A�A�A�A�A�A�A�A�%A�%A�1A�1A�%A�A�%A�1A�1A�1A�
=A�1A�
=A�1A�%A�A�A�A�A�%A�%A�%A�%A�%A�%A�%A�1A�oA�1'A�&�A�
=A�JA�A�"�A���A���A˓uAʇ+A���AœuA��TA�K�A��yA�XA�M�A�hsA���A�oA�E�A��A�+A���A���A���A�%A�/A��-A���A���A���A�A�I�A��yA�`BA���A�x�A��A�`BA�E�A���A��A���A��+A���A�A��
A���A�ffA�E�A�&�A�A�-A���A�\)A��A�Q�A��yA��;A�{A}��Az~�Aw��Au��ArAi7LAc�7AaA_t�A\��AZ�9AY\)AS��AS�ARE�AQ�FANr�AF��AA�A?
=A<z�A8�\A6��A6  A4��A3�A2n�A0��A/|�A.�A.bNA.{A-��A*�DA(��A&ȴA%�FA$ZA"^5A!�A!�hA!�A!`BA n�AG�AQ�A{A�A-A�hA%A�DAE�AbA�7A��A��AoA�A�jA5?Al�A��A�A�AQ�A(�AXA�A=qA�mAO�AȴA-A�A�^A�A/A�9Az�A-A��A�;A  A�A$�AJA�#A�-A��AhsA
=A
�jA
�DA
��A	�A	"�A�Al�A�HA��A�AI�AbA�A�
AƨA��A�Ap�A?}A"�AA�`AĜA�\A=qA��A7LA��A��A�mA�A j@���@��@�5?@��/@�\)@�$�@��j@��m@�33@�5?@���@�`B@�Ĝ@�@�@�V@�-@��@�F@�O�@�D@�bN@��@�dZ@�$�@��@�X@���@���@��@�Z@�|�@�+@��#@�&�@�D@� �@�@��@�!@��@ᙚ@�h@�O�@��@�j@�j@� �@߾w@޸R@��#@ݡ�@�V@ܣ�@�;d@�@�G�@���@أ�@�A�@���@�dZ@�o@�ff@���@ղ-@�z�@�ƨ@��@��@�  @�
=@ΰ!@Ώ\@͡�@�bN@�Z@�1@��@�ƨ@˝�@˅@�l�@�\)@�S�@�K�@�"�@�o@���@��H@�M�@�V@ț�@�A�@ǅ@�;d@�@�p�@�Q�@�1@��
@��@�V@�{@���@��-@�hs@�`B@�G�@�V@�A�@���@���@���@�X@��@��u@�t�@��!@�v�@�~�@�^5@���@�@�%@���@�Z@���@�o@�@�~�@�J@���@��/@�bN@��@���@�V@�^5@���@��@��@���@��@��@��^@�5?@��@��h@�7L@��j@��/@�j@��@���@��R@�5?@��@�V@���@�9X@��@��F@�o@��\@���@�/@��@��j@��@��@�ff@�M�@�O�@���@���@�b@�@��R@��\@�v�@�^5@�@��@��@��@�A�@���@�t�@�l�@�l�@�\)@�S�@�33@�"�@��@�ȴ@���@��+@�E�@�-@�{@��@���@�/@�V@���@�r�@��;@���@�\)@�33@��y@�^5@�-@�{@���@��@��T@���@�@�@��-@��h@�x�@��@���@��@��D@�j@�A�@�  @�l�@���@�n�@���@�x�@�X@�7L@�&�@��@��@�Ĝ@��9@��9@��@��@���@�r�@��@���@�;d@���@��+@�{@��7@�%@���@���@��u@�Q�@� �@���@��
@��@�dZ@�K�@�
=@���@�M�@�-@�@��#@���@�p�@�X@�/@�&�@���@���@�Z@���@�|�@�+@��y@��R@��+@�n�@�M�@��T@��h@��@���@���@�Z@�1'@� �@� �@�(�@� �@� �@�b@��m@��
@��
@��w@��P@�dZ@�K�@�"�@���@�n�@�M�@��@���@���@���@��@�7L@���@��@�(�@��m@���@���@�dZ@�C�@�+@�@��H@��+@�5?@�J@��@���@�`B@�G�@�&�@�%@��@���@���@�j@�@~�@~V@~$�@}��@|�@|��@|Z@{��@{"�@z=q@y��@yx�@yhs@x��@xr�@w�@w�P@v�+@v{@u@u�@uV@t�@t�@s�@r�@q��@qx�@q�@pb@o;d@n�@n��@n�+@nV@m/@l�j@l�j@l��@l�@l(�@k�m@kƨ@k��@k��@kt�@kS�@kC�@k33@k"�@k@j^5@j-@j�@i��@h�`@h��@h��@hĜ@hA�@g�P@f��@fȴ@f�+@f{@e��@ep�@d�j@d�D@d(�@c��@cdZ@cC�@c@b��@bn�@bM�@b-@bJ@a�^@ahs@aG�@a7L@`��@`bN@_�@_�@_l�@^��@^5?@]�h@]�@\9X@[�m@[dZ@Z��@Z=q@Y��@Y��@Y�#@Y�^@Y&�@Xr�@W�w@W+@V��@V�y@VE�@U��@UO�@T�/@S�m@Sƨ@S�@SC�@R��@R-@Q�7@P��@PbN@Pb@O�@O
=@N5?@M��@M�@M/@L�D@L1@K�F@K33@J�\@Jn�@J-@I�@I��@I%@H�u@H1'@Hb@G|�@F��@F�@F�+@FV@E�T@EV@C�
@CC�@Co@B��@B=q@A�@Ahs@A%@@�u@?�w@?��@?l�@?�@>�@>ȴ@>�R@>�+@>5?@=p�@<9X@;ƨ@;��@;dZ@;@:^5@9��@9&�@8��@8Ĝ@81'@7��@7��@7|�@7;d@6�y@6�R@6��@6v�@6V@5�@5�h@5p�@5�@4��@4��@4�D@4z�@4j@4�@3�m@3�@3C�@3"�@3o@2��@2n�@2=q@2-@2�@1��@1�^@1��@1x�@1X@1X@1X@17L@0�`@0�9@0bN@0b@/�@/\)@/
=@.��@.��@.�y@.ff@.{@-@-�h@-`B@-/@,�@,�j@,�D@,I�@+�m@+t�@+33@+@*�@*�H@*��@*�!@*n�@*J@)��@)x�@)hs@)&�@(��@(�9@(r�@(A�@(b@'�w@'l�@'K�@'+@&��@&�y@&�R@&ff@&E�@&$�@%�@%��@$��@$�@$��@$z�@$I�@$9X@$�@#��@#��@#dZ@#33@"��@"M�@!�@!��@!�^@!��@!�7@!G�@!%@ ��@ 1'@��@|�@K�@��@�R@��@ff@$�@��@�h@O�@V@�@�D@�@�m@ƨ@�F@��@��@�@dZ@33@@�H@��@�\@~�@n�@M�@J@��@hs@&�@%@��@��@��@��@��@Q�@�@��@��@|�@l�@K�@�@
=@�y@�@�R@v�@E�@@��@�-@?}@/@�@��@1@t�@t�@t�@t�@C�@"�@�H@��@�!@��@��@~�@n�@=q@�#@�^@��@��@�7@X@�@��@��@�@�@��@��@��@\)@K�@;d@�@�y@ȴ@��@��@��@��@�+@5?@�T@�-@p�@/@�@�/@��@�jG�O�AсAуA�v�Aч+AсA�z�A�z�A�x�A�l�A�VA�S�A�M�A�VA�S�A�Q�A�?}A�K�A�-A�(�A��
A���A�ȴA���A�ĜA���AЮA�t�A�p�A�ZA�S�A�S�A�Q�A�K�A�M�A�O�A�K�A�I�A�M�A�E�A�;dA�9XA�5?A�1'A�33A�-A�+A�-A�(�A�"�A�$�A��A��A��A��A��A� �A��A� �A��A��A��A��A��A��A�{A��A�{A��A��A�oA�{A�VA�bA�oA�bA�JA�oA�VA�JA�oA�bA�JA�VA�JA�
=A�VA�VA�
=A�bA�
=A�
=A�VA�
=A�
=A�VA�
=A�1A�JA�
=A�JA�JA�1A�JA�
=A�1A�JA�1A�1A�
=A�%A�1A�JA�%A�1A�JA�%A�%A�1A�A�A�%A�%A�  A�A�%A�A�%A�A�A�%A�A�A�%A�A�  A�A�%A�A�A�A�  A�%A�%A�A�%A�A�A�1A�A�A�A�  A�%A�A�  A�A�  A�A�%A�  A�A�%A�A�A�%A�A�A�1A�A�%A�%A�A�1A�A�A�1A�A�%A�1A�A�1A�
=A�%A�A�
=A�
=A�%A�
=A�
=A�%A�
=A�1A�A�%A�
=A�%A�%A�1A�A�%A�1A�A�A�%A�A�A�%A�1A�A�A�
=A�%A�%A�
=A�1A�%A�
=A�1A�%A�
=A�1A�%A�1A�JA�1A�%A�JA�1A�%A�JA�1A�1A�
=A�
=A�%A�1A�
=A�%A�
=A�
=A�1A�JA�%A�%A�JA�
=A�%A�
=A�%A�A�%A�
=A�%A�%A�1A�A�A�%A�A�A�%A�  A�A�A�  A�A�%A�  A�A�A�A�A�1A�A�A�%A�A�A�1A�A�A�%A�%A�A�%A�1A�A�%A�1A�A�A�1A�%A�A�1A�%A�A�1A�A�A�1A�%A�A�%A�1A�A�%A�1A�A�1A�1A�A�1A�
=A�A�%A�1A�A�%A�1A�A�%A�
=A�%A�A�1A�%A�A�1A�1A�A�1A�%A�A�1A�%A�A�1A�A�A�1A�%A�A�%A�1A�A�A�1A�%A�A�1A�1A�A�%A�1A�A�%A�1A�%A�A�
=A�JA�1A�1A�JA�1A�
=A�bA�VA�JA�oA�bA�{A��A��A��A��A� �A��A�&�A�(�A�$�A�(�A�7LA�7LA�?}A�E�A�E�A�A�A�E�A�E�A�A�A�E�A�E�A�=qA�+A��A�bA�VA�VA�
=A�A�A�A���A�A�1A�1A�
=A�bA�VA�
=A�VA�oA�bA�bA�{A�bA�VA��A��A��A�VA�JA���A���A���A�  A��A��`A���A���A�ȴAϾwAϴ9Aϴ9AϮAϥ�Aϥ�Aϟ�Aϗ�Aϙ�AύPA�~�A�ffA�S�A��A���A��`A��TA��#A�AμjAΉ7A�O�A�+A�-A�&�A��A�1A���A���A��`AͶFAͰ!AͮAͥ�A͇+A�n�A�K�A�33A�{A��A̺^A̛�A̋DA̅A�~�A�hsA�dZA�Q�A�oA���A��
A���AˮA˧�A˕�AˍPA˅A�z�A�XA�&�A��A�VA�1A�
=A�%A��A��mA���AʾwAʩ�A�z�A�XA�I�A�-A� �A���AɼjA�K�A��A���Aȥ�Aȉ7A�t�A��A���A�ZA�A�A���Aƺ^A�x�A�l�A�ffA�?}A��A�ĜAŰ!Aŝ�AőhAŁA�r�A�^5A�=qA�&�A���A�A�;dA�r�A�C�A�A�ȴAPA�bNA�Q�A�K�A�%A���A���A�x�A�Q�A�O�A�;dA�7LA�/A�-A�-A�+A�"�A��A�bA�oA�JA���A��mA���A���A�ĜA��^A��RA��-A��A��A���A��\A�n�A�A�A�{A��mA���A��A�\)A�7LA���A�`BA��A��yA���A��A�&�A�ȴA���A���A��A�|�A�p�A�ZA�E�A�33A�&�A�%A�ƨA���A�~�A�;dA��A�ȴA��A�&�A��FA��DA�r�A�^5A�E�A�VA���A��A���A���A��A�l�A�dZA�XA�G�A�;dA�/A�{A�%A��A��^A�l�A�G�A�{A���A��wA��FA���A�t�A�`BA�G�A�C�A�5?A�&�A�VA��`A���A��^A���A���A��hA���A���A��PA��+A��7A��A�~�A�\)A�ȴA���A��+A�ffA�&�A��A���A��9A��A��\A��A�r�A�`BA�O�A�?}A�/A��A�%A��A��`A���A��FA���A�jA�G�A�-A��A�A��HA��^A���A��DA�`BA�?}A���A�9XA��9A�?}A���A�ȴA��-A��A���A��uA�z�A�r�A�jA�M�A�7LA�&�A�{A���A���A��A�;dA�oA�1A���A��TA�A���A�|�A�I�A�oA���A��yA���A��RA��A���A���A���A��\A�z�A�n�A�dZA�S�A�1'A�VA�JA�%A���A���A���A���A���A���A��A��^A���A���A��DA�|�A�jA�VA�G�A�A�A�5?A�33A�/A�$�A�JA��A�ȴA��-A��uA�x�A�`BA�A�A��A�A��A���A��A�~�A�A�A�A�r�A��A�jA�ZA�9XA��A���A��A�K�A�&�A�
=A��A��/A���A��RA��PA�v�A�`BA�33A�{A�A��A��
A��RA���A�~�A�ZA�I�A�"�A�{A��mA���A��FA���A��DA�l�A�C�A��A�  A��jA�p�A� �A���A��A�hsA�S�A�1A��#A��A�|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B1�B2�B2�B0�B/B0�B/OB-CB1�B+kB+6B+B*�B)�B)*B)*B(�B(�B(�B(�B(�B(�B(XB(XB(�B(XB($B($B($B($B(�B(�B(�B(�B(�B)*B(�B)*B(�B(�B(�B)*B(�B)_B)_B)*B)�B)_B)�B(�B(�B(�B(�B)*B)�B)�B)�B)�B)�B)�B)�B,qB3�BH�B^jBn/B��B�+B��B��B��B��B��B��B��BܒB�&B�B�B�B�B 'B,B/OB<�B@�BB[BG�BM6BL�BNpBQBMBN<BLdBHBFBB�B?HB:�B6zB#�B �BBhB��B�BΥB�OB��B��Bn�Bi�Bg�Bc�B_�B[�BC�BIB
��B
͟B
��B
w�B
T�B
E�B
6FB
VB
MB	��B	�pB	��B	�zB	�7B	�(B	��B	��B	kB	cTB	_pB	WsB	S[B	@B	-�B	%zB	#B	"hB	�B	1B	B	�B	oB	hB	�B	�B	VB	�B	�B	4B	 B	�B	,=B	=�B	V�B	[�B	\�B	\�B	]dB	g�B	n�B	o B	poB	zxB	o�B	v�B	xlB	{JB	}VB	��B	��B	�JB	�B	� B	�B	��B	�	B	�fB	�~B	��B	�FB	�'B	��B	�aB	�B	�FB	�*B	��B	�UB	��B	�OB	��B	�tB	�B	��B	��B	��B	�XB	�)B	�pB	� B	�,B	�B	�9B	��B	�gB	�mB	��B	ѷB	�BB	��B	�pB	�5B	�B	�B	�B	� B	�B	�&B	��B	��B	��B	�B	�`B	��B	��B	�fB	�fB	�B	�B	�fB	�2B	�fB	�8B	�sB	�>B	�>B	�B	�B	�KB	�B	�yB	�B	�B	�B	��B	�fB	��B	�ZB	��B	��B	�`B	�B	�B	�B	�B	�B	�B	�B	�;B	�5B	� B	�iB	�oB	��B	�AB	�B	�B	�B	�vB	��B	�B	��B	�TB	��B	��B	��B	�2B	��B	�B	��B	��B	�lB	�>B	��B	�	B	�lB	�lB	�fB	��B	��B	�2B	��B	�8B	��B
 iB
B
B
oB
�B
uB
B
�B
�B
MB
�B
�B
 iB
{B
oB
B	��B	��B	��B	�JB	��B	��B	�rB	�B	�]B
;B
AB
AB
�B
�B
+B
1B
�B
�B
�B

�B
B
xB
�B
xB

�B

rB
JB
�B
SB
�B
{B
AB
B
�B
B
�B
B
�B
�B
MB
B
�B
B
B
�B
�B
�B
�B
�B
fB
	�B
	B
	B

=B

�B
�B
~B
JB
�B
"B
�B
"B
(B
\B
�B
B
�B
uB
�B
B
 'B
 'B
 �B
qB
�B
OB
 �B
VB
VB
�B
�B
!bB
$tB
%zB
#�B
"4B
#B
#nB
#nB
#nB
#B
#nB
#B
#nB
%B
#�B
$@B
$�B
$�B
&B
#nB
"�B
#B
 �B
 \B
 �B
 �B
!-B
"�B
#�B
&B
(�B
*0B
+6B
+�B
,�B
.�B
/OB
0!B
0�B
1'B
1�B
1�B
1�B
3�B
5B
5�B
6B
7B
7LB
7�B
7�B
8B
8�B
8�B
8�B
9$B
:*B
:�B
:�B
:�B
;0B
<6B
<jB
<jB
<�B
<�B
<�B
=<B
=qB
=�B
>BB
>�B
?B
@B
@B
@OB
@OB
@�B
@�B
@�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D3B
D3B
D�B
EB
D�B
D�B
D�B
D�B
D�B
E9B
E�B
FtB
GzB
G�B
G�B
H�B
I�B
JXB
J#B
JXB
J�B
K)B
K^B
K�B
K�B
L0B
LdB
LdB
L�B
MjB
M�B
M�B
NB
NB
N�B
N�B
N�B
N�B
NpB
N�B
NpB
OBB
O�B
PB
P�B
QB
Q�B
Q�B
Q�B
R B
R�B
S&B
S�B
S�B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
T�B
U2B
UgB
UgB
UgB
U�B
VB
V9B
VmB
V�B
XB
W�B
XB
XEB
X�B
X�B
X�B
XyB
YKB
YB
YB
Z�B
Y�B
Y�B
ZQB
Z�B
ZQB
Z�B
Z�B
Z�B
[�B
[�B
\�B
]/B
\�B
]�B
^�B
^�B
_B
^�B
^�B
^�B
^�B
_�B
_�B
`B
`B
aB
a|B
a|B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
c B
c�B
c�B
d&B
dZB
c�B
c�B
c�B
d�B
d&B
d�B
dZB
dZB
e�B
e�B
e�B
ffB
e�B
ffB
ffB
e�B
e�B
gmB
g8B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
h
B
h
B
h
B
h
B
h
B
h
B
h�B
h�B
hsB
iB
i�B
iyB
iDB
iB
i�B
jKB
jB
jB
j�B
kB
j�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m]B
m�B
n/B
n/B
ncB
o B
o5B
o5B
oiB
o�B
o�B
p�B
p�B
qB
qB
qAB
q�B
q�B
q�B
rB
rB
q�B
q�B
rGB
r�B
s�B
s�B
s�B
s�B
t�B
u%B
u%B
u�B
u�B
u�B
v+B
v`B
v�B
w�B
x�B
y	B
y�B
y�B
zB
{B
{�B
|B
{�B
|PB
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~]B
~�B
~]B
cB
cB
�B
�B
�B
�4B
�B
�B
�AB
�AB
��B
��B
�{B
��B
�B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�1B
�1B
�fB
�B
�	B
��B
�DB
�xB
��B
�~B
��B
�PB
��B
�"B
��B
��B
��B
��B
��B
�\B
��B
��B
�.B
�.B
�bB
�bB
�bB
�bB
��B
��B
�4B
�hB
�hB
�hB
��B
�B
��B
��B
��B
��B
�:B
�:B
�:B
�oB
�oB
�:B
�:B
��B
�:B
��B
��B
�B
�@B
�uB
�@B
�@B
�@B
��B
��B
�FB
�{B
��B
��B
�B
�MB
�MB
�MB
��B
��B
��B
�B
�B
�B
��B
��B
�SB
��B
�$B
��B
�YB
��B
��B
�+B
�+B
�_B
�+B
��B
��B
��B
��B
��B
��B
��B
�1B
�1B
�1B
�1B
��B
��B
��B
��B
�	B
�=B
�	B
�=B
�=B
��B
��B
��B
�CB
��B
�B
��B
�B
��B
�B
�~B
�~B
��B
�B
�OB
��B
��B
��B
��B
�!B
�VB
��B
��B
�'B
�'B
�\B
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
�hB
�hB
�hB
��B
��B
��B
��B
��B
�nB
��B
��B
�tB
�@B
�tB
�tB
�@B
��B
��B
�B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�$B
�$B
�$B
��B
��B
�0B
��B
��B
��B
�0B
�eB
��B
�B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
�B
�=B
�qB
�=B
�=B
��B
�wB
�CB
�CB
��B
��B
��B
��B
��B
�B
�B
�IB
�}B
�IB
�IB
�IB
��B
�B
�OB
�OB
��B
�UB
�UB
�!B
��B
��B-CB-CB4B0UB1�B33B1�B2-B5B2�B2aB0�B/�B0�B)�B8�B&�B4nB/�B6�B:�B$@B2�B*�B1�B6�B9XB,=B,�B+6B*�B*0B+kB+6B)�B+kB+�B*0B,=B,�B)�B)�B+B)�B)_B*�B(�B)_B*eB($B)�B)*B(�B)�B($B(�B)�B'�B)_B(�B(�B*0B'�B($B)�B'�B)�B($B(�B)�B'�B)_B)_B'�B(�B)�B'�B(�B)�B'�B($B)�B'�B($B)�B'�B($B)_B'�B(�B)*B'�B(�B)_B'RB(�B)_B'�B)*B'�B'�B)_B'�B(�B)_B'B)*B(�B'�B)�B(�B'�B)�B(�B'�B(�B(�B'RB(XB)�B'�B'�B)_B'�B'RB)*B'RB($B(�B'B'�B)_B'�B'�B(�B'�B'�B)*B'�B($B)_B'RB'�B(�B'RB)*B)*B'�B)*B(�B(XB'B'RB(�B)�B'�B)�B(�B'�B)�B(�B($B)�B)�B'�B)*B)�B($B)�B(XB(�B)�B($B)_B)�B($B)�B)*B'�B)_B)*B($B)�B)�B($B(XB*0B(�B)*B*�B(�B)�B*0B(�B'�B)�B)�B&LB'�B)*B'�B(�B'�B'�B(XB)�B(�B'�B)�B*0B(XB)_B)�B'�B)*B*eB(XB)*B*0B'�B)_B*�B)�B(XB)_B)�B($B)*B*0B($B)�B)�B(�B(�B*eB)�B(�B*0B(�B)_B*eB(XB*�B)�B($B)�B*�B(�B)�B+B)�B($B)�B)�B(�B(�B(�B'�B(�B)�B'�B)�B(�B'�B)�B(�B($B*eB(XB($B)�B)_B'�B)_B*0B(�B)*B)�B(XB)_B*eB(�B)*B*0B)_B(�B*eB)�B(�B*0B*0B(�B)�B+B(�B)�B*�B(�B*�B*�B(�B)�B+6B)�B)_B+6B)�B)_B+6B)*B)_B+B)_B(�B+6B)�B)*B+B)�B)_B+B*�B(�B*�B+kB)�B)�B+B(�B)*B+6B)*B)�B+B)*B)�B+6B(�B*�B*�B(�B)�B+B)�B)_B+B*�B(�B)�B+B)*B)_B+B*eB)�B+�B+kB*eB,B.IB-�B-�B0!B/�B.}B0!B0�B/�B2�B3�B2�B3�B4�B7�B7�B:�B;dB<�B=�B=qB?�BB�BB'BE�BO�BT�BU�BY�BZ�BX�BZ�B\]B[�B[#B]dBbNB`�B`vB_�B^jB`�Ba|B_;B^B`vBd�Be`Bj�Bs�Bt�Bv�By	Bx8BzxBz�B{�BzxB{�B|B}VB|�B�oB�1B��B�xB�=B�JB��B��B��B��B�1B��B�	B��B�YB��B�1B��B��B�eB��B�$B�B��B�B��B��B�	B�_B�=B��B�B�B��B��B�JB��B��B��B��B�YB��B�\B�'B�OB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�RB��B�qB�XB��B��B�6B��B��B�B��B��B��B�eB��B�0B�XB��B��B��B��B�B��B��B�B��B�B�B�UB�-B��B�B�B�0B�B��B��B�zB�HB��BΥBŢB�^B��B��B�jB�pB��B�)B�XBǮBɺBɺB��B�)B�KB�BBѷB�;B�TB��B�]B�dBںB�/BںB�yB�
B�B�vB�B�B� B��B� B��B�B�B�B�ZB�`B�8B�ZB�B�B�
B��B�2B�>B�B��B�2B�B�B��B�B�QB�]B�]B��B�fB�B�B�B�PB	lB�B�]B{BB�BB�BVB�BJB�BbBB�B�BBkBSB�B \B �B�B!�B,�B/�B(�B&LB*0B)�B0UB1'B/�B/OB+kB.�B/OB-�B-wB/�B/OB.IB1�B0�B2�B5tB?HB:�BB'B>�B;�B;0BB�B@�B?HB?}B>BBB�BAUBB�BC�BB[BE�BB�BB�BB�B@B@OBB'BA�B?B?B@BGEBR�BH�BJXBK�BPBQNBN�BI�BJ�BM�BK�BJ�BL�BLdBK�BJ�BM6BM6BL�BK�BM�BM�BP}BQ�BNBOBK�BJ�BNpBOBHBG�BK^BK)BR�BZ�BX�BV�BQ�BOvBM�BK)BJXBMBN�BK^BK^BMjBM�BJ#BK)BMBO�BT,BRTBMBJXBI�BL0BMBK^BJ#BT�BJ�BH�BJXBJ�BGzBH�BF?BD�BFtBFtBF�BF�BE�BGEBGEBDgB>�BE�BC�BB'BA�BA�BA�BA�BDgBAUBB'B@�B>�B?�B@�B@�B>�B<jB=�B<6B:�B;dB=�B=�B<6B9�B:�B9$B6�B7�B8�B49B3�B3�B2-B4nB/�B-�BJXB8B!�B#nB#�B&�B&LB'RB#�B!�B�B �B!�B�B 'B%FB$tB#nB!�B �B�B�B�B�B�B�B�B�B"�B�B�B�BBB{B�BFB B�BoB B�B	�B	B�B��B 4B��B�2B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2022011603045320220116030453IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012523323520220125233235QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022012523323520220125233235QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365720220126093657IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423300020220204233000IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                