CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-06-27T15:24:13Z creation; 2022-02-04T23:30:01Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � aP   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � i8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �$   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �@   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210627152413  20220204223514  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_174                 6810_008521_174                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��Q�R@��Q�R11  @�����@�����@2"��9C@2"��9C�e��n��e��n�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?�@=p�@}p�@��R@��R@޸R@�p�A\)A ��A+�A?\)A_\)A\)A�  A�Q�A�Q�A�  AУ�A���A��B (�B  B�
B�B�B(  B0(�B7�
B?�
BG�
BP  BX  B`(�Bh  Bo�
Bx  B�  B�  B�  B��B�{B�{B�  B�  B�  B�  B��B�{B�{B�{B�(�B�{B�  B�  B��B�  B�{B�{B�{B��B�  B�  B�  B��B�  B��
B��B�  C 
=C{C{C
=C
=C
  C
=C  C��C  C{C
=C��C�C�C�C�C!��C${C&
=C(  C*  C,
=C-��C0
=C2
=C4  C6  C8  C9��C<  C>{C@
=CB{CD  CF  CG��CJ
=CL  CM��CP{CR{CT
=CU��CX
=CZ{C\
=C^  C`  Cb{Cd  Cf  Ch
=Cj  Ck��Cn  Co��Cr
=Cs��Cu�Cx
=Cz{C|
=C}��C�  C�
=C�  C���C�  C�  C�
=C�C���C�C���C���C���C�  C�  C�  C�  C�C���C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C�C�  C���C���C�C�
=C�  C�  C�  C�  C���C�  C�C���C�  C�C�C�  C���C�  C�C���C�C���C�  C�C���C�  C���C���C���C���C�  C�C���C�C�  C�  C�  C�  C�  C�  C�  C�
=C�C���C�C���C�C���C�  C���C�  C�C�C�  C�C�  C�C�C��C��C���C�  C���C�  C�  C���C���C���C�C�C�  C�  C�  C���C���C���C���C�  C�  C�C�C�\C�
=C���C�  C�C�  D   D ��D�D}qD��D��DD}qD�D� D�D��D�D}qD�qD� D�D��D�qD	}qD
  D
� D�D��D�D� D  Dz�D��D}qD�qD}qD  D��D  D��D�qDz�D  D� D  Dz�D�qD� D  D� D�D� D�D��D  D��D�D��D  D��D�D� D�D��D�Dz�D  D�D �D � D �qD!� D"�D"��D#�D#� D$  D$��D%�D%� D&�D&��D'�D'}qD'��D(z�D(�qD)� D)�qD*� D+�D+� D,  D,� D,�qD-}qD.  D.� D.�qD/}qD/�qD0� D1  D1}qD2  D2��D3�D3� D4  D4}qD4��D5� D6  D6}qD7  D7� D8  D8� D9�D9}qD9��D:}qD;�D;� D;�qD<��D=D=��D>  D>}qD?  D?��D@�D@� DA  DA� DA�qDB}qDC  DC� DD  DD��DE�DE��DE�qDF� DG  DG}qDH  DH� DI  DI��DI�qDJ}qDJ�qDK}qDL  DL� DL�qDM}qDN  DN� DO  DO��DP�DP� DP�qDQ� DR�DRz�DS  DS�DT  DT� DU�DU� DV  DV}qDV�qDW}qDW�qDX� DX�qDY��DZ  DZ� D[  D[}qD[�qD\� D]D]� D^�D^�D_�D_��D`�D`��DaDa� Da��Db��Dc�Dc��Dd  Dd}qDe�De� Df  Df��Dg  Dg��Dh�Dhz�Dh��Di}qDi��Dj}qDk  Dk� Dk�qDl��Dm�Dm� Dn  Dn}qDo  Do�Dp�Dp� Dp�qDqz�Dr  Dr� Ds�Ds� Ds��Dt}qDt�qDu� Du�qDv� Dw  Dw� Dx�Dx��Dy�Dy� Dz�Dz� D{  D{}qD{��D|}qD}�D}}qD}��D~}qD~�qDz�D�HD�AHD��HD�D�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�D���D�@ D�� D��qD�  D�@ D��HD��HD�HD�>�D�~�D���D�  D�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D�� D���D�HD�@ D�~�D�� D�HD�AHD��HD��HD���D�>�D�� D��HD�HD�@ D�� D�� D���D�@ D�~�D���D���D�>�D�~�D���D��qD�@ D���D�� D�  D�@ D�� D�� D��D�@ D�� D�D��D�@ D�� D��HD�HD�B�D�� D�� D���D�@ D�� D��qD�  D�B�D�� D�� D���D�>�D�~�D���D���D�>�D�~�D�� D��D�B�D��HD��HD�HD�>�D�~�D���D���D�AHD��HD�� D�HD�AHD�� D��HD���D�>�D���D�� D���D�@ D�}qD��qD���D�>�D�� D���D�HD�B�D��HD�� D�  D�AHD��HD��HD��D�B�D���D�� D�  D�@ D�~�D�� D���D�@ D��HD���D��qD�AHD�� D��qD�  D�>�D�}qD���D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�� D��HD�HD�AHD�}qD���D�HD�AHD�~�D���D�HD�@ D�~�D���D���D�AHD�}qD�� D��D�@ D�~�D�� D���D�AHD��HD���D�  D�@ D�~�D��HD�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�>�D�� D���D���D�>�D�~�D��qD���D�AHD���D�� D���D�@ D�� D���D�  D�>�D�~�D�� D���D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�� D�� D�HD�@ D�~�D�� D���D�@ D���D��HD�  D�AHD D�� D�  D�>�DÀ D��HD�  D�@ D�~�D�� D�  D�>�Dŀ DŽqD�  D�AHDƁHD��HD�  D�@ Dǂ�D��HD�  D�@ DȀ D��HD�HD�>�D�~�D��HD�HD�>�Dʀ D��HD�HD�>�D�}qD˾�D�  D�>�D̀ D̾�D���D�@ D�~�D�� D�HD�>�D΀ D�� D���D�AHDρHD�� D��D�@ D�~�DнqD���D�AHDт�D�� D�  D�@ DҀ DҾ�D���D�@ DӀ D��HD�  D�>�D�}qDԾ�D�HD�AHDՀ D�� D���D�@ Dւ�D�D�  D�>�D�}qD׽qD��qD�=qD؀ D��HD�HD�@ D�~�D�� D�  D�AHDڀ Dھ�D�  D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�@ D݁HD��HD��qD�>�Dހ D��HD�  D�AHD߁HD��HD�  D�@ D�� D��HD��D�B�D�HD��HD�  D�>�D� D��HD�HD�AHD� D��HD��D�AHD�HD��HD�  D�@ D�HD��HD�  D�>�D� D澸D���D�AHD�HD��HD�HD�@ D�HD�� D���D�@ D� D龸D�HD�AHD�~�D꾸D�  D�AHD� D��HD��D�>�D�}qD쾸D�HD�AHD� D�� D�HD�>�D� D��HD�  D�AHD�HD�� D���D�@ D��HD��HD���D�@ D� D�D���D�>�D�~�D�� D���D�>�D� D��HD�  D�>�D� D���D�  D�B�D��HD�� D���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�B�D��HD��HD�  D�>�D�~�D�� D�  D�@ D��HD��HD��D�B�?\)?.{?aG�?�\)?���?�(�?��H@
=q@#�
@5@B�\@W
=@n{@}p�@�ff@�33@��H@�G�@���@�
=@�p�@�ff@��@��H@�G�@���@�@�(�A33AQ�A
�HA  A�A��A��A!�A&ffA)��A-p�A333A7
=A9��A?\)ADz�AG�AK�AQG�AUAY��A^{Ac�
Ag
=Ak�AqG�AuAx��A~{A��A��A�A���A�33A���A�
=A��A�(�A�A�Q�A�33A��A��RA���A�(�A�A��A��\A��A�
=A�Q�A�33A�A�\)A�G�A�(�A�{A��A\A��AƸRAȣ�A�33A�A�\)Aљ�A�z�A�ffA�  A��HA�p�A�
=A�G�A��
A�ffA�  A�=qA���A�
=A��A�33A�A��A�G�A��
A��RB (�B ��BffB�BQ�Bp�B�HB�
B��B	�B33B(�B�B�\B�B��B��B
=B  B��B�B\)BQ�BG�B=qB�B��Bp�B�\B   B ��B!B"�RB$(�B%G�B%�B&�HB((�B)��B*ffB+\)B,z�B-�B.�HB/�B0��B2ffB3\)B4(�B5p�B6�RB7�B8��B9�B;33B<  B<��B>=qB?�B@Q�BAG�BB�HBC�
BD��BE��BG
=BH  BH��BIBK33BLQ�BM�BN{BO�BP��BQ��BR�\BS�BT��BV{BW
=BW�
BX��BZffB[\)B\(�B]G�B^�RB_�B`z�Ba�Bc
=Bd  Bd��BfffBg�Bhz�Bip�Bj�RBl(�Bl��Bm�Bo33Bp��Bq��BrffBs�Bt��Bv{Bw
=Bx  ByG�Bz�RB|  B}�B~{B�B�Q�B�
=B��B�{B���B��B�{B��\B�33B��B���B�33B��B�Q�B��B��
B�z�B���B��B�=qB���B��B�{B��\B�G�B�  B���B�33B��B�ffB��B�B�(�B���B���B�=qB���B�G�B�  B���B�\)B��B�ffB���B���B�Q�B�
=B��B�  B��RB�p�B�{B���B�33B��B��RB�33B�B�ffB�33B��
B�Q�B��HB��B�Q�B���B��B�  B���B�\)B�{B���B�p�B��B�z�B��B�B�z�B�
=B��B�(�B��HB��B�{B��RB�G�B��
B��\B�33B��B���B��B��B�Q�B�
=B�B�Q�B��HB�p�B�  B���B�p�B�  B��\B��B��
B\B�
=BÅB�(�B��HBř�B�(�Bƣ�B�33B��
Bȏ\B�G�B�B�Q�B��HB˙�B�Q�B���B�G�B��BΏ\B�33B��
B�Q�B���B�G�B��
B�z�B�33BӮB�(�Bԣ�B�G�B�  B֣�B�G�B׮B�(�B��HBٙ�B�(�Bڣ�B��B�B�z�B�
=B݅B�{B޸RB�\)B��B�ffB��HB�p�B�{B���B�33B㙚B�Q�B���B�\)B��
B�z�B��B癚B�  B�\B�33B��
B�Q�B�RB�G�B��B�\B�
=B�B�  B�RB�\)B��
B�=qB��HB�B�(�B�\B�G�B��B�ffB���B��B�Q�B���B�G�B�  B��RB�p�B��B�z�B��B��B���B��B�B�z�B�G�B�C G�C �C  CG�C��C  CffC�C��CffCC  CffC��C�CffC��C(�Cz�C��C(�C�\C�C33Cz�C�HC	G�C	�C	�C
Q�C
��C
�C\)C�RC  CQ�C�RC33Cz�CC(�C��C�C33C�\C��C\)C�RC  CQ�C�RC�Cp�CC(�C��C�HC33C��C  CQ�C��C  Cp�C�RC
=Cz�C�
C�Cz�C�HC=qC�\C�HC33C��C  CQ�C��C
=CffC�C  CffC��C(�Cp�C��C=qC�\C�HC 33C ��C!  C!\)C!��C"  C"p�C"C#{C#ffC#��C$33C$�C$��C%�C%�C%��C&=qC&�C&�
C'=qC'��C'��C(33C(�\C(��C)Q�C)��C)�HC*=qC*�\C+  C+\)C+�\C+�HC,G�C,�C,�C-33C-�\C-��C.G�C.�C.��C/(�C/�\C/�C033C0z�C0��C1(�C1�C1�
C2�C2ffC2��C3(�C3z�C3C4{C4\)C4�RC5{C5z�C5�RC6  C6\)C6C7(�C7p�C7C8
=C8ffC8�RC9{C9p�C9�C:  C:Q�C:��C;
=C;ffC;�RC<  C<Q�C<��C<�C=G�C=��C=��C>G�C>�\C>��C?�C?z�C?��C@{C@Q�C@��CA  CAQ�CA�CB
=CBQ�CB��CB�HCC=qCC��CC��CD=qCD�CD�
CE33CE�\CE�CFG�CF��CF�CG33CG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ?k�?�@=p�@}p�@��R@��R@޸R@�p�A\)A ��A+�A?\)A_\)A\)A�  A�Q�A�Q�A�  AУ�A���A��B (�B  B�
B�B�B(  B0(�B7�
B?�
BG�
BP  BX  B`(�Bh  Bo�
Bx  B�  B�  B�  B��B�{B�{B�  B�  B�  B�  B��B�{B�{B�{B�(�B�{B�  B�  B��B�  B�{B�{B�{B��B�  B�  B�  B��B�  B��
B��B�  C 
=C{C{C
=C
=C
  C
=C  C��C  C{C
=C��C�C�C�C�C!��C${C&
=C(  C*  C,
=C-��C0
=C2
=C4  C6  C8  C9��C<  C>{C@
=CB{CD  CF  CG��CJ
=CL  CM��CP{CR{CT
=CU��CX
=CZ{C\
=C^  C`  Cb{Cd  Cf  Ch
=Cj  Ck��Cn  Co��Cr
=Cs��Cu�Cx
=Cz{C|
=C}��C�  C�
=C�  C���C�  C�  C�
=C�C���C�C���C���C���C�  C�  C�  C�  C�C���C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�  C�C�  C���C���C�C�
=C�  C�  C�  C�  C���C�  C�C���C�  C�C�C�  C���C�  C�C���C�C���C�  C�C���C�  C���C���C���C���C�  C�C���C�C�  C�  C�  C�  C�  C�  C�  C�
=C�C���C�C���C�C���C�  C���C�  C�C�C�  C�C�  C�C�C��C��C���C�  C���C�  C�  C���C���C���C�C�C�  C�  C�  C���C���C���C���C�  C�  C�C�C�\C�
=C���C�  C�C�  D   D ��D�D}qD��D��DD}qD�D� D�D��D�D}qD�qD� D�D��D�qD	}qD
  D
� D�D��D�D� D  Dz�D��D}qD�qD}qD  D��D  D��D�qDz�D  D� D  Dz�D�qD� D  D� D�D� D�D��D  D��D�D��D  D��D�D� D�D��D�Dz�D  D�D �D � D �qD!� D"�D"��D#�D#� D$  D$��D%�D%� D&�D&��D'�D'}qD'��D(z�D(�qD)� D)�qD*� D+�D+� D,  D,� D,�qD-}qD.  D.� D.�qD/}qD/�qD0� D1  D1}qD2  D2��D3�D3� D4  D4}qD4��D5� D6  D6}qD7  D7� D8  D8� D9�D9}qD9��D:}qD;�D;� D;�qD<��D=D=��D>  D>}qD?  D?��D@�D@� DA  DA� DA�qDB}qDC  DC� DD  DD��DE�DE��DE�qDF� DG  DG}qDH  DH� DI  DI��DI�qDJ}qDJ�qDK}qDL  DL� DL�qDM}qDN  DN� DO  DO��DP�DP� DP�qDQ� DR�DRz�DS  DS�DT  DT� DU�DU� DV  DV}qDV�qDW}qDW�qDX� DX�qDY��DZ  DZ� D[  D[}qD[�qD\� D]D]� D^�D^�D_�D_��D`�D`��DaDa� Da��Db��Dc�Dc��Dd  Dd}qDe�De� Df  Df��Dg  Dg��Dh�Dhz�Dh��Di}qDi��Dj}qDk  Dk� Dk�qDl��Dm�Dm� Dn  Dn}qDo  Do�Dp�Dp� Dp�qDqz�Dr  Dr� Ds�Ds� Ds��Dt}qDt�qDu� Du�qDv� Dw  Dw� Dx�Dx��Dy�Dy� Dz�Dz� D{  D{}qD{��D|}qD}�D}}qD}��D~}qD~�qDz�D�HD�AHD��HD�D�  D�>�D�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�D���D�@ D�� D��qD�  D�@ D��HD��HD�HD�>�D�~�D���D�  D�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D�� D�� D�HD�>�D�� D�� D��qD�@ D�� D���D�HD�@ D�~�D�� D�HD�AHD��HD��HD���D�>�D�� D��HD�HD�@ D�� D�� D���D�@ D�~�D���D���D�>�D�~�D���D��qD�@ D���D�� D�  D�@ D�� D�� D��D�@ D�� D�D��D�@ D�� D��HD�HD�B�D�� D�� D���D�@ D�� D��qD�  D�B�D�� D�� D���D�>�D�~�D���D���D�>�D�~�D�� D��D�B�D��HD��HD�HD�>�D�~�D���D���D�AHD��HD�� D�HD�AHD�� D��HD���D�>�D���D�� D���D�@ D�}qD��qD���D�>�D�� D���D�HD�B�D��HD�� D�  D�AHD��HD��HD��D�B�D���D�� D�  D�@ D�~�D�� D���D�@ D��HD���D��qD�AHD�� D��qD�  D�>�D�}qD���D���D�>�D�� D��HD�  D�AHD�� D�� D�  D�AHD�~�D�� D���D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�� D��HD�HD�AHD�}qD���D�HD�AHD�~�D���D�HD�@ D�~�D���D���D�AHD�}qD�� D��D�@ D�~�D�� D���D�AHD��HD���D�  D�@ D�~�D��HD�  D�AHD��HD�� D�HD�@ D�~�D���D�  D�>�D�� D���D���D�>�D�~�D��qD���D�AHD���D�� D���D�@ D�� D���D�  D�>�D�~�D�� D���D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�� D�� D�HD�@ D�~�D�� D���D�@ D���D��HD�  D�AHD D�� D�  D�>�DÀ D��HD�  D�@ D�~�D�� D�  D�>�Dŀ DŽqD�  D�AHDƁHD��HD�  D�@ Dǂ�D��HD�  D�@ DȀ D��HD�HD�>�D�~�D��HD�HD�>�Dʀ D��HD�HD�>�D�}qD˾�D�  D�>�D̀ D̾�D���D�@ D�~�D�� D�HD�>�D΀ D�� D���D�AHDρHD�� D��D�@ D�~�DнqD���D�AHDт�D�� D�  D�@ DҀ DҾ�D���D�@ DӀ D��HD�  D�>�D�}qDԾ�D�HD�AHDՀ D�� D���D�@ Dւ�D�D�  D�>�D�}qD׽qD��qD�=qD؀ D��HD�HD�@ D�~�D�� D�  D�AHDڀ Dھ�D�  D�=qD�~�D�� D�  D�@ D�~�D�� D�  D�@ D݁HD��HD��qD�>�Dހ D��HD�  D�AHD߁HD��HD�  D�@ D�� D��HD��D�B�D�HD��HD�  D�>�D� D��HD�HD�AHD� D��HD��D�AHD�HD��HD�  D�@ D�HD��HD�  D�>�D� D澸D���D�AHD�HD��HD�HD�@ D�HD�� D���D�@ D� D龸D�HD�AHD�~�D꾸D�  D�AHD� D��HD��D�>�D�}qD쾸D�HD�AHD� D�� D�HD�>�D� D��HD�  D�AHD�HD�� D���D�@ D��HD��HD���D�@ D� D�D���D�>�D�~�D�� D���D�>�D� D��HD�  D�>�D� D���D�  D�B�D��HD�� D���D�@ D�� D��HD�HD�@ D�~�D�� D�HD�B�D��HD��HD�  D�>�D�~�D�� D�  D�@ D��HD��HD��G�O�?\)?.{?aG�?�\)?���?�(�?��H@
=q@#�
@5@B�\@W
=@n{@}p�@�ff@�33@��H@�G�@���@�
=@�p�@�ff@��@��H@�G�@���@�@�(�A33AQ�A
�HA  A�A��A��A!�A&ffA)��A-p�A333A7
=A9��A?\)ADz�AG�AK�AQG�AUAY��A^{Ac�
Ag
=Ak�AqG�AuAx��A~{A��A��A�A���A�33A���A�
=A��A�(�A�A�Q�A�33A��A��RA���A�(�A�A��A��\A��A�
=A�Q�A�33A�A�\)A�G�A�(�A�{A��A\A��AƸRAȣ�A�33A�A�\)Aљ�A�z�A�ffA�  A��HA�p�A�
=A�G�A��
A�ffA�  A�=qA���A�
=A��A�33A�A��A�G�A��
A��RB (�B ��BffB�BQ�Bp�B�HB�
B��B	�B33B(�B�B�\B�B��B��B
=B  B��B�B\)BQ�BG�B=qB�B��Bp�B�\B   B ��B!B"�RB$(�B%G�B%�B&�HB((�B)��B*ffB+\)B,z�B-�B.�HB/�B0��B2ffB3\)B4(�B5p�B6�RB7�B8��B9�B;33B<  B<��B>=qB?�B@Q�BAG�BB�HBC�
BD��BE��BG
=BH  BH��BIBK33BLQ�BM�BN{BO�BP��BQ��BR�\BS�BT��BV{BW
=BW�
BX��BZffB[\)B\(�B]G�B^�RB_�B`z�Ba�Bc
=Bd  Bd��BfffBg�Bhz�Bip�Bj�RBl(�Bl��Bm�Bo33Bp��Bq��BrffBs�Bt��Bv{Bw
=Bx  ByG�Bz�RB|  B}�B~{B�B�Q�B�
=B��B�{B���B��B�{B��\B�33B��B���B�33B��B�Q�B��B��
B�z�B���B��B�=qB���B��B�{B��\B�G�B�  B���B�33B��B�ffB��B�B�(�B���B���B�=qB���B�G�B�  B���B�\)B��B�ffB���B���B�Q�B�
=B��B�  B��RB�p�B�{B���B�33B��B��RB�33B�B�ffB�33B��
B�Q�B��HB��B�Q�B���B��B�  B���B�\)B�{B���B�p�B��B�z�B��B�B�z�B�
=B��B�(�B��HB��B�{B��RB�G�B��
B��\B�33B��B���B��B��B�Q�B�
=B�B�Q�B��HB�p�B�  B���B�p�B�  B��\B��B��
B\B�
=BÅB�(�B��HBř�B�(�Bƣ�B�33B��
Bȏ\B�G�B�B�Q�B��HB˙�B�Q�B���B�G�B��BΏ\B�33B��
B�Q�B���B�G�B��
B�z�B�33BӮB�(�Bԣ�B�G�B�  B֣�B�G�B׮B�(�B��HBٙ�B�(�Bڣ�B��B�B�z�B�
=B݅B�{B޸RB�\)B��B�ffB��HB�p�B�{B���B�33B㙚B�Q�B���B�\)B��
B�z�B��B癚B�  B�\B�33B��
B�Q�B�RB�G�B��B�\B�
=B�B�  B�RB�\)B��
B�=qB��HB�B�(�B�\B�G�B��B�ffB���B��B�Q�B���B�G�B�  B��RB�p�B��B�z�B��B��B���B��B�B�z�B�G�B�C G�C �C  CG�C��C  CffC�C��CffCC  CffC��C�CffC��C(�Cz�C��C(�C�\C�C33Cz�C�HC	G�C	�C	�C
Q�C
��C
�C\)C�RC  CQ�C�RC33Cz�CC(�C��C�C33C�\C��C\)C�RC  CQ�C�RC�Cp�CC(�C��C�HC33C��C  CQ�C��C  Cp�C�RC
=Cz�C�
C�Cz�C�HC=qC�\C�HC33C��C  CQ�C��C
=CffC�C  CffC��C(�Cp�C��C=qC�\C�HC 33C ��C!  C!\)C!��C"  C"p�C"C#{C#ffC#��C$33C$�C$��C%�C%�C%��C&=qC&�C&�
C'=qC'��C'��C(33C(�\C(��C)Q�C)��C)�HC*=qC*�\C+  C+\)C+�\C+�HC,G�C,�C,�C-33C-�\C-��C.G�C.�C.��C/(�C/�\C/�C033C0z�C0��C1(�C1�C1�
C2�C2ffC2��C3(�C3z�C3C4{C4\)C4�RC5{C5z�C5�RC6  C6\)C6C7(�C7p�C7C8
=C8ffC8�RC9{C9p�C9�C:  C:Q�C:��C;
=C;ffC;�RC<  C<Q�C<��C<�C=G�C=��C=��C>G�C>�\C>��C?�C?z�C?��C@{C@Q�C@��CA  CAQ�CA�CB
=CBQ�CB��CB�HCC=qCC��CC��CD=qCD�CD�
CE33CE�\CE�CFG�CF��CF�CG33CG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@Ȟ@֧G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�33A�;dA�E�A�G�A�G�A�E�A�E�A�E�A�E�A�?}A�
=Aُ\A�G�A�7LA�+A�"�A��A��A�oA�1A�A���A��A��`A���Aؕ�A�z�A�hsA�G�A�VAכ�A�A�1'AՕ�A�VA��TA�(�A�=qAЬA���A�S�A΁A���A�7LA�ȴA�p�A�(�A��A�C�AŰ!A�t�A�;dA¶FA�bNA�?}A�A�A��yA�"�A�bNA��#A���A�C�A���A�r�A���A�5?A�oA�%A�G�A�JA�E�A��hA�S�A�A��+A�bNA���A��9A�^5A���A��!A���A��A���A��A���A���A�dZA�x�A� �A�C�A���A���A��-A��A�$�A�K�A��TA��#A�`BA���A��HA��A���A��/A�
=A~Q�A{dZAy�hAw;dAvVAu&�Atz�Aq��Ap�An��Ak�Aj�Ai��Ah��Ahv�Ag�;AgAf�Ad�HA`VAZ�AY�AW�
AV-AU/AS�AQ��AP �AO;dAN1'ALAJz�AIAGdZADVAB�RA@��A=�FA9��A8�A7oA5��A4n�A3C�A25?A0�+A/�A.9XA-�A,JA)�7A'?}A%�mA$��A$r�A$9XA$�A#��A#C�A!��A!�-A!��A!7LA �A =qAXAƨA��AM�A�A�^A|�A��Av�AA�A$�A  A�wAdZA��A5?A�A��AƨA{AI�Ar�A�9A��AoA=qA�;AAhsA(�A�A�A;dA��A��AJAffA
n�A1A�wAVAAS�A��A�A�A�AjAS�Av�A\)A ȴ@�hs@�ƨ@��@�@�V@�{@�@��D@�ȴ@�l�@�  @�33@��P@��R@�l�@��
@�G�@�J@�(�@��T@�z�@� �@�ff@�@�p�@�/@���@�@�9X@��@�F@�R@ᙚ@�1'@���@ݡ�@�%@�9X@�K�@ڏ\@ٲ-@��@�+@�`B@��/@��@�V@ѩ�@�hs@���@��@��
@ϝ�@�n�@�$�@��@�X@���@�1'@��
@�l�@��@ʏ\@�E�@��T@ɲ-@ɉ7@�hs@���@��@�X@�x�@ȓu@��@���@�"�@�dZ@���@�@�9X@�"�@��;@� �@�(�@� �@�1@�b@��@þw@�;d@�
=@��@\@�v�@�M�@�$�@���@�G�@�r�@�"�@���@��+@���@�G�@��/@��w@���@��\@��R@�V@���@�G�@�G�@�O�@�G�@�/@��`@��9@�Z@���@��@�dZ@�@�^5@�$�@�@���@���@�p�@�`B@�7L@��@���@��D@�  @�t�@�@���@��D@��D@�r�@�Q�@�(�@�b@��@�+@���@��R@��@��H@��y@��y@�~�@�-@�$�@�$�@�@���@��@�O�@�?}@��@��j@�A�@��@�33@�33@�"�@��@��@�o@�@���@�~�@��@��-@�/@��/@���@�bN@�Q�@�Q�@�1'@��m@�\)@��H@���@�@���@�p�@�7L@���@���@�Z@�1@��@�\)@���@�~�@���@�`B@�%@�  @��F@���@��@�C�@��y@��!@��\@�^5@�E�@�$�@�{@�J@�@�@��@��#@���@��@���@�j@�I�@�1@��w@���@�l�@�K�@�33@�33@�o@�ȴ@�-@���@��#@���@�?}@��`@�(�@��;@��F@�K�@���@�-@�J@�J@��@��#@���@��@�r�@�1@�|�@�S�@��y@�v�@��T@�@�X@��u@�(�@�b@��@��@��P@��@�t�@�C�@�33@�+@�"�@�@��H@��@��!@�^5@�$�@�X@���@���@��@�bN@�Q�@�A�@��@���@��@�dZ@�"�@�@��H@��R@�n�@��@�?}@��@��@�%@���@���@��/@�Ĝ@��j@��9@��u@�z�@�A�@�1'@��@��@�b@�  @��;@��
@�ƨ@��w@��F@���@�\)@���@��@��-@���@��@�7L@���@���@��u@�A�@��w@�;d@��y@�M�@�@��@�%@��`@��9@�z�@��@~�y@~v�@~{@}�@}��@}/@|Z@|(�@{��@{C�@z~�@y��@y�@xĜ@x �@w�P@w�P@w+@v��@v��@v��@vv�@v5?@u�@u�-@u�h@u/@t��@tI�@s�F@s��@s�@sdZ@r=q@qhs@q7L@q%@p��@p��@p�9@pr�@pr�@pr�@pr�@pr�@pr�@pr�@pr�@pA�@p  @o|�@n@m�@m?}@l�j@lZ@k��@k�m@k�m@k�m@k�m@k�
@k��@k"�@j=q@i�@iG�@h��@hA�@gl�@g+@f�R@f��@f��@f��@fff@ep�@e?}@d��@dz�@dZ@dI�@dI�@d9X@d(�@d1@cƨ@c��@c��@c�@c33@b~�@a�@`�9@`1'@_�;@_�P@_K�@^��@^�@^�R@^��@^v�@^ff@^{@]��@]�@\��@\�@\�/@\�/@\�j@\�j@\�@\�@[��@[�
@["�@Z��@ZM�@Z-@Y�@X�@W�P@W�P@W�P@W|�@Wl�@W\)@W;d@V��@VE�@V{@V@V@U�@U�@U��@U�T@U@U�-@U��@U��@U��@U`B@U?}@UV@T�@T(�@S��@R�@R��@R��@Rn�@RM�@RM�@RM�@RM�@R=q@RM�@R=q@RJ@Q��@QG�@P�9@Pr�@OK�@Nv�@N{@M@Mp�@M�@L��@L9X@K�m@K��@K33@K@J�@JM�@I��@I�^@I��@I�7@I�7@IX@IG�@I%@Hr�@H �@H  @G��@G��@G��@G�P@Gl�@GK�@GK�@G;d@F��@F��@Fff@FV@FE�@FV@FE�@F@E@Ep�@E�@D��@D�@D��@D�D@Dz�@Dj@DZ@DZ@D�@D1@C�m@Ct�@C@B��@Bn�@B-@A�^@Ax�@?��@>V@=��@=�-@=O�@=`B@=�@=/@<1@;t�@;o@:=q@9hs@8��@7�;@7�P@7;d@6�y@65?@5�h@5�@4�@4�/@4�D@4�@3��@3C�@2�H@2~�@2M�@2�@2J@1�^@1�7@1&�@0��@0r�@0 �@0b@/�@/��@/l�@.�y@.V@-?}@,(�@+dZ@)��@)��@)�@(�`@(�u@'�w@'\)@'K�@&��@&�@&��@&�+@&E�@%`B@$�/@$z�@$(�@#�m@#ƨ@#��@#��@#t�@#dZ@#S�@#S�@#C�@#C�@#C�@#C�@#o@"�H@"��@"��@"~�@"n�@"^5@"M�@"=q@"=q@"=q@"�@"�@"�@!��@!�^@ �`@ A�@��@�P@�@ff@$�@5?@5?@�@�@�@z�@�D@z�@z�@z�@z�@j@z�@Z@I�@Z@z�@j@9X@(�@I�@I�@9X@�m@t�@S�@C�@��@�H@�@�H@�H@��@�H@�@@�!@��@�^@��@��@x�@&�@��@�9@Ĝ@��@�u@r�@�@�@�9@Ĝ@bN@Q�@b@  @  @�@�@�;@�;@��@��@��@�P@��@V@�-@p�@`B@`B@`B@O�@?}@/@/@�@��@�@�D@z�@z�@Z@I�@�m@o@��@�\@-@-@-@-@-@-@��@�^@��@��@��@��@�7@�7@x�@hs@G�@A�@l�@;d@+@+@�@�@
=@��@��@�y@�y@�RAٟ�A���A��A�"�A�$�A��A�7LA�A�A�;dA�7LA�;dA�E�A�I�A�C�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�E�A�G�A�E�A�A�A�E�A�G�A�C�A�C�A�G�A�C�A�E�A�G�A�C�A�E�A�G�A�G�A�C�A�;dA�=qA�/A���A�JA�{A��mA���A���Aٴ9Aٲ-A١�AّhA�Q�A�Q�A�O�A�I�A�G�A�G�A�K�A�G�A�C�A�A�A�A�A�9XA�5?A�5?A�33A�-A�/A�/A�+A�&�A�(�A�+A�&�A�&�A�&�A�$�A�"�A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A�{A�{A��A��A�{A��A��A�bA�JA�bA�bA�JA�JA�VA�JA�A�%A�1A�A�A�A�%A�A�  A�A�A���A���A���A���A���A��A���A��A��A��A��A��A��yA��A��A��mA��mA��yA��HA��;A��HA��TA��HA��A���A��
A���A�ȴA���Aغ^Aإ�Aؗ�AؑhAؓuAؑhA؇+A؃A؁A؃A؁A�t�A�t�A�z�A�v�A�p�A�n�A�n�A�hsA�`BA�bNA�`BA�ZA�S�A�K�A�I�A�A�A�;dA�=qA�9XA�/A�+A�(�A��A�JA���A�A׾wA׺^A׺^Aװ!Aן�A׋DA�r�A�VA�M�A�=qA��A��yA��/A��A���Aִ9A֕�A�l�A�9XA��A���A���Aղ-A՛�AՏ\A՛�A՝�AՏ\AՇ+AՋDAՉ7AՁA�|�A�VA�&�A��A�
=A�%A�%A�  A��A��HA԰!A�z�A�
=Aӥ�A�9XA�VA���A�A�Aћ�A�XA�7LA�(�A�"�A�&�A�(�A���A��AЮAБhA�n�A�M�A��A���A���A�Aϧ�AϏ\A�p�A�dZA�^5A�^5A�S�A�I�A�7LA�&�A�JA��/Aκ^A�K�A̓A�bA�A��A���A̶FA̍PA�ffA�VA�E�A�=qA�-A�$�A��A�VA���AˋDA���A���Aə�Aɕ�Aɏ\A�z�A�n�A�ffA�`BA�VA�I�A�A�A�=qA�1'A�(�A��A�l�A�VAƾwAƩ�AƟ�AƗ�AƇ+A�t�A�ffA�ZA�M�A�&�A���A��A���A�ȴAŸRAũ�AōPA�?}A�
=A���A�x�A�^5A��A��TA�ĜAÉ7A�\)A� �A��A��TA���A���A���A¼jA¥�A�AuAA�t�A�jA�l�A�;dA�?}A�A�A�I�A�C�A�=qA�;dA�=qA�=qA�?}A�?}A�A�A�E�A�C�A�=qA�?}A�A�A�;dA�1'A�%A���A�?}A��A�bA�1A���A�ƨA�ĜA�p�A�%A���A�r�A�bNA�?}A�5?A�$�A��A�1A��A��A��FA��FA��wA��A��RA��FA���A��hA��PA�t�A�n�A�n�A�`BA�VA�G�A�E�A�bA��A��/A�dZA�|�A�ĜA���A�^5A�A�A��A���A��jA�z�A�C�A���A��-A�XA�{A��`A�ƨA��!A���A���A��7A��A�t�A�K�A��A��TA��+A�&�A���A�(�A���A���A�ZA���A��A���A�=qA��^A�`BA�  A���A��uA�=qA�
=A���A�ĜA�dZA�?}A���A��`A���A���A��+A�dZA�K�A�(�A�A��HA�ĜA���A��DA�bNA�VA�ZA�dZA�l�A�hsA�5?A�$�A��A�
=A�JA�
=A��A��;A���A�ĜA���A�\)A�"�A��/A���A�p�A�VA��A���A��yA��!A���A�~�A�Q�A�1A��`A���A���A���A�S�A��`A�v�A�dZA�S�A�1'A�VA���A�x�A���A�|�A�hsA�E�A�7LA��A��uA�=qA��/A��jA��A���A���A�~�A��mA��A�A��HA���A��FA��\A�t�A�/A�ƨA���A���A���A�I�A���A���A��^A���A�G�A��A���A�dZA�Q�A�M�A�33A�A��mA��A���A��wA���A��uA�K�A�VA�1A��yA��9A�z�A�O�A�-A��A��^A�z�A�7LA���A���A�K�A���A�`BA�33A��A�%A��A���A� �A��uA�`BA�A�A��A�1A��yA���A��A��PA��A�ffA�VA�K�A�=qA��A�\)A�K�A�C�A�/A�A���A��A��A���A�l�A�"�A��;A�dZA�5?A��mA�A�A��A�bA���A�ZA�oA���A��`A���A��uA�?}A�(�A�%A��`A��^A�|�A�t�A�n�A�hsA�hsA�ffA�^5A�M�A�1'A�VA�%A���A�|�A��;A�5?A�r�A�/A���A���A�XA�=qA�&�A�
=A�A�$�A��mA���A�O�A��yA�?}A��+A�p�A�XA�E�A�$�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      A�A�33A�;dA�E�A�G�A�G�A�E�A�E�A�E�A�E�A�?}A�
=Aُ\A�G�A�7LA�+A�"�A��A��A�oA�1A�A���A��A��`A���Aؕ�A�z�A�hsA�G�A�VAכ�A�A�1'AՕ�A�VA��TA�(�A�=qAЬA���A�S�A΁A���A�7LA�ȴA�p�A�(�A��A�C�AŰ!A�t�A�;dA¶FA�bNA�?}A�A�A��yA�"�A�bNA��#A���A�C�A���A�r�A���A�5?A�oA�%A�G�A�JA�E�A��hA�S�A�A��+A�bNA���A��9A�^5A���A��!A���A��A���A��A���A���A�dZA�x�A� �A�C�A���A���A��-A��A�$�A�K�A��TA��#A�`BA���A��HA��A���A��/A�
=A~Q�A{dZAy�hAw;dAvVAu&�Atz�Aq��Ap�An��Ak�Aj�Ai��Ah��Ahv�Ag�;AgAf�Ad�HA`VAZ�AY�AW�
AV-AU/AS�AQ��AP �AO;dAN1'ALAJz�AIAGdZADVAB�RA@��A=�FA9��A8�A7oA5��A4n�A3C�A25?A0�+A/�A.9XA-�A,JA)�7A'?}A%�mA$��A$r�A$9XA$�A#��A#C�A!��A!�-A!��A!7LA �A =qAXAƨA��AM�A�A�^A|�A��Av�AA�A$�A  A�wAdZA��A5?A�A��AƨA{AI�Ar�A�9A��AoA=qA�;AAhsA(�A�A�A;dA��A��AJAffA
n�A1A�wAVAAS�A��A�A�A�AjAS�Av�A\)A ȴ@�hs@�ƨ@��@�@�V@�{@�@��D@�ȴ@�l�@�  @�33@��P@��R@�l�@��
@�G�@�J@�(�@��T@�z�@� �@�ff@�@�p�@�/@���@�@�9X@��@�F@�R@ᙚ@�1'@���@ݡ�@�%@�9X@�K�@ڏ\@ٲ-@��@�+@�`B@��/@��@�V@ѩ�@�hs@���@��@��
@ϝ�@�n�@�$�@��@�X@���@�1'@��
@�l�@��@ʏ\@�E�@��T@ɲ-@ɉ7@�hs@���@��@�X@�x�@ȓu@��@���@�"�@�dZ@���@�@�9X@�"�@��;@� �@�(�@� �@�1@�b@��@þw@�;d@�
=@��@\@�v�@�M�@�$�@���@�G�@�r�@�"�@���@��+@���@�G�@��/@��w@���@��\@��R@�V@���@�G�@�G�@�O�@�G�@�/@��`@��9@�Z@���@��@�dZ@�@�^5@�$�@�@���@���@�p�@�`B@�7L@��@���@��D@�  @�t�@�@���@��D@��D@�r�@�Q�@�(�@�b@��@�+@���@��R@��@��H@��y@��y@�~�@�-@�$�@�$�@�@���@��@�O�@�?}@��@��j@�A�@��@�33@�33@�"�@��@��@�o@�@���@�~�@��@��-@�/@��/@���@�bN@�Q�@�Q�@�1'@��m@�\)@��H@���@�@���@�p�@�7L@���@���@�Z@�1@��@�\)@���@�~�@���@�`B@�%@�  @��F@���@��@�C�@��y@��!@��\@�^5@�E�@�$�@�{@�J@�@�@��@��#@���@��@���@�j@�I�@�1@��w@���@�l�@�K�@�33@�33@�o@�ȴ@�-@���@��#@���@�?}@��`@�(�@��;@��F@�K�@���@�-@�J@�J@��@��#@���@��@�r�@�1@�|�@�S�@��y@�v�@��T@�@�X@��u@�(�@�b@��@��@��P@��@�t�@�C�@�33@�+@�"�@�@��H@��@��!@�^5@�$�@�X@���@���@��@�bN@�Q�@�A�@��@���@��@�dZ@�"�@�@��H@��R@�n�@��@�?}@��@��@�%@���@���@��/@�Ĝ@��j@��9@��u@�z�@�A�@�1'@��@��@�b@�  @��;@��
@�ƨ@��w@��F@���@�\)@���@��@��-@���@��@�7L@���@���@��u@�A�@��w@�;d@��y@�M�@�@��@�%@��`@��9@�z�@��@~�y@~v�@~{@}�@}��@}/@|Z@|(�@{��@{C�@z~�@y��@y�@xĜ@x �@w�P@w�P@w+@v��@v��@v��@vv�@v5?@u�@u�-@u�h@u/@t��@tI�@s�F@s��@s�@sdZ@r=q@qhs@q7L@q%@p��@p��@p�9@pr�@pr�@pr�@pr�@pr�@pr�@pr�@pr�@pA�@p  @o|�@n@m�@m?}@l�j@lZ@k��@k�m@k�m@k�m@k�m@k�
@k��@k"�@j=q@i�@iG�@h��@hA�@gl�@g+@f�R@f��@f��@f��@fff@ep�@e?}@d��@dz�@dZ@dI�@dI�@d9X@d(�@d1@cƨ@c��@c��@c�@c33@b~�@a�@`�9@`1'@_�;@_�P@_K�@^��@^�@^�R@^��@^v�@^ff@^{@]��@]�@\��@\�@\�/@\�/@\�j@\�j@\�@\�@[��@[�
@["�@Z��@ZM�@Z-@Y�@X�@W�P@W�P@W�P@W|�@Wl�@W\)@W;d@V��@VE�@V{@V@V@U�@U�@U��@U�T@U@U�-@U��@U��@U��@U`B@U?}@UV@T�@T(�@S��@R�@R��@R��@Rn�@RM�@RM�@RM�@RM�@R=q@RM�@R=q@RJ@Q��@QG�@P�9@Pr�@OK�@Nv�@N{@M@Mp�@M�@L��@L9X@K�m@K��@K33@K@J�@JM�@I��@I�^@I��@I�7@I�7@IX@IG�@I%@Hr�@H �@H  @G��@G��@G��@G�P@Gl�@GK�@GK�@G;d@F��@F��@Fff@FV@FE�@FV@FE�@F@E@Ep�@E�@D��@D�@D��@D�D@Dz�@Dj@DZ@DZ@D�@D1@C�m@Ct�@C@B��@Bn�@B-@A�^@Ax�@?��@>V@=��@=�-@=O�@=`B@=�@=/@<1@;t�@;o@:=q@9hs@8��@7�;@7�P@7;d@6�y@65?@5�h@5�@4�@4�/@4�D@4�@3��@3C�@2�H@2~�@2M�@2�@2J@1�^@1�7@1&�@0��@0r�@0 �@0b@/�@/��@/l�@.�y@.V@-?}@,(�@+dZ@)��@)��@)�@(�`@(�u@'�w@'\)@'K�@&��@&�@&��@&�+@&E�@%`B@$�/@$z�@$(�@#�m@#ƨ@#��@#��@#t�@#dZ@#S�@#S�@#C�@#C�@#C�@#C�@#o@"�H@"��@"��@"~�@"n�@"^5@"M�@"=q@"=q@"=q@"�@"�@"�@!��@!�^@ �`@ A�@��@�P@�@ff@$�@5?@5?@�@�@�@z�@�D@z�@z�@z�@z�@j@z�@Z@I�@Z@z�@j@9X@(�@I�@I�@9X@�m@t�@S�@C�@��@�H@�@�H@�H@��@�H@�@@�!@��@�^@��@��@x�@&�@��@�9@Ĝ@��@�u@r�@�@�@�9@Ĝ@bN@Q�@b@  @  @�@�@�;@�;@��@��@��@�P@��@V@�-@p�@`B@`B@`B@O�@?}@/@/@�@��@�@�D@z�@z�@Z@I�@�m@o@��@�\@-@-@-@-@-@-@��@�^@��@��@��@��@�7@�7@x�@hs@G�@A�@l�@;d@+@+@�@�@
=@��@��@�y@�yG�O�Aٟ�A���A��A�"�A�$�A��A�7LA�A�A�;dA�7LA�;dA�E�A�I�A�C�A�E�A�I�A�I�A�E�A�G�A�I�A�E�A�E�A�G�A�E�A�A�A�E�A�G�A�C�A�C�A�G�A�C�A�E�A�G�A�C�A�E�A�G�A�G�A�C�A�;dA�=qA�/A���A�JA�{A��mA���A���Aٴ9Aٲ-A١�AّhA�Q�A�Q�A�O�A�I�A�G�A�G�A�K�A�G�A�C�A�A�A�A�A�9XA�5?A�5?A�33A�-A�/A�/A�+A�&�A�(�A�+A�&�A�&�A�&�A�$�A�"�A��A� �A�"�A��A��A��A��A��A��A��A��A��A��A��A�{A�{A��A��A�{A��A��A�bA�JA�bA�bA�JA�JA�VA�JA�A�%A�1A�A�A�A�%A�A�  A�A�A���A���A���A���A���A��A���A��A��A��A��A��A��yA��A��A��mA��mA��yA��HA��;A��HA��TA��HA��A���A��
A���A�ȴA���Aغ^Aإ�Aؗ�AؑhAؓuAؑhA؇+A؃A؁A؃A؁A�t�A�t�A�z�A�v�A�p�A�n�A�n�A�hsA�`BA�bNA�`BA�ZA�S�A�K�A�I�A�A�A�;dA�=qA�9XA�/A�+A�(�A��A�JA���A�A׾wA׺^A׺^Aװ!Aן�A׋DA�r�A�VA�M�A�=qA��A��yA��/A��A���Aִ9A֕�A�l�A�9XA��A���A���Aղ-A՛�AՏ\A՛�A՝�AՏ\AՇ+AՋDAՉ7AՁA�|�A�VA�&�A��A�
=A�%A�%A�  A��A��HA԰!A�z�A�
=Aӥ�A�9XA�VA���A�A�Aћ�A�XA�7LA�(�A�"�A�&�A�(�A���A��AЮAБhA�n�A�M�A��A���A���A�Aϧ�AϏ\A�p�A�dZA�^5A�^5A�S�A�I�A�7LA�&�A�JA��/Aκ^A�K�A̓A�bA�A��A���A̶FA̍PA�ffA�VA�E�A�=qA�-A�$�A��A�VA���AˋDA���A���Aə�Aɕ�Aɏ\A�z�A�n�A�ffA�`BA�VA�I�A�A�A�=qA�1'A�(�A��A�l�A�VAƾwAƩ�AƟ�AƗ�AƇ+A�t�A�ffA�ZA�M�A�&�A���A��A���A�ȴAŸRAũ�AōPA�?}A�
=A���A�x�A�^5A��A��TA�ĜAÉ7A�\)A� �A��A��TA���A���A���A¼jA¥�A�AuAA�t�A�jA�l�A�;dA�?}A�A�A�I�A�C�A�=qA�;dA�=qA�=qA�?}A�?}A�A�A�E�A�C�A�=qA�?}A�A�A�;dA�1'A�%A���A�?}A��A�bA�1A���A�ƨA�ĜA�p�A�%A���A�r�A�bNA�?}A�5?A�$�A��A�1A��A��A��FA��FA��wA��A��RA��FA���A��hA��PA�t�A�n�A�n�A�`BA�VA�G�A�E�A�bA��A��/A�dZA�|�A�ĜA���A�^5A�A�A��A���A��jA�z�A�C�A���A��-A�XA�{A��`A�ƨA��!A���A���A��7A��A�t�A�K�A��A��TA��+A�&�A���A�(�A���A���A�ZA���A��A���A�=qA��^A�`BA�  A���A��uA�=qA�
=A���A�ĜA�dZA�?}A���A��`A���A���A��+A�dZA�K�A�(�A�A��HA�ĜA���A��DA�bNA�VA�ZA�dZA�l�A�hsA�5?A�$�A��A�
=A�JA�
=A��A��;A���A�ĜA���A�\)A�"�A��/A���A�p�A�VA��A���A��yA��!A���A�~�A�Q�A�1A��`A���A���A���A�S�A��`A�v�A�dZA�S�A�1'A�VA���A�x�A���A�|�A�hsA�E�A�7LA��A��uA�=qA��/A��jA��A���A���A�~�A��mA��A�A��HA���A��FA��\A�t�A�/A�ƨA���A���A���A�I�A���A���A��^A���A�G�A��A���A�dZA�Q�A�M�A�33A�A��mA��A���A��wA���A��uA�K�A�VA�1A��yA��9A�z�A�O�A�-A��A��^A�z�A�7LA���A���A�K�A���A�`BA�33A��A�%A��A���A� �A��uA�`BA�A�A��A�1A��yA���A��A��PA��A�ffA�VA�K�A�=qA��A�\)A�K�A�C�A�/A�A���A��A��A���A�l�A�"�A��;A�dZA�5?A��mA�A�A��A�bA���A�ZA�oA���A��`A���A��uA�?}A�(�A�%A��`A��^A�|�A�t�A�n�A�hsA�hsA�ffA�^5A�M�A�1'A�VA�%A���A�|�A��;A�5?A�r�A�/A���A���A�XA�=qA�&�A�
=A�A�$�A��mA���A�O�A��yA�?}A��+A�p�A�XA�E�A�$�A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�:B
�:B
�B
�:B
�B
�B
��B
�4B
��B
��B
��B
�uB
�@B
�B
��B
�B
�B
��B
�oB
�B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�=B
�oB
��B
�SB
��B
�nB
��B
ǮB
��B
�B
�+B
��B#�B2-Bc�B�(B��B�qB��B��B�gB҉BɺB҉BGB=B7B�BϫB��B�*B��B��B�FB�9B� B��B�B�B�B7B7B�B$B*�B"�BB	B�B��B�B�B�B�vB��B�}B�RB�B�FB��B�B��B��B�MBcTBQNB>BB*�B�BoBB
��B
�fB
�<B
�*B
�CB
��B
�;B
s�B
i�B
b�B
^jB
[�B
OBB
C-B
=<B
)_B
&�B
�B
�B
:B
4B
�B
�B	�B	�B	�RB	��B	�LB	�B	��B	��B	�+B	�~B	�_B	��B	y�B	qAB	jB	f�B	V9B	MjB	DgB	9�B	+�B	"4B	�B	1B	MB	bB	�B	�B	�B�.B�PB�8B�MB�cB�QB�DB�B�B��B�B�B�B�B�B�B�HB��B�vB�B�B��B�GB��B�)B�;B��B�B�;B�iB�B�B�B��B�B�B�B�fB��B	AB	�B	�B	�B	�B	�B	YB	�B	�B	#:B	5�B	K)B	MB	N<B	NB	@�B	3�B	�B	�B	�B	!�B	$tB	)*B	*�B	0!B	33B	3�B	3�B	3�B	(�B	($B	6FB	@B	D3B	QB	cTB	r�B	t�B	~]B	{JB	�7B	�MB	��B	�=B	��B	�oB	tB	�_B	��B	�	B	�1B	�hB	��B	��B	�%B	p;B	m�B	l�B	j�B	iB	gmB	e�B	c�B	^�B	\�B	Y�B	_B	`B	a|B	d&B	e�B	g�B	g�B	m�B	o B	oiB	q�B	w�B	y�B	zDB	{�B	�B	~�B	.B	��B	��B	��B	��B	�B	�B	��B	��B	�bB	�:B	��B	��B	�B	��B	��B	�1B	��B	��B	��B	��B	��B	�gB	�'B	ƨB	��B	�B	��B	��B	�BB	ӏB	ԕB	��B	��B	�B	�mB	یB	ޞB	ޞB	�B	ܒB	��B	��B	ںB	چB	�B	�B	�B	��B	�QB	�B	�B	�WB	�/B	�WB	�B	��B	�B	��B	��B	��B	�2B	�fB	�8B	�xB	�"B
 iB
�B
MB
�B

=B
�B
	�B

=B
B
DB
B
B

�B

=B

=B
	�B
	B
�B

rB
xB
DB
xB
�B
JB
JB
~B
JB
B
�B
�B
�B
�B
kB
=B
!�B
%FB
&B
&�B
'�B
(�B
)�B
)�B
)�B
)�B
*0B
,B
.�B
/B
.�B
/B
/�B
/OB
.�B
/OB
/�B
/�B
/�B
.�B
-�B
-B
,�B
,�B
,�B
,�B
,�B
,�B
.}B
.IB
/B
2�B
2�B
4B
4�B
5�B
6�B
7B
7�B
8�B
9$B
:^B
:�B
;�B
;�B
<B
>�B
>BB
>BB
>�B
?HB
?�B
@OB
@B
@�B
@�B
@�B
@�B
@�B
A B
@�B
@�B
A B
AUB
B[B
B�B
B�B
B�B
C�B
C�B
DgB
D3B
D�B
D3B
C�B
D3B
EB
E�B
E�B
E�B
E9B
FB
E9B
F?B
E�B
F?B
F�B
H�B
IRB
IRB
H�B
IB
H�B
IRB
J#B
LdB
L�B
MB
MB
MjB
N�B
N�B
NpB
OB
Q�B
P�B
QNB
QNB
Q�B
Q�B
Q�B
R B
RTB
R B
Q�B
Q�B
Q�B
R B
Q�B
R B
R�B
R B
UgB
S�B
UgB
T�B
U2B
T�B
T�B
T�B
T�B
UgB
U�B
V9B
VB
VmB
V9B
V�B
W�B
X�B
XyB
XEB
X�B
XEB
XyB
XyB
X�B
X�B
X�B
X�B
X�B
YB
YB
YB
YB
YB
Y�B
Y�B
Y�B
Y�B
Y�B
YB
Y�B
Y�B
[#B
[�B
[�B
[�B
[�B
\�B
]/B
\�B
]dB
^jB
_pB
_pB
_pB
`BB
`�B
`�B
a�B
aHB
aB
a�B
b�B
c B
c B
c�B
c B
c�B
c�B
d�B
dZB
d�B
e�B
e�B
g8B
g8B
gmB
h
B
h
B
h
B
iB
hsB
h�B
h�B
iB
iDB
iDB
iyB
iyB
iyB
jB
jKB
kB
j�B
j�B
jB
l�B
k�B
l"B
lWB
lWB
l"B
l"B
l�B
l�B
lWB
lWB
l�B
lWB
lWB
lWB
lWB
lWB
l�B
n/B
m�B
n/B
ncB
oiB
o�B
o�B
o�B
oiB
o�B
o�B
oiB
pB
poB
poB
p�B
qB
p�B
rB
q�B
r�B
rGB
rB
rB
r�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
tB
s�B
t�B
tTB
t�B
t�B
tB
t�B
u�B
uZB
w�B
v�B
wfB
wfB
xB
xB
x8B
xB
xB
xlB
w�B
xlB
x�B
y>B
y>B
yrB
yrB
y>B
y�B
y	B
y>B
zB
y�B
y�B
z�B
z�B
z�B
z�B
zDB
}"B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}VB
}�B
~(B
}�B
}�B
~(B
}�B
~]B
}�B
~�B
~]B
~�B
~�B
~]B
~�B
~�B
.B
�B
�4B
�B
�AB
�B
�B
�uB
��B
�uB
�uB
�uB
�uB
�uB
�uB
��B
�AB
�B
�{B
�{B
��B
�SB
��B
��B
��B
��B
��B
��B
��B
�+B
�_B
��B
��B
�1B
�fB
�fB
��B
��B
��B
��B
��B
�B
�lB
��B
��B
��B
��B
�	B
��B
�=B
�=B
�	B
�=B
�rB
��B
��B
��B
�B
��B
��B
�B
�DB
�xB
�B
�B
�B
�JB
�JB
�JB
�JB
�~B
�~B
�~B
�~B
�~B
�PB
�B
��B
��B
��B
�"B
��B
��B
�4B
� B
�hB
��B
��B
� B
��B
��B
��B
��B
�B
�{B
��B
��B
��B
�B
��B
�$B
��B
�+B
��B
��B
��B
��B
�eB
�eB
�B
�B
�kB
�kB
�kB
��B
��B
�qB
�qB
�qB
�CB
�B
��B
�B
�CB
�xB
��B
�B
��B
��B
�-B
��B
��B
�bB
��B
�B
��B
��B
�:B
�:B
�:B
�nB
�:B
�B
��B
�zB
�zB
��B
��B
��B
��B
�B
�B
�LB
�LB
�LB
�LB
�B
�LB
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�RB
�B
�B
��B
��B
��B
��B
�*B
��B
��B
��B
��B
�eB
�eB
��B
��B
�B
�=B
�B
�=B
�=B
�=B
�=B
�=B
�B
�qB
��B
�=B
�=B
�qB
�qB
��B
�qB
�qB
�=B
��B
�wB
�CB
�CB
��B
�B
��B
�wB
��B
��B
��B
�wB
�CB
�B
��B
��B
��B
��B
��B
�OB
�OB
�!B
��B
��B
�UB
�UB
�!B
�!B
��B
��B
��B
�UB
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
�hB
�3B
��B
�3B
�3B
�hB
�hB
�hB
��B
��B
��B
�B
�B
�B
�B
�B
�nB
��B
��B
��B
�FB
�FB
�FB
�FB
�FB
�B
�zB
��B
��B
�zB
�zB
��B
��B
��B
��B
��B
�zB
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
�X?ͿH@��B	�B
�{B
�FB
�uB
�B
��B
��B
�hB
�:B
��B
��B
�oB
�B
� B
� B
��B
�B
��B
��B
��B
��B
��B
�@B
�hB
� B
��B
�:B
��B
��B
��B
� B
�:B
��B
�bB
��B
��B
��B
��B
�uB
�$B
��B
�oB
��B
��B
��B
�B
��B
�uB
�B
�1B
�B
��B
�B
�FB
�uB
�B
�uB
��B
��B
�B
��B
�B
��B
��B
��B
��B
��B
��B
�B
�B
��B
�uB
��B
��B
�:B
��B
�B
�@B
�B
�uB
��B
�B
�@B
�B
��B
�B
�B
��B
�oB
��B
�B
��B
��B
�oB
�B
��B
��B
�uB
��B
��B
��B
�@B
��B
��B
�:B
�uB
�oB
�4B
��B
�@B
�oB
� B
�B
��B
��B
� B
�B
�B
� B
��B
��B
�oB
��B
�:B
��B
�4B
� B
�:B
��B
�4B
��B
��B
��B
�B
�B
��B
��B
�MB
�FB
�YB
��B
��B
�B
�$B
��B
�YB
�$B
��B
��B
��B
��B
�B
��B
�CB
��B
�qB
�~B
�=B
�7B
�CB
�~B
��B
��B
�xB
��B
��B
�kB
�kB
��B
�1B
�YB
�_B
��B
�SB
��B
�B
�SB
��B
�FB
��B
��B
��B
��B
��B
��B
�B
�fB
��B
�DB
�B
�;B
�oB
�+B
��B
�B
~]B
�B
�AB
�B
�B
��B
��B
�	B
�@B
��B
��B
��B
�.B
�+B
�CB
��B
��B
�B
�OB
��B
��B
��B
�[B
��B
�aB
��B
��B
�B
�B
��B
��B
�FB
�B
��B
��B
��B
�9B
ǮB
�[B
B
�B
�B
ɆB
��B
�B
�B
�]B
��B
�B
�B
��B
��B �B
��B
�B
��B
��B
��B
��B
��B
�+B
��B
��B
��B
�B
�oB
��B	lB+BB�B#B"�B%B*0B-�B.�B1�B1[B2�B2�B3�B4�B8�BL�Bi�B�B�	B��B�lB��B��B�hB�4B�B�B�B�FB�{B�hB�OB�B��B�B��B��B�RB�XB�qB�B�B�B��B�RB�B��B�B��B��B�B�jB��B�0B�sB�
B�HB�HB�gB��B҉B�
B̘B�#B��B�#B��B�RB�XB�)BȴB��B͟B��B�HB�5B��B�NB�B��B%B.B$B	BkB�B	B7BkBB�B�B�B1B�BB>BB�B
�B
�B�B~B3hB'�B�B�B�B�BƨB�KBȴB�B�?B�-B��B�XB�B�zB�UB�*B�B�jB�RB��B�6B�-B��B��B��B�kB��B�*B��B�'B�0B�B�UB��B�!B��B��B��B��B�RB�'B��B� BǮB�KBĜB��BĜB�3B�-B��BŢB�)B�B��B�jB�DB�WB�8BB��B��B�B�B;B �B�BYB�B@B(BoBB�B@B�B!�B$B�BMB1B�B�BB�BB�B�B�B~BqB$BeB�B�BqBIB(�B#�B!�B#�B"hB$@B(�B&�B%zB'�B+�B0!B.�B*eB'�B#B�B$�B1B�B!B�BB4BB�B�BSB�BB�B��B�TB��B��B�oB��B��BBیB��BޞB��B��B�2B�B�2BںB��B��B��B�B�B�fB��B�,B��B�B�fB�QB�B�ZB�B�}B̘B�B��B�?B��B��B�mB�B��B��B��B�B��B��B��B�zB�B��B�LB��B�}B�B��B�B��B�tB�B�3B�zB�'B�'B��B�[B��B��B�hB��B��B��B��B��B��B�B��B��B��B�bB��B�xB�YB�DB�B�oB�B�lB��B�bBt�Bm�B`Ba�Bd&BkBd�Bh�BWsBN<BR�BJ�BL�BNpB=<BD�BW?B2�B0UBFB1�B*eB&�B'�B"�B)�BB�BxB�B �B�B�B�BoB4B�B�BVB\B_B	lB
	B'B~B!�B�B
��B
��B
��B
�oB
��B
�mB
��B
�/B
��B
�QB
�pB
бB
��BxB
�OB
��B
��B
�0B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�9444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021062715241320210627152413IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021070719005920210707190059QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021070719005920210707190059QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365020220126093650IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295220220204232952IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                