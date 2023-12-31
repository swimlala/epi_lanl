CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-15T07:21:25Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210315072125  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               u   uAA  AOAO7314_008642_117                 7314_008642_117                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�e�6�F@�e�6�F11  @�e�e��O@�e�e��O@1�64�@1�64��b��r�/Z�b��r�/Z11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@@  @�  @�G�@�  @�  AG�A��A ��A,(�A?\)A`  A�  A���A���A�Q�A���A�Q�A�  A�  B   B�
B�
B(�B z�B((�B/�
B8  B@  BH  BO�
BW�
B_�
Bg�Bp  Bx(�B�{B�  B��B�  B�{B�{B�(�B�  B��B�  B�  B�  B�  B��
B�  B�  B��B��
B��
B��B�{B�  B��B��B�  B�  B�  B��B��
B��
B��
B�  C �C  C�C�C  C

=C  C��C  C
=C
=C  C��C  C  C  C��C"  C$
=C&  C'��C*  C,  C.  C/��C2  C4
=C6  C8  C9��C<  C>  C@  CB
=CD  CF  CH  CJ
=CL{CN  CO��CR  CT  CU��CX  CY��C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co��Cr  Cs��Cu��Cx  Cz  C{��C~  C�C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C���C���C���C�  C���C�  C�
=C�C���C���C�C�  C���C�  C�  C�  C�  C�  C�C�  C�  C�  C���C���C���C���C�  C���C�  C�C�  C�  C���C�  C�C�C�C�  C�C�
=C�C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C���C���C���C�C�  C���C���C���C�  C�C���C���C�  C�C�  C�C�C�  C�  C�  C�
=C�
=C�  C�C�C�C�  C�C�C�  C�
=C�C�  C�  C���C���C�  C�  C�C�C�C���C�  C�
=C�  C���C���C���C�  C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  D   D � D�D��D�qD� D  Dz�D�qD� D�D�D�D��D  D��D  Dz�D	  D	�D
�D
�DD��D�D}qD  D� D�D�D�D� D�D��D  D�D�D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D��DD� D  D� D  D��D�qD � D!�D!}qD"  D"� D"��D#}qD$  D$}qD%  D%� D%�qD&� D'  D'��D(D(� D)�D)��D)�qD*� D+�D+� D,�D,�D-�D-� D-�qD.z�D/  D/��D0�D0� D0�qD1}qD1��D2}qD3  D3� D3�qD4� D5D5��D6�D6� D6�qD7� D8�D8��D9  D9}qD:  D:� D:�qD;� D<�D<��D=  D=� D=�qD>� D>�qD?� D@  D@}qDA  DA� DB  DB��DC�DC� DC�qDD}qDE  DE� DF  DF�DGDG� DH  DH��DI  DI� DJ�DJ��DK�DK� DL  DL� DL��DM}qDN  DN}qDO  DO� DO�qDP}qDQ  DQ� DQ�qDR� DS�DS��DT  DT� DU  DU��DV  DV}qDW  DW� DW��DX}qDY�DY��DZ  DZ� D[�D[� D\  D\��D]  D]}qD^  D^��D_  D_� D`�D`��Da  Da� Db  Db��Dc  Dc� Dd  Dd}qDd�qDe}qDe�qDf}qDg  Dg� Dg�qDh� Di�Di��Dj  Dj� Dk  Dk��DlDl��Dm  Dm��DnDn��Dn�qDo}qDp  Dp� Dq  Dq� Dr  Dr��Ds  Ds� Dt  Dt}qDu�Du� Dv  Dv��Dw�Dw� Dx  Dx� Dx�qDy}qDy�qDz� D{  D{z�D{��D|}qD}�D}� D}�qD~}qD  D� D�HD�AHD�� D���D��qD�>�D�� D���D�  D�AHD�� D��HD��D�AHD�� D�� D�HD�AHD��HD�D�  D�>�D�� D��HD���D�@ D�� D���D�HD�AHD��HD��HD�  D�>�D�� D�� D���D�@ D�� D���D���D�AHD��HD�� D�HD�AHD�� D��qD�HD�AHD�~�D�� D�  D�AHD�~�D��qD�  D�AHD�� D���D�  D�>�D�}qD���D�HD�@ D�~�D���D���D�@ D�� D�� D���D�>�D�}qD���D��D�B�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D���D�HD�@ D�}qD��qD�  D�@ D�� D�� D���D�>�D�~�D�� D��qD�>�D��HD��HD�  D�@ D�� D�D��D�@ D�� D�� D�HD�AHD�~�D���D�  D�B�D��HD���D���D�AHD��HD���D���D�@ D�� D��HD���D�>�D�� D��qD���D�@ D�}qD���D�  D�@ D��HD�� D��qD�>�D�� D���D�  D�AHD�� D�� D���D�>�D�~�D���D��qD�=qD�� D��HD��qD�AHD��HD��qD�  D�AHD��HD��HD�HD�AHD���D�� D���D�>�D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD���D�D�  D�=qD�}qD���D���D�>�D�~�D�� D�  D�AHD�� D��qD���D�>�D�~�D�� D�HD�AHD���D��HD���D�@ D�~�D��)D�HD�AHD�� D��HD�HD�B�D���D�� D��qD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�AHD�� D�� D�HD�@ D�� D���D���D�@ D��HD�� D���D�>�D�~�D���D���D�>�D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD�  D�@ D�~�D�� D���D�@ DÂ�D��HD���D�>�D�~�D�� D�  D�>�Dŀ D�� D�  D�>�Dƀ D�� D���D�AHDǀ D�� D�  D�@ DȀ D��HD�  D�@ Dɀ D��HD�  D�@ Dʀ D�� D�HD�>�Dˀ D�� D�  D�>�D̀ D��HD�HD�AHD́HD��HD�HD�@ D�~�Dξ�D�  D�AHDπ DϾ�D���D�>�D�~�D��HD��D�AHDсHD�� D���D�>�D�~�DҽqD��qD�=qD�}qDӽqD���D�@ DԀ D��HD�  D�>�DՀ Dվ�D���D�@ Dր D־�D���D�@ D׀ D׾�D�  D�@ D؁HD��HD�  D�>�D�}qDپ�D�  D�>�D�~�D�� D�HD�@ D�~�D۾�D�  D�AHD܁HD��HD�HD�AHD݁HD��HD���D�=qDހ D��HD�  D�>�D�}qD߾�D�  D�AHD��HD�� D�  D�B�D�HD�� D�  D�>�D� D�� D���D�AHDわD��HD�HD�B�D�HD�� D�  D�@ D� D徸D�HD�@ D� D�� D�  D�@ D� D�� D�  D�AHD�~�D辸D�HD�AHD�~�D龸D���D�@ D� D�� D�  D�>�D�HD��HD���D�>�D�~�D�� D�  D�AHD�HD�� D��qD�>�D�HD��HD�HD�AHD�HD�D�  D�AHD���D�� D�  D�AHD�~�D�� D�HD�@ D�~�D�� D��D�AHD� D�qD�  D�@ D�HD��HD�  D�>�D�}qD���D�  D�AHD���D��HD���D�=qD�~�D�� D�HD�AHD�~�D��qD���D�@ D�}qD���D��D�0�>�?�?L��?�=q?���?��?��@�@
=@&ff@8Q�@G�@Y��@h��@u@��
@��@�z�@��H@��\@�=q@��@��H@\@�=q@��@ٙ�@�G�@�@�z�@�p�A33A
=A(�AG�A�A��A{A"�\A'�A,(�A1G�A5A:=qA>{AB�\AG�AL(�AP��AU�AY��A]p�AaG�AfffAj=qAn�RAr�\Aw
=A{�A�Q�A��\A���A�
=A���A�33A�p�A��A��A�(�A�{A�  A�=qA���A�
=A���A��A�p�A��A��A�z�A�ffA���A�33A�A��A�=qA�z�A��RA�G�AÅA�A�  A�=qA�z�AθRA���A��HA�p�A׮A��A�(�A�ffA���A��HA��A�\)A陚A��
A�A�  A�=qA�z�A��RA�G�A��A�B   B�B=qB\)B��B��B�RB�B��B
{B
=B  BG�BffB�B��B�B
=B(�Bp�BffB�B��BB�HB  BG�BffB�B ��B!�B#
=B$  B%�B&=qB'�B(��B)B+33B,Q�B-p�B.�\B/�B0��B1�B3
=B4(�B5G�B6�\B7�
B8��B9�B;
=B<(�B=p�B>�RB?�
B@��BB{BC33BDz�BE��BF�HBH  BI�BJ�\BK�BL��BM��BN�HBP  BQG�BR�\BS�BT��BU�BV�HBX(�BYG�BZffB[�B\��B]�B_
=B`(�BaG�Bb{Bc�Bd��BeBg
=Bg�
Bh��Bj=qBk�Bl��Bm�Bo33BpQ�Bqp�Br�\Bs�Bt��Bv{Bw\)Bxz�ByB{
=B|  B}�B~{B\)B�(�B��RB�G�B��B�z�B��B���B�(�B��RB�33B��
B�z�B�
=B��B�=qB���B�\)B�{B���B�G�B��
B�ffB���B��B�(�B��RB�\)B�  B�z�B�
=B���B�{B��RB�\)B�  B��\B��B��B�=qB��HB�p�B�(�B��RB�\)B�  B��\B�33B�B�z�B��B�B�ffB���B��B�=qB��HB��B�(�B���B�\)B��B��\B�33B��B��\B��B��B�Q�B���B���B�(�B��RB�\)B��B���B�G�B��B�z�B�
=B�B�z�B�33B��
B�ffB��B��
B��\B�G�B�{B��RB�\)B�(�B��HB���B�=qB��HB���B�Q�B��B��
B�z�B��B��
B���B�p�B�{B¸RB�p�B�=qB���B�B�ffB��B�Bȏ\B�G�B�{BʸRB�\)B�{B���B͙�B�Q�B�
=BϮB�ffB�
=B��
Bң�B�33B��BԸRB�p�B�(�B��HBׅB�=qB���BٮB�z�B��B�Bܣ�B�\)B�(�B��HBߙ�B�ffB�33B�  B��B�p�B�=qB�
=B�B�z�B��B��B�RB�B�Q�B���B�B�z�B�\)B�{B���BB�z�B�\)B�{B���B�B�z�B�\)B�{B���B��B�z�B�G�B�  B���B��B�z�B�33B�  B���B��C =qC ��C  Cp�C�
C33C��C
=CffC��C=qC��C
=Cp�C��C=qC��C
=C\)CC33C��C��C	Q�C	C
33C
�\C
�C\)CC�Cz�C�
CG�C�C
=CffC�
CG�C��C  Cp�CC(�C�\C  C\)C�RC(�C�\C�CG�CC�C�C�
CG�C�C
=C\)C��C33C�\C�CQ�CC{Cp�C�
CG�C��C��CffC��C�Cz�C�CQ�C��C
=Cp�CC �C z�C �HC!33C!z�C!��C"(�C"ffC"��C"�C#(�C#Q�C#�\C#��C#��C${C$G�C$z�C$��C$��C%  C%33C%Q�C%p�C%�C%�HC&  C&�C&Q�C&�C&�C&��C'  C'33C'\)C'z�C'�C'�HC(  C(�C(\)C(�\C(�C(�
C)
=C)=qC)\)C)�C)�RC)�C*{C*=qC*p�C*��C*C*��C+(�C+G�C+p�C+��C+�
C,  C,(�C,\)C,�C,�C,�HC-{C-=qC-ffC-��C-C-�C.�C.Q�C.p�C.�C.�
C/  C/(�C/ffC/�\C/�C/�C0�C0G�C0p�C0�C0�HC1  C1=qC1p�C1��C1C2  C233C2ffC2��C2��C3  C3=qC3p�C3��C3�
C4{C4=qC4p�C4�C4�HC5
=C5Q�C5z�C5�C5�C6�C6G�C6�\C6C6��C7(�C7ffC7��C7��C8
=C8=qC8p�C8�C8�C9�C9\)C9��C9��C:  C:G�C:z�C:�C:��C;(�C;\)C;��C;�
C<
=C<G�C<�C<�RC<��C=33C=\)C=��C=�C>{C>Q�C>��C>C?  C?G�C?p�C?�C?��C@�C@ffC@��C@�
CA�CA\)CA�\CA��CB{CBG�CBz�CB��CB��CC33CCz�CC�RCC�CD=qCDp�CD�CD��CE�CEp�CE�CE�HCF33CFp�CF��CF��CG33CGffCG�RCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               ?�  @�\@@  @�  @�G�@�  @�  AG�A��A ��A,(�A?\)A`  A�  A���A���A�Q�A���A�Q�A�  A�  B   B�
B�
B(�B z�B((�B/�
B8  B@  BH  BO�
BW�
B_�
Bg�Bp  Bx(�B�{B�  B��B�  B�{B�{B�(�B�  B��B�  B�  B�  B�  B��
B�  B�  B��B��
B��
B��B�{B�  B��B��B�  B�  B�  B��B��
B��
B��
B�  C �C  C�C�C  C

=C  C��C  C
=C
=C  C��C  C  C  C��C"  C$
=C&  C'��C*  C,  C.  C/��C2  C4
=C6  C8  C9��C<  C>  C@  CB
=CD  CF  CH  CJ
=CL{CN  CO��CR  CT  CU��CX  CY��C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co��Cr  Cs��Cu��Cx  Cz  C{��C~  C�C�  C�  C�  C�C�  C�  C�C�  C���C�  C�  C���C���C���C�  C���C�  C�
=C�C���C���C�C�  C���C�  C�  C�  C�  C�  C�C�  C�  C�  C���C���C���C���C�  C���C�  C�C�  C�  C���C�  C�C�C�C�  C�C�
=C�C���C���C���C���C���C�  C�  C���C���C�  C�C�
=C�C���C���C���C�C�  C���C���C���C�  C�C���C���C�  C�C�  C�C�C�  C�  C�  C�
=C�
=C�  C�C�C�C�  C�C�C�  C�
=C�C�  C�  C���C���C�  C�  C�C�C�C���C�  C�
=C�  C���C���C���C�  C�  C�C�  C���C�  C���C���C�  C�  C�  C�  C�  C�  D   D � D�D��D�qD� D  Dz�D�qD� D�D�D�D��D  D��D  Dz�D	  D	�D
�D
�DD��D�D}qD  D� D�D�D�D� D�D��D  D�D�D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D��DD� D  D� D  D��D�qD � D!�D!}qD"  D"� D"��D#}qD$  D$}qD%  D%� D%�qD&� D'  D'��D(D(� D)�D)��D)�qD*� D+�D+� D,�D,�D-�D-� D-�qD.z�D/  D/��D0�D0� D0�qD1}qD1��D2}qD3  D3� D3�qD4� D5D5��D6�D6� D6�qD7� D8�D8��D9  D9}qD:  D:� D:�qD;� D<�D<��D=  D=� D=�qD>� D>�qD?� D@  D@}qDA  DA� DB  DB��DC�DC� DC�qDD}qDE  DE� DF  DF�DGDG� DH  DH��DI  DI� DJ�DJ��DK�DK� DL  DL� DL��DM}qDN  DN}qDO  DO� DO�qDP}qDQ  DQ� DQ�qDR� DS�DS��DT  DT� DU  DU��DV  DV}qDW  DW� DW��DX}qDY�DY��DZ  DZ� D[�D[� D\  D\��D]  D]}qD^  D^��D_  D_� D`�D`��Da  Da� Db  Db��Dc  Dc� Dd  Dd}qDd�qDe}qDe�qDf}qDg  Dg� Dg�qDh� Di�Di��Dj  Dj� Dk  Dk��DlDl��Dm  Dm��DnDn��Dn�qDo}qDp  Dp� Dq  Dq� Dr  Dr��Ds  Ds� Dt  Dt}qDu�Du� Dv  Dv��Dw�Dw� Dx  Dx� Dx�qDy}qDy�qDz� D{  D{z�D{��D|}qD}�D}� D}�qD~}qD  D� D�HD�AHD�� D���D��qD�>�D�� D���D�  D�AHD�� D��HD��D�AHD�� D�� D�HD�AHD��HD�D�  D�>�D�� D��HD���D�@ D�� D���D�HD�AHD��HD��HD�  D�>�D�� D�� D���D�@ D�� D���D���D�AHD��HD�� D�HD�AHD�� D��qD�HD�AHD�~�D�� D�  D�AHD�~�D��qD�  D�AHD�� D���D�  D�>�D�}qD���D�HD�@ D�~�D���D���D�@ D�� D�� D���D�>�D�}qD���D��D�B�D�� D�� D�HD�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD�� D�� D�  D�@ D�~�D���D�HD�@ D�}qD��qD�  D�@ D�� D�� D���D�>�D�~�D�� D��qD�>�D��HD��HD�  D�@ D�� D�D��D�@ D�� D�� D�HD�AHD�~�D���D�  D�B�D��HD���D���D�AHD��HD���D���D�@ D�� D��HD���D�>�D�� D��qD���D�@ D�}qD���D�  D�@ D��HD�� D��qD�>�D�� D���D�  D�AHD�� D�� D���D�>�D�~�D���D��qD�=qD�� D��HD��qD�AHD��HD��qD�  D�AHD��HD��HD�HD�AHD���D�� D���D�>�D�� D�� D�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD���D�D�  D�=qD�}qD���D���D�>�D�~�D�� D�  D�AHD�� D��qD���D�>�D�~�D�� D�HD�AHD���D��HD���D�@ D�~�D��)D�HD�AHD�� D��HD�HD�B�D���D�� D��qD�>�D�~�D�� D�  D�@ D�~�D�� D�HD�@ D��HD��HD�HD�AHD�� D�� D�HD�@ D�� D���D���D�@ D��HD�� D���D�>�D�~�D���D���D�>�D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD�  D�@ D�~�D�� D���D�@ DÂ�D��HD���D�>�D�~�D�� D�  D�>�Dŀ D�� D�  D�>�Dƀ D�� D���D�AHDǀ D�� D�  D�@ DȀ D��HD�  D�@ Dɀ D��HD�  D�@ Dʀ D�� D�HD�>�Dˀ D�� D�  D�>�D̀ D��HD�HD�AHD́HD��HD�HD�@ D�~�Dξ�D�  D�AHDπ DϾ�D���D�>�D�~�D��HD��D�AHDсHD�� D���D�>�D�~�DҽqD��qD�=qD�}qDӽqD���D�@ DԀ D��HD�  D�>�DՀ Dվ�D���D�@ Dր D־�D���D�@ D׀ D׾�D�  D�@ D؁HD��HD�  D�>�D�}qDپ�D�  D�>�D�~�D�� D�HD�@ D�~�D۾�D�  D�AHD܁HD��HD�HD�AHD݁HD��HD���D�=qDހ D��HD�  D�>�D�}qD߾�D�  D�AHD��HD�� D�  D�B�D�HD�� D�  D�>�D� D�� D���D�AHDわD��HD�HD�B�D�HD�� D�  D�@ D� D徸D�HD�@ D� D�� D�  D�@ D� D�� D�  D�AHD�~�D辸D�HD�AHD�~�D龸D���D�@ D� D�� D�  D�>�D�HD��HD���D�>�D�~�D�� D�  D�AHD�HD�� D��qD�>�D�HD��HD�HD�AHD�HD�D�  D�AHD���D�� D�  D�AHD�~�D�� D�HD�@ D�~�D�� D��D�AHD� D�qD�  D�@ D�HD��HD�  D�>�D�}qD���D�  D�AHD���D��HD���D�=qD�~�D�� D�HD�AHD�~�D��qD���D�@ D�}qD���D��G�O�>�?�?L��?�=q?���?��?��@�@
=@&ff@8Q�@G�@Y��@h��@u@��
@��@�z�@��H@��\@�=q@��@��H@\@�=q@��@ٙ�@�G�@�@�z�@�p�A33A
=A(�AG�A�A��A{A"�\A'�A,(�A1G�A5A:=qA>{AB�\AG�AL(�AP��AU�AY��A]p�AaG�AfffAj=qAn�RAr�\Aw
=A{�A�Q�A��\A���A�
=A���A�33A�p�A��A��A�(�A�{A�  A�=qA���A�
=A���A��A�p�A��A��A�z�A�ffA���A�33A�A��A�=qA�z�A��RA�G�AÅA�A�  A�=qA�z�AθRA���A��HA�p�A׮A��A�(�A�ffA���A��HA��A�\)A陚A��
A�A�  A�=qA�z�A��RA�G�A��A�B   B�B=qB\)B��B��B�RB�B��B
{B
=B  BG�BffB�B��B�B
=B(�Bp�BffB�B��BB�HB  BG�BffB�B ��B!�B#
=B$  B%�B&=qB'�B(��B)B+33B,Q�B-p�B.�\B/�B0��B1�B3
=B4(�B5G�B6�\B7�
B8��B9�B;
=B<(�B=p�B>�RB?�
B@��BB{BC33BDz�BE��BF�HBH  BI�BJ�\BK�BL��BM��BN�HBP  BQG�BR�\BS�BT��BU�BV�HBX(�BYG�BZffB[�B\��B]�B_
=B`(�BaG�Bb{Bc�Bd��BeBg
=Bg�
Bh��Bj=qBk�Bl��Bm�Bo33BpQ�Bqp�Br�\Bs�Bt��Bv{Bw\)Bxz�ByB{
=B|  B}�B~{B\)B�(�B��RB�G�B��B�z�B��B���B�(�B��RB�33B��
B�z�B�
=B��B�=qB���B�\)B�{B���B�G�B��
B�ffB���B��B�(�B��RB�\)B�  B�z�B�
=B���B�{B��RB�\)B�  B��\B��B��B�=qB��HB�p�B�(�B��RB�\)B�  B��\B�33B�B�z�B��B�B�ffB���B��B�=qB��HB��B�(�B���B�\)B��B��\B�33B��B��\B��B��B�Q�B���B���B�(�B��RB�\)B��B���B�G�B��B�z�B�
=B�B�z�B�33B��
B�ffB��B��
B��\B�G�B�{B��RB�\)B�(�B��HB���B�=qB��HB���B�Q�B��B��
B�z�B��B��
B���B�p�B�{B¸RB�p�B�=qB���B�B�ffB��B�Bȏ\B�G�B�{BʸRB�\)B�{B���B͙�B�Q�B�
=BϮB�ffB�
=B��
Bң�B�33B��BԸRB�p�B�(�B��HBׅB�=qB���BٮB�z�B��B�Bܣ�B�\)B�(�B��HBߙ�B�ffB�33B�  B��B�p�B�=qB�
=B�B�z�B��B��B�RB�B�Q�B���B�B�z�B�\)B�{B���BB�z�B�\)B�{B���B�B�z�B�\)B�{B���B��B�z�B�G�B�  B���B��B�z�B�33B�  B���B��C =qC ��C  Cp�C�
C33C��C
=CffC��C=qC��C
=Cp�C��C=qC��C
=C\)CC33C��C��C	Q�C	C
33C
�\C
�C\)CC�Cz�C�
CG�C�C
=CffC�
CG�C��C  Cp�CC(�C�\C  C\)C�RC(�C�\C�CG�CC�C�C�
CG�C�C
=C\)C��C33C�\C�CQ�CC{Cp�C�
CG�C��C��CffC��C�Cz�C�CQ�C��C
=Cp�CC �C z�C �HC!33C!z�C!��C"(�C"ffC"��C"�C#(�C#Q�C#�\C#��C#��C${C$G�C$z�C$��C$��C%  C%33C%Q�C%p�C%�C%�HC&  C&�C&Q�C&�C&�C&��C'  C'33C'\)C'z�C'�C'�HC(  C(�C(\)C(�\C(�C(�
C)
=C)=qC)\)C)�C)�RC)�C*{C*=qC*p�C*��C*C*��C+(�C+G�C+p�C+��C+�
C,  C,(�C,\)C,�C,�C,�HC-{C-=qC-ffC-��C-C-�C.�C.Q�C.p�C.�C.�
C/  C/(�C/ffC/�\C/�C/�C0�C0G�C0p�C0�C0�HC1  C1=qC1p�C1��C1C2  C233C2ffC2��C2��C3  C3=qC3p�C3��C3�
C4{C4=qC4p�C4�C4�HC5
=C5Q�C5z�C5�C5�C6�C6G�C6�\C6C6��C7(�C7ffC7��C7��C8
=C8=qC8p�C8�C8�C9�C9\)C9��C9��C:  C:G�C:z�C:�C:��C;(�C;\)C;��C;�
C<
=C<G�C<�C<�RC<��C=33C=\)C=��C=�C>{C>Q�C>��C>C?  C?G�C?p�C?�C?��C@�C@ffC@��C@�
CA�CA\)CA�\CA��CB{CBG�CBz�CB��CB��CC33CCz�CC�RCC�CD=qCDp�CD�CD��CE�CEp�CE�CE�HCF33CFp�CF��CF��CG33CGffCG�RCG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AƶFAƶFAƲ-Aư!AƬAƶFAƶFAƺ^AƝ�AƓuA�dZA�XA��A�\)A�bA�ĜA�dZA��A���A��/AþwAÇ+A�E�A��A��;A¥�A�v�A�&�A�  A���A�~�A�S�A��A��\A�C�A���A��uA�t�A��+A���A���A��9A��A�A��A�9XA��#A��A�A��wA��FA�jA���A�+A�VA��
A��wA��hA�  A��\A�-A���A�?}A�VA���A��-A��A��wA���A�hsA��/A��A���A�I�A��A��A�Q�A�VA���A�hsA�ȴA�dZA��A��mA���A���A�?}A��FA�z�A�1A���A���A���A��DA�O�A�\)A�/A�/A�dZA}��A{S�Ax��Aw��Au&�Ar��An�Ak�PAgdZAf��AdĜAc�Aa��A`(�A^�9A\�HAY�TAV��AVbNAUK�AR�APJAN��AM
=AK�AJ�HAH�9ADZAA%A?C�A=XA;�A:I�A8bNA6jA4��A3hsA0�!A-��A,z�A+��A)�7A(^5A'�FA&�uA#�-A!t�A�A��Az�A��A"�A�\A�A~�AƨA�`AZAx�A5?A�A�;A�AjA�A&�Az�AoA	�A	C�A	oA��A�7A~�AE�AbA��A�yA��A`BA5?A ��@�
=@�v�@��9@��@��;@�"�@���@�^5@���@��@�"�@���@��P@���@�l�@�@�^5@�5?@�=q@���@��;@�j@��/@�bN@���@�M�@�33@@���@�@�-@�M�@��@��-@�V@�I�@�1@�  @� �@�D@���@��@���@��@�j@�(�@��@�C�@�dZ@웦@�x�@���@�(�@��@땁@��y@�^@�j@��@�/@�9@�j@�9X@���@�E�@�x�@�V@�bN@� �@��
@ߥ�@��@�^5@�@���@݉7@���@��@��/@�7@��@�A�@�9X@ߥ�@ݲ-@�r�@�bN@�r�@�Z@ׅ@�`B@�z�@�A�@Ԭ@���@�dZ@��@���@�E�@�7L@�Q�@�bN@щ7@Ѳ-@���@�=q@�V@Ώ\@��;@�1'@ϕ�@ϕ�@�;d@�$�@�-@��@�r�@�C�@Ƈ+@�@��@Ĭ@�z�@�Q�@þw@�S�@�+@���@Ý�@�n�@�J@�O�@�7L@�hs@���@�j@�t�@��D@�&�@��/@�r�@��P@�dZ@���@��w@�ƨ@�S�@�o@�S�@�|�@��@���@�E�@��@��@� �@�  @�1@���@�33@��@���@�n�@��#@�%@���@���@�dZ@��@��+@���@�7L@��/@��u@�j@���@��y@��+@��+@�v�@�M�@��@�hs@���@�j@�ƨ@�t�@��@��H@��+@�M�@�{@��T@�X@�7L@��@��@��j@��u@�Z@� �@�dZ@��y@�~�@�{@��7@�hs@�/@��/@�Q�@���@���@�"�@��!@�~�@�@��7@�p�@�hs@�X@�%@�9X@���@���@�dZ@��@�5?@�p�@�/@��@���@�bN@���@��
@���@�~�@��@��@��@�/@�Ĝ@��D@�A�@��m@�ƨ@���@�\)@�+@�
=@���@�M�@�@��@���@��7@��@�/@�Ĝ@��D@�Q�@�1'@� �@�  @���@���@��@�\)@�;d@�33@���@��\@��@��T@�@���@��7@�`B@��@���@��/@��j@���@�Z@�b@��w@��@�@��@��!@�E�@��@���@�hs@��@���@��@��@���@���@�l�@�S�@�"�@�
=@�@���@�n�@���@���@���@���@��h@�/@��@���@���@��9@��@�9X@���@��F@��@�;d@�
=@��y@�ȴ@��!@��\@�v�@�@�X@�%@���@��/@���@�Q�@�1@��;@��P@�33@��H@��+@�V@���@��#@�/@�Ĝ@��@���@�Z@��m@��P@�
=@���@��+@��+@�~�@�V@�$�@�{@���@���@�?}@��/@��u@�I�@�9X@�b@�P@\)@K�@+@~�R@~��@~��@~v�@}�T@}�h@|��@|j@{�
@{@z�H@z��@z�!@y��@yhs@y%@w�;@wK�@w+@v��@v��@v�+@vv�@v{@u��@u?}@t�j@tz�@t(�@t�@s�m@s��@s@r�@q��@qhs@p��@p �@o\)@o+@o
=@n��@nv�@nV@n$�@m�@m�@l��@lj@kƨ@ko@j��@j��@j��@jM�@i��@ix�@i�@h�`@h��@hr�@h  @g�w@g�w@gl�@g;d@g
=@f�y@f�@f��@f�y@fv�@e��@e/@d��@d�@dI�@d1@c�
@c�@co@b��@b=q@a��@a�7@ahs@`Ĝ@`r�@_�@_|�@_l�@_K�@^��@^��@^E�@]��@]?}@\��@\�j@\�D@\I�@\�@\1@[�m@[ƨ@[��@["�@Z�!@ZM�@ZJ@Y�#@Y��@X�`@X�u@X  @WK�@W
=@V�+@V$�@U��@U/@T��@Sƨ@SC�@S"�@S@R��@R^5@R-@Q��@Q�#@Qx�@Q&�@Q%@PbN@Pb@O�P@N�@N5?@M@MV@L��@L9X@Kƨ@K�@K33@J�@J^5@I�@I�7@Ihs@Ihs@I&�@H�9@HQ�@H  @G�P@F��@F��@FE�@E�@Ep�@D�@D9X@D1@C�m@C��@CdZ@C"�@B�H@B�@A��@Ax�@A7L@@Q�@?��@?�@?�P@?+@>��@>��@>@=��@=��@=��@=/@<��@<j@<9X@<(�@;�
@;��@;�@;dZ@;o@:�!@:~�@:�@9x�@97L@9&�@9&�@9�@9%@8��@8A�@81'@7��@7\)@6��@6ȴ@6ȴ@6�+@6E�@6{@5�-@5�@4�D@49X@3��@3��@3S�@2�H@2^5@2=q@2-@2�@2J@1��@1�@1�^@1��@1G�@0��@0�9@0��@0r�@0 �@/�@/�;@/�@/�P@/l�@/�@.�@.v�@.V@.5?@-p�@,�@,��@,9X@+ƨ@+��@+��@+�@*�H@*��@*�\@*~�@)�@)��@)G�@)&�@)�@)%@(r�@( �@(b@'��@'��@'K�@'�@&��@&V@&$�@%@%`B@%O�@$�@$��@$z�@$Z@$�@#�
@#��@#C�@#33@#"�@"�@"��@"-@"J@!�@!��@!x�@!&�@ ��@ �9@ ��@ bN@  �@   @��@|�@;d@��@�@��@V@V@E�@{@�T@�@?}@/@V@��@��@z�@��@�F@��@�@dZ@S�@33@o@@�@�H@��@�\@M�@�@��@�^@hs@&�@Ĝ@��@�u@r�@A�@  @�@��@|�@\)@��@�R@�R@��@v�@{@@�@��@@��@�@`B@O�@?}@�@V@�@�/@��@�@z�@j@Z@I�@��@��@�@S�@33@"�@��@~�@^5@M�@=q@-@�@�@��@X@7L@7L@&�@%@��@��@A�@b@�@�P@\)@K�@K�@+@
=@�y@ȴ@�R@��@��@v�@V@E�@@�-@O�@V@�/@�j@�D@j@j@Z@9X@9X@(�@�@�F@��@��@��@��@t�@33@33@o@
�H@
�H@
��@
��@
n�@
M�@
-@	�@	�#@	��@	��@	��@	X@	X@	�@Ĝ@�9@��AƲ-Aƴ9AƶFAƴ9AƸRAƸRAƶFAƴ9AƸRAƲ-AƲ-AƲ-AƲ-Aƴ9AƮAƮAƮAƧ�AƧ�Aư!AƮAƶFAƸRAƴ9Aƴ9AƶFAƶFAƴ9AƶFAƾwAƼjAƺ^Aƴ9Aƕ�Aơ�AƗ�AƓuAƏ\Aƙ�AƑhA�hsA�hsAƃA�t�A�^5A�G�A�?}A�1'A�33A�E�A�$�A�33A�-A�{A�%A�ƨAś�AŋDA�Q�A�G�A�A�A�5?A�7LA�/A��A��A�{A��A�VA���A��A��A���A���A�ĜAľwAĬAġ�Aě�A�~�A�r�A�`BA�M�A�=qA�-A�&�A�"�A��A��A��A�{A��A�bA�
=A�%A�A��A��A��yA��`A��HA��HA��/A��A���A���A���A���A�ƨA�ȴAöFAò-Aé�AÓuAËDAÉ7AÉ7AÇ+A�|�A�t�A�l�A�ZA�M�A�A�A�7LA�33A�+A�(�A�(�A�&�A�"�A��A��A�VA�A���A��A��`A���A���A���A�ƨAº^A´9A®A£�A�APA+AADA�|�A�n�A�dZA�hsA�hsA�dZA�9XA�VA�%A�A�
=A�JA�1A�A�A�A���A��A��HA��;A��A���A���A��jA���A��uA��A�z�A��A��A�v�A�ffA�VA�VA�ZA�dZA�bNA�G�A� �A�JA�1A���A��;A���A���A���A�ƨA��jA���A��+A�l�A�`BA�VA�O�A�I�A�C�A�C�A�C�A�?}A�1'A�{A�A���A���A��A��yA��HA���A��!A���A��+A�t�A�hsA�ffA�ffA�hsA�p�A�x�A��A��A��A�~�A�~�A��+A��DA��7A��PA��hA���A���A��hA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���A��FA�A���A��
A��#A��yA��A��A��A���A���A���A���A�  A�
=A�bA�1A�A�
=A��A�"�A�&�A�A�A�^5A�\)A�K�A�1'A��A�oA�VA�  A��A��HA���A��RA���A�t�A�oA��HA���A��+A�jA�M�A�33A�{A���A��hA�;dA��HA��\A��A��A��A��A��jA�Q�A��A��
A�^5A��A���A��A�r�A�hsA�XA�G�A�"�A��A��9A�r�A�C�A�9XA�5?A�-A�+A�&�A� �A��A��A�oA�JA���A��A��yA��#A���A���A���A�ȴA�ĜA�A��wA��jA��-A���A���A���A��uA�v�A�`BA�I�A�1'A� �A��/A���A�+A���A��A�z�A�n�A�dZA�S�A��;A�  A���A�"�A���A�jA�=qA��A�p�A�&�A��TA��+A�  A���A���A��+A�l�A�XA�C�A� �A�
=A�A��HA��wA���A�9XA�
=A���A��hA�JA��wA���A���A��A�E�A��
A���A��wA��FA��!A��9A��wA���A���A���A���A��PA�z�A�l�A�=qA�=qA�1'A�  A�A�~�A�I�A�33A��A�1A���A��A��A���A���A��uA��A�t�A�K�A�9XA�(�A��A��A��^A��+A�$�A��
A���A�|�A�t�A�r�A�p�A�r�A�ffA�XA�=qA�bA��TA���A�M�A��A�ƨA�S�A���A��\A�^5A�33A��A�A��A�bNA��RA�9XA���A���A���A��\A��\A��DA�t�A�ZA�M�A��A��wA�r�A�bA��\A�{A���A���A�ƨA���A�r�A�JA�+A��yA��A�|�A�M�A�-A�bA��A���A���A�r�A�O�A�7LA�+A�(�A�(�A�"�A��A�
=A��A���A�ƨA��jA���A��hA�v�A�VA�7LA��A�A��A��;A��!A��uA�|�A�`BA�=qA��#A�Q�A�oA��#A��-A���A�ffA�M�A��A��A��A�^5A�oA��`A���A�ƨA��^A���A��A�;dA���A�;dA�x�A�(�A��A��jA��RA��9A��!A���A��PA�E�A���A��A�33A��mA���A�;dA�oA�ƨA�A�A��#A���A�ZA��A��yA���A�bA���A�G�A�%A���A�n�A�
=A�
=A�+A�C�A�XA�XA�VA�bA��A��HA��^A�p�A�  A��wA���A�r�A�E�A��A���A���A��\A�`BA�1'A���A��RA��A�~�A�ffA�Q�A�C�A�9XA�-A� �A�VA��A���A��-A��hA��A��DA�VA��A���A�n�A�-A��A���A��-A��+A�XA�$�A�A��AK�A33A~�HA~�+A}�mA}C�A|�HA|z�A|v�A|bNA|-A|JA{�;A{��A{�A{
=Azz�AzbAy�FAyhsAy`BAy�Ax��Ax��Ax�9Ax��Axz�AxbNAx^5AxM�Ax$�AxJAw��Aw�AwVAvbNAu�Au�-Au��Aut�Au+At�9At5?As�#As�-As`BAr�/Ar�!Ar9XAr-Aq�Aq��Aq�hApVAo/An9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               AƶFAƶFAƲ-Aư!AƬAƶFAƶFAƺ^AƝ�AƓuA�dZA�XA��A�\)A�bA�ĜA�dZA��A���A��/AþwAÇ+A�E�A��A��;A¥�A�v�A�&�A�  A���A�~�A�S�A��A��\A�C�A���A��uA�t�A��+A���A���A��9A��A�A��A�9XA��#A��A�A��wA��FA�jA���A�+A�VA��
A��wA��hA�  A��\A�-A���A�?}A�VA���A��-A��A��wA���A�hsA��/A��A���A�I�A��A��A�Q�A�VA���A�hsA�ȴA�dZA��A��mA���A���A�?}A��FA�z�A�1A���A���A���A��DA�O�A�\)A�/A�/A�dZA}��A{S�Ax��Aw��Au&�Ar��An�Ak�PAgdZAf��AdĜAc�Aa��A`(�A^�9A\�HAY�TAV��AVbNAUK�AR�APJAN��AM
=AK�AJ�HAH�9ADZAA%A?C�A=XA;�A:I�A8bNA6jA4��A3hsA0�!A-��A,z�A+��A)�7A(^5A'�FA&�uA#�-A!t�A�A��Az�A��A"�A�\A�A~�AƨA�`AZAx�A5?A�A�;A�AjA�A&�Az�AoA	�A	C�A	oA��A�7A~�AE�AbA��A�yA��A`BA5?A ��@�
=@�v�@��9@��@��;@�"�@���@�^5@���@��@�"�@���@��P@���@�l�@�@�^5@�5?@�=q@���@��;@�j@��/@�bN@���@�M�@�33@@���@�@�-@�M�@��@��-@�V@�I�@�1@�  @� �@�D@���@��@���@��@�j@�(�@��@�C�@�dZ@웦@�x�@���@�(�@��@땁@��y@�^@�j@��@�/@�9@�j@�9X@���@�E�@�x�@�V@�bN@� �@��
@ߥ�@��@�^5@�@���@݉7@���@��@��/@�7@��@�A�@�9X@ߥ�@ݲ-@�r�@�bN@�r�@�Z@ׅ@�`B@�z�@�A�@Ԭ@���@�dZ@��@���@�E�@�7L@�Q�@�bN@щ7@Ѳ-@���@�=q@�V@Ώ\@��;@�1'@ϕ�@ϕ�@�;d@�$�@�-@��@�r�@�C�@Ƈ+@�@��@Ĭ@�z�@�Q�@þw@�S�@�+@���@Ý�@�n�@�J@�O�@�7L@�hs@���@�j@�t�@��D@�&�@��/@�r�@��P@�dZ@���@��w@�ƨ@�S�@�o@�S�@�|�@��@���@�E�@��@��@� �@�  @�1@���@�33@��@���@�n�@��#@�%@���@���@�dZ@��@��+@���@�7L@��/@��u@�j@���@��y@��+@��+@�v�@�M�@��@�hs@���@�j@�ƨ@�t�@��@��H@��+@�M�@�{@��T@�X@�7L@��@��@��j@��u@�Z@� �@�dZ@��y@�~�@�{@��7@�hs@�/@��/@�Q�@���@���@�"�@��!@�~�@�@��7@�p�@�hs@�X@�%@�9X@���@���@�dZ@��@�5?@�p�@�/@��@���@�bN@���@��
@���@�~�@��@��@��@�/@�Ĝ@��D@�A�@��m@�ƨ@���@�\)@�+@�
=@���@�M�@�@��@���@��7@��@�/@�Ĝ@��D@�Q�@�1'@� �@�  @���@���@��@�\)@�;d@�33@���@��\@��@��T@�@���@��7@�`B@��@���@��/@��j@���@�Z@�b@��w@��@�@��@��!@�E�@��@���@�hs@��@���@��@��@���@���@�l�@�S�@�"�@�
=@�@���@�n�@���@���@���@���@��h@�/@��@���@���@��9@��@�9X@���@��F@��@�;d@�
=@��y@�ȴ@��!@��\@�v�@�@�X@�%@���@��/@���@�Q�@�1@��;@��P@�33@��H@��+@�V@���@��#@�/@�Ĝ@��@���@�Z@��m@��P@�
=@���@��+@��+@�~�@�V@�$�@�{@���@���@�?}@��/@��u@�I�@�9X@�b@�P@\)@K�@+@~�R@~��@~��@~v�@}�T@}�h@|��@|j@{�
@{@z�H@z��@z�!@y��@yhs@y%@w�;@wK�@w+@v��@v��@v�+@vv�@v{@u��@u?}@t�j@tz�@t(�@t�@s�m@s��@s@r�@q��@qhs@p��@p �@o\)@o+@o
=@n��@nv�@nV@n$�@m�@m�@l��@lj@kƨ@ko@j��@j��@j��@jM�@i��@ix�@i�@h�`@h��@hr�@h  @g�w@g�w@gl�@g;d@g
=@f�y@f�@f��@f�y@fv�@e��@e/@d��@d�@dI�@d1@c�
@c�@co@b��@b=q@a��@a�7@ahs@`Ĝ@`r�@_�@_|�@_l�@_K�@^��@^��@^E�@]��@]?}@\��@\�j@\�D@\I�@\�@\1@[�m@[ƨ@[��@["�@Z�!@ZM�@ZJ@Y�#@Y��@X�`@X�u@X  @WK�@W
=@V�+@V$�@U��@U/@T��@Sƨ@SC�@S"�@S@R��@R^5@R-@Q��@Q�#@Qx�@Q&�@Q%@PbN@Pb@O�P@N�@N5?@M@MV@L��@L9X@Kƨ@K�@K33@J�@J^5@I�@I�7@Ihs@Ihs@I&�@H�9@HQ�@H  @G�P@F��@F��@FE�@E�@Ep�@D�@D9X@D1@C�m@C��@CdZ@C"�@B�H@B�@A��@Ax�@A7L@@Q�@?��@?�@?�P@?+@>��@>��@>@=��@=��@=��@=/@<��@<j@<9X@<(�@;�
@;��@;�@;dZ@;o@:�!@:~�@:�@9x�@97L@9&�@9&�@9�@9%@8��@8A�@81'@7��@7\)@6��@6ȴ@6ȴ@6�+@6E�@6{@5�-@5�@4�D@49X@3��@3��@3S�@2�H@2^5@2=q@2-@2�@2J@1��@1�@1�^@1��@1G�@0��@0�9@0��@0r�@0 �@/�@/�;@/�@/�P@/l�@/�@.�@.v�@.V@.5?@-p�@,�@,��@,9X@+ƨ@+��@+��@+�@*�H@*��@*�\@*~�@)�@)��@)G�@)&�@)�@)%@(r�@( �@(b@'��@'��@'K�@'�@&��@&V@&$�@%@%`B@%O�@$�@$��@$z�@$Z@$�@#�
@#��@#C�@#33@#"�@"�@"��@"-@"J@!�@!��@!x�@!&�@ ��@ �9@ ��@ bN@  �@   @��@|�@;d@��@�@��@V@V@E�@{@�T@�@?}@/@V@��@��@z�@��@�F@��@�@dZ@S�@33@o@@�@�H@��@�\@M�@�@��@�^@hs@&�@Ĝ@��@�u@r�@A�@  @�@��@|�@\)@��@�R@�R@��@v�@{@@�@��@@��@�@`B@O�@?}@�@V@�@�/@��@�@z�@j@Z@I�@��@��@�@S�@33@"�@��@~�@^5@M�@=q@-@�@�@��@X@7L@7L@&�@%@��@��@A�@b@�@�P@\)@K�@K�@+@
=@�y@ȴ@�R@��@��@v�@V@E�@@�-@O�@V@�/@�j@�D@j@j@Z@9X@9X@(�@�@�F@��@��@��@��@t�@33@33@o@
�H@
�H@
��@
��@
n�@
M�@
-@	�@	�#@	��@	��@	��@	X@	X@	�@Ĝ@�9G�O�AƲ-Aƴ9AƶFAƴ9AƸRAƸRAƶFAƴ9AƸRAƲ-AƲ-AƲ-AƲ-Aƴ9AƮAƮAƮAƧ�AƧ�Aư!AƮAƶFAƸRAƴ9Aƴ9AƶFAƶFAƴ9AƶFAƾwAƼjAƺ^Aƴ9Aƕ�Aơ�AƗ�AƓuAƏ\Aƙ�AƑhA�hsA�hsAƃA�t�A�^5A�G�A�?}A�1'A�33A�E�A�$�A�33A�-A�{A�%A�ƨAś�AŋDA�Q�A�G�A�A�A�5?A�7LA�/A��A��A�{A��A�VA���A��A��A���A���A�ĜAľwAĬAġ�Aě�A�~�A�r�A�`BA�M�A�=qA�-A�&�A�"�A��A��A��A�{A��A�bA�
=A�%A�A��A��A��yA��`A��HA��HA��/A��A���A���A���A���A�ƨA�ȴAöFAò-Aé�AÓuAËDAÉ7AÉ7AÇ+A�|�A�t�A�l�A�ZA�M�A�A�A�7LA�33A�+A�(�A�(�A�&�A�"�A��A��A�VA�A���A��A��`A���A���A���A�ƨAº^A´9A®A£�A�APA+AADA�|�A�n�A�dZA�hsA�hsA�dZA�9XA�VA�%A�A�
=A�JA�1A�A�A�A���A��A��HA��;A��A���A���A��jA���A��uA��A�z�A��A��A�v�A�ffA�VA�VA�ZA�dZA�bNA�G�A� �A�JA�1A���A��;A���A���A���A�ƨA��jA���A��+A�l�A�`BA�VA�O�A�I�A�C�A�C�A�C�A�?}A�1'A�{A�A���A���A��A��yA��HA���A��!A���A��+A�t�A�hsA�ffA�ffA�hsA�p�A�x�A��A��A��A�~�A�~�A��+A��DA��7A��PA��hA���A���A��hA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���A��FA�A���A��
A��#A��yA��A��A��A���A���A���A���A�  A�
=A�bA�1A�A�
=A��A�"�A�&�A�A�A�^5A�\)A�K�A�1'A��A�oA�VA�  A��A��HA���A��RA���A�t�A�oA��HA���A��+A�jA�M�A�33A�{A���A��hA�;dA��HA��\A��A��A��A��A��jA�Q�A��A��
A�^5A��A���A��A�r�A�hsA�XA�G�A�"�A��A��9A�r�A�C�A�9XA�5?A�-A�+A�&�A� �A��A��A�oA�JA���A��A��yA��#A���A���A���A�ȴA�ĜA�A��wA��jA��-A���A���A���A��uA�v�A�`BA�I�A�1'A� �A��/A���A�+A���A��A�z�A�n�A�dZA�S�A��;A�  A���A�"�A���A�jA�=qA��A�p�A�&�A��TA��+A�  A���A���A��+A�l�A�XA�C�A� �A�
=A�A��HA��wA���A�9XA�
=A���A��hA�JA��wA���A���A��A�E�A��
A���A��wA��FA��!A��9A��wA���A���A���A���A��PA�z�A�l�A�=qA�=qA�1'A�  A�A�~�A�I�A�33A��A�1A���A��A��A���A���A��uA��A�t�A�K�A�9XA�(�A��A��A��^A��+A�$�A��
A���A�|�A�t�A�r�A�p�A�r�A�ffA�XA�=qA�bA��TA���A�M�A��A�ƨA�S�A���A��\A�^5A�33A��A�A��A�bNA��RA�9XA���A���A���A��\A��\A��DA�t�A�ZA�M�A��A��wA�r�A�bA��\A�{A���A���A�ƨA���A�r�A�JA�+A��yA��A�|�A�M�A�-A�bA��A���A���A�r�A�O�A�7LA�+A�(�A�(�A�"�A��A�
=A��A���A�ƨA��jA���A��hA�v�A�VA�7LA��A�A��A��;A��!A��uA�|�A�`BA�=qA��#A�Q�A�oA��#A��-A���A�ffA�M�A��A��A��A�^5A�oA��`A���A�ƨA��^A���A��A�;dA���A�;dA�x�A�(�A��A��jA��RA��9A��!A���A��PA�E�A���A��A�33A��mA���A�;dA�oA�ƨA�A�A��#A���A�ZA��A��yA���A�bA���A�G�A�%A���A�n�A�
=A�
=A�+A�C�A�XA�XA�VA�bA��A��HA��^A�p�A�  A��wA���A�r�A�E�A��A���A���A��\A�`BA�1'A���A��RA��A�~�A�ffA�Q�A�C�A�9XA�-A� �A�VA��A���A��-A��hA��A��DA�VA��A���A�n�A�-A��A���A��-A��+A�XA�$�A�A��AK�A33A~�HA~�+A}�mA}C�A|�HA|z�A|v�A|bNA|-A|JA{�;A{��A{�A{
=Azz�AzbAy�FAyhsAy`BAy�Ax��Ax��Ax�9Ax��Axz�AxbNAx^5AxM�Ax$�AxJAw��Aw�AwVAvbNAu�Au�-Au��Aut�Au+At�9At5?As�#As�-As`BAr�/Ar�!Ar9XAr-Aq�Aq��Aq�hApVAo/An9XG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	B	�B	JB	B	�B	�B	B	xB	�B	(B	1B	�B	7B	-�B	<jB	J�B	W�B	Y�B	VmB	VB	YB	^�B	jB	xB	��B	��B	��B	��B	��B	�=B	��B	�_B	��B	��B	��B	�tB	��B	�-B	��B	�?B	�B	��B
2�B
p�B
�B�B4�BN�B\�B|�B��B��B��B��B��B�B�4B�4B�$B�hB��B��B��B��B�B�B�B�B)_B0UB<�B>�BB�BF�BNBC�BI�BK�B;dB/�BeBB�B�B��B�6B�wB�nB��Be`B/�B
� B
��B
��B
�!B
{�B
d&B
YKB
A�B
-�B
�B

�B
 �B	�B	��B	�UB	�LB	��B	�;B	xB	l�B	cTB	X�B	Q�B	D3B	:�B	$tB	�B	CB	�B	�B��B�	B��B��B�fB�B��B�KB�tB��B��B��B��B�'B�!B��B��B�-B�UB�CB�qB�kB��B�-B�OB�B��B��B��B��B�~B�OB�B�B��B�B�B��B�*B�zB�LB�LB��B��B��B��B��B��B��B�zB��B��B�'B�-B��B��B��B��B�kB��B�hB��B�'B�eB��B��B�B�EBߤB��B�ZB�B�B��B�vB��B�&B�aB��B�`B�2B�B��B��B��B�8B		B	DB	"B	hB	B	�B	�B	B	�B	7B	CB	"hB	'�B	;0B	B[B	HB	K�B	NB	Q�B	U�B	ZB	\�B	`vB	iB	v`B	zDB	xB	w�B	xlB	x�B	v�B	tTB	s�B	r�B	qB	s�B	x�B	v�B	v�B	v�B	v�B	w�B	w2B	wfB	w2B	x�B	}"B	�B	��B	�(B	�MB	��B	��B	��B	�$B	�FB	�XB	��B	��B	��B	�!B	��B	�OB	�B	�@B	�-B	��B	�XB	�_B	�B	�LB	�LB	�B	��B	�0B	��B	�aB	��B	�zB	��B	�hB	�9B	��B	��B	��B	��B	�UB	�BB	��B	�B	�XB	�_B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�tB	�B	��B	��B	��B	�B	�B	��B	�XB	��B	�B	ƨB	�B	��B	�dB	�jB	�B	�B	��B	� B	�KB	ٴB	�B	�KB	�B	�WB	ںB	��B	�BB	�B	� B	��B	��B	�,B	��B	��B	�QB	��B	�]B	�)B	�/B	��B	�5B	��B	��B	�oB	�AB	��B	��B	�B	�|B	�B	�MB	��B	��B	��B	�lB	�B	��B	��B	�lB	�8B	�B	�8B	�	B	�	B	�	B	�rB	�>B	�>B	��B	�rB	�xB	��B	�B	�B	��B	�PB	��B	��B	��B	�(B	��B	�]B	�]B	��B	��B	�]B	��B	��B	��B	��B	�]B	�.B
  B
 �B
;B
�B
{B
�B
GB
�B
�B
{B
B
B
�B
B
�B
YB
�B
_B
�B
1B
�B
�B
	B
	lB
	�B
	�B

rB
B
B
B
�B
�B
�B
�B
PB
B
�B
"B
"B
VB
(B
�B
�B
bB
�B
�B
.B
 B
B
�B
:B
oB
oB
B
@B
uB
�B
�B
�B
FB
FB
�B
�B
�B
MB
�B
B
�B
SB
�B
�B
$B
YB
_B
+B
�B
�B
�B
�B
1B
�B
�B
	B
=B
=B
=B
=B
qB
�B
�B
B
�B
�B
�B
B
�B
B
�B
�B
�B
�B
 'B
 �B
 �B
 \B
!�B
!�B
!�B
!�B
"4B
"�B
#nB
#:B
"�B
#�B
#�B
$�B
$�B
%FB
%zB
%�B
'�B
'�B
'�B
'�B
(�B
)_B
)_B
+B
+�B
+�B
+�B
+kB
,=B
,qB
,qB
,=B
,=B
-wB
-�B
.�B
/�B
/OB
/�B
1'B
1'B
0�B
1[B
1�B
1�B
1�B
1�B
2aB
2-B
2�B
2�B
3hB
3�B
3�B
3hB
3hB
49B
4�B
4�B
6B
6B
6FB
6zB
6zB
6FB
6FB
6�B
7B
7�B
7�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
7�B
7�B
8RB
9XB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9$B
9XB
9�B
:�B
;0B
;0B
:�B
:�B
;�B
<6B
<jB
<�B
<�B
=<B
=�B
=�B
=�B
=qB
>BB
>BB
>�B
>�B
?�B
?�B
@OB
@�B
A�B
A�B
A�B
A�B
B[B
B[B
B[B
B[B
B�B
B�B
C-B
C-B
CaB
C-B
D3B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
FB
FB
FB
FB
F?B
FtB
F�B
F�B
F�B
F�B
F�B
GzB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
J#B
I�B
J�B
K)B
L0B
LdB
MjB
N<B
N<B
NpB
NpB
N�B
N�B
OB
OB
OB
O�B
OB
OB
OvB
N�B
PB
PB
PB
O�B
O�B
O�B
O�B
P}B
P}B
P�B
QNB
Q�B
Q�B
R B
R�B
S�B
S�B
T�B
TaB
UgB
VB
VmB
V9B
VmB
V�B
W?B
XEB
YB
X�B
X�B
YB
YB
YKB
YB
Z�B
Y�B
YB
ZB
[�B
[�B
[�B
[�B
\)B
[�B
\�B
]/B
]/B
\�B
\�B
]dB
]�B
^B
]�B
]�B
]�B
]�B
]�B
^5B
^�B
^�B
^�B
_B
`BB
`vB
`vB
`�B
`�B
aB
a|B
a�B
a�B
b�B
b�B
cTB
cTB
c�B
dZB
d&B
dZB
dZB
e,B
e�B
e�B
e�B
f2B
ffB
gmB
h
B
h
B
h
B
h>B
hsB
hsB
h�B
h�B
h�B
iyB
i�B
i�B
iyB
i�B
i�B
i�B
i�B
jB
i�B
i�B
jB
jB
jB
jKB
jKB
kB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
m)B
ncB
ncB
o B
n�B
n�B
n�B
oiB
o5B
o B
o�B
o�B
pB
pB
poB
poB
p�B
p�B
qAB
qB
q�B
q�B
q�B
rB
rGB
rGB
r�B
r�B
r�B
r�B
sB
s�B
s�B
tB
s�B
s�B
tTB
t�B
t�B
u%B
t�B
u�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
wfB
w�B
wfB
w�B
w�B
w�B
xlB
xlB
xlB
xlB
x�B
y	B
y>B
y�B
y�B
y�B
zB
zDB
zDB
zDB
zxB
z�B
zxB
z�B
z�B
{B
{JB
{�B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}VB
}"B
}"B
}�B
}�B
}�B
~]B
~]B
~]B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
cB
�B
�B
�B
�4B
�iB
�iB
��B
��B
��B
��B
��B
��B
�oB
��B
��B
�uB
�uB
�uB
�B
�B
�GB
�GB
�B
�GB
�{B
�B
�MB
��B
�MB
�MB
�MB
�MB
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
�+B
�+B
�_B
��B
�_B
��B
��B
��B
�1B
�1B
��B
�7B
�lB
��B
��B
��B
��B
�	B
�	B
�	B
�=B
�=B
��B
��B
��B
��B
��B
��B
��B
��B
�DB
�DB
�DB
�xB
��B
��B
��B
�B
�JB
�JB
�JB
�JB
��B
��B
��B
�PB
��B
��B
��B	SB	SB	PB	xB	JB	
�B	xB	B	DB	JB	B	~B	�B	~B	PB	�B	B	VB	�B	B	�B		�B	DB	PB	�B	DB	JB	B	DB	�B	xB	�B	�B	B	�B	B	PB	�B		�B	�B	�B	�B	(B	�B	�B	�B	$B	B	�B	�B	�B	�B	�B	SB	OB	!�B	)*B	)*B	2�B	.�B	/�B	3�B	2-B	5B	:*B	;0B	;�B	;0B	<�B	A�B	CaB	G�B	HB	H�B	K^B	K�B	QB	O�B	Q�B	XB	T�B	Y�B	X�B	\�B	\]B	[#B	Y�B	Z�B	Z�B	ZB	Z�B	W�B	Y�B	W?B	V�B	V9B	T�B	S�B	T�B	T�B	U2B	UgB	V�B	VB	V�B	WsB	XEB	X�B	W�B	W�B	\�B	Y�B	Z�B	Y�B	Z�B	^�B	_B	_pB	`�B	bB	c�B	jB	l�B	jB	jKB	m�B	n/B	k�B	n�B	r�B	z�B	|PB	~]B	��B	��B	��B	��B	�JB	�(B	��B	�B	��B	��B	�$B	�B	��B	��B	��B	��B	�OB	�\B	�hB	�-B	��B	�4B	��B	�LB	��B	�B	��B	�VB	��B	��B	��B	��B	��B	��B	�kB	��B	��B	�_B	��B	�OB	��B	�B	�eB	��B	��B	�4B	��B	�_B	��B	��B	��B	�B	�hB	�B	��B	�B	�-B	��B	��B	�RB	��B	��B	��B	��B	�FB	��B	��B	��B	��B	��B	��B	�bB	�bB	�\B	��B	��B	�B	�$B	�$B	�B	�zB	��B	�B	�:B	��B	��B	��B	�B	�~B	�CB	�kB	��B	�=B	�CB	�VB	�nB	��B	�LB	�RB	�XB	�$B	�_B	�kB	��B	��B	�B	��B	�$B	�B	ϫB	��B	��B	ںB	ߤB	�vB	�B	�B	�BB	�HB	�B	�8B	�]B	�;B	�xB
B
xB
�B
�B
#�B
6FB
=B
HB
IB
YB
^5B
b�B
jB
w�B
��B
��B
��B
��B
�EB
��B
�
B
��B
��B�B=BxB �B'B*�B1'B2�B6B8�B8RB:�B9�BK�BPHBUgBV�B[WB[�BXBT,BUgB`�BbBjBr|BzBrB��B�SB��B��B�B�=B�VB��B��B�$B�$B��B��B��B��B��B��B��B�@B�4B��B��B�'B�\B��B�bB�-B�\B��B�@B�nB�nB�:B�hB�hB��B�4B�:B��B��B��B�:B��B�bB��B�4B�FB��B�bB��B�B��B�=B��B��B�-B�B��B�CB�IB�zB�)B�HB�yB��B�WB�QB�NB�
B�fB�>B��B��B�DB�B�JB��B�B�2B��B�%B�>B��B��B9XB�B�B�B+B�BB�B�BVB�BIB�B+B�B�B�B�B/B0�B0�B/OB0�B2aB2-B/�B,�B4�B;dBE�BD3BCaB<6B=�B=�B>�BDgBD3BA�BB'BB'BFtBC-BGzBGBHBF�BM�BK�BL�BT�BMBHBD�BAUBA B@OBFBK�BJXBK�BIRBL�BN<BK�BEmBN�BD3BA�BEB-�B1'B6zB)�B,�B1�B5�B(�B�B�B�B�B�B�BBB�BBxB�B�B�B 4B�]B�B�B�B�B��B�B�^B��B��B��B�B�B�XB�dB��B��B��B��B�B��B��B��B�$B�XB��B�B��B��B�-B�bB��B�B�~B�B�$B�B��B��B��B��B�B��B�_B��BzDBv�Br|BpBm�Bf�Bc B^B`vB\]BW�BFtBD�BC�BB[B;dB=�B8�B2�BA�B,B�B	�B
�B
�rB
�2B
��B
��B
��B
�B
�B
�|B
�jB
�/B
��B
҉B
�B
ΥB
�B
�B
�FB
�}B
��B
��B
��B
�OB
��B
�B
��B
��B
�%B
��B
s�B
z�B
��B
��B
��B
��B
�B
�@B
��B
�nB
�XB
��B
�B
��B
��B
��B
�	B
��B
�B
�4B
~(B
u�B
tB
t�B
kB
f�B
iyB
f�B
cTB
c�B
d�B
cTB
bNB
bB
_B
[�B
YKB
TaB
R B
bNB
\)B
YKB
[�B
PHB
I�B
EmB
I�B
C�B
C�B
>B
@OB
9�B
4�B
1�B
9�B
4B
6FB
0�B
.IB
!B
VB
OB
%FB
�B
�B
�B
�B
�B
�B
�B
@B
�B

�B
bB
�B
B
�B
�B
	lB
B
uB
 �B
MB	�.B
�B	�	B
GB
 4B	��B	��B	��B	�KB	�B	�/B	�B	��B	�2B	�/B	ӏB	бB	ϫB	��B	ʌB	�B	�B	�#B	�HB	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021031507212520210315072125IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021032507004220210325070042QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021032507004220210325070042QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014520210427140145IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162933              CF      PSAL                            ?�  G�O�D�0�G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163444    CF                  PSAL            G�O�>�G�O�CG�G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353520230501213535IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                