CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-05T00:45:01Z creation; 2022-09-06T18:25:47Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20211205004501  20220907192127  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               f   fAA  AOAO7825_008765_102                 7825_008765_102                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٧��$ @٧��$ 11  @٧����@٧����@5.�5��@5.�5���e6���1�e6���111  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�z�@   @@  @��\@��\@�  @�  A ��A\)A�RA+�A@  A^{A\)A�Q�A�Q�A�  A��A�  A�Q�A�  B Q�BQ�B�
B�
B�
B(Q�B0  B7�B?�
BH  BP  BXQ�B`z�BhQ�Bp  Bw�
B�
B�  B�  B�{B�{B�  B�  B�  B��B��
B�{B�(�B�(�B�  B��B��
B�  B�  B�  B�{B��B�  B�{B�{B�  B�{B�=qB�Q�B�Q�B�  B��B��
B��C
=C
=C  C  C
  C
=C  C  C  C
=C
=C  C  C
=C  C   C"  C$  C%��C'��C)��C,  C.
=C0  C2  C4
=C6  C7��C:  C;��C=��C@  CA��CC�CE��CH  CJ
=CL
=CM��CO��CQ��CS�CU��CX  CY��C[��C^  C_��Ca�Cd  Cf
=Ch
=Cj  Cl
=Cn
=Cp
=Cr  Cs��Cu��Cw��Cz
=C|  C}��C��C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C��C���C���C���C���C���C�  C�
=C�
=C�C�  C���C���C�  C�C���C�  C�C�  C�  C�C���C���C���C�  C�C�C�  C�C�C�C�C�  C�C�C�C�
=C�  C�C�
=C�
=C�C�  C�  C�  C�C���C���C�  C�  C�  C�C�  C���C�  C�
=C�
=C�  C�  C�
=C�  C���C���C�  C�C�  C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C�C�C�C�C�
=C�C���C���C�C�  C���C���C���C�  C���C�C�C�  C���C�  C�C���C���C�  C���C���C���C���C��C�  C�C�C���C���C���D �D ��DD�DD�D  D� D�D� D  D� D  D� D�qD}qD�D��D	�D	�D	�qD
� D�D}qD  D� D�D��D  D}qD�qDz�D  D�D�D� D�qDz�D��D}qD  D��D�qD� D�D��D�D��D  D� D�D��D��Dz�D  D��D��D}qD�qD}qD�D�DD� D   D � D!�D!�D"  D"� D#  D#}qD#�qD$}qD%  D%�D&�D&��D&�qD'� D(  D(}qD(�qD)� D*  D*��D+�D+��D+�qD,z�D-  D-� D.  D.�D/�D/}qD0  D0� D1  D1��D2�D2� D3  D3��D4�D4� D4�qD5� D5�qD6}qD7  D7� D8�D8}qD8�qD9� D:  D:}qD;�D;}qD;�qD<� D=  D=��D>  D>}qD?�D?��D?�qD@� DA  DA� DA�qDB}qDB�qDC}qDC�qDD}qDD�qDEz�DE��DF� DF�qDG� DH  DH��DI  DI� DJ�DJ}qDJ�qDK��DL�DL��DMDM� DN  DN� DO  DO� DO�qDP}qDQ�DQ��DQ�qDR� DSDS�DTDT� DU  DU��DV�DV� DW  DW��DXDX��DY�DY� DZ�DZ� D[  D[� D[�qD\z�D\�qD]� D^�D^}qD^�qD_��D_�qD`}qDa�Da� Db  Db}qDc�Dc� Dc��Ddz�Dd�RDe}qDf  Df}qDg  Dg�DhDh�Di  Di� Dj�Dj}qDj�qDk}qDl  Dl��DmDm��Dn�Dn� Do  Do� Dp�Dp� Dp�qDq� Dq�qDr}qDs  Ds}qDt  Dt��Du  Du� Dv  Dv� Dv�qDw}qDw�qDxz�Dy  Dy��Dz�Dz}qDz�qD{}qD{�qD|�D}  D}xRD}��D~� D�D� D�  D�@ D�~�D���D�  D�AHD���D��HD�  D�>�D��HD�D�HD�@ D�~�D���D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�}qD��qD��qD�>�D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�B�D�� D���D�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D�}qD���D���D�@ D��HD��HD��D�@ D�~�D�� D�  D�>�D�� D�D�  D�>�D�}qD���D�  D�>�D�~�D�� D���D�@ D��HD�� D�HD�@ D�� D�D��D�B�D�� D��qD���D�>�D��HD��HD���D�>�D�}qD��qD��qD�=qD�~�D�� D�HD�AHD���D�D��D�B�D���D�D�  D�>�D��HD�� D���D�@ D��HD�D�HD�@ D�� D��HD��D�B�D�� D���D���D�@ D�� D�� D���D�>�D�� D��qD���D�@ D�}qD��)D��qD�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�}qD���D�HD�AHD�� D�� D���D�@ D��HD�� D���D�@ D�� D���D���D�@ D�}qD��qD�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�>�D�|)D��qD�  D�AHD�� D�� D��)D�>�D��HD��HD�  D�@ D��HD��HD��D�@ D�}qD��qD���D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D��HD���D�  D�@ D�}qD���D��qD�@ D�� D�� D�  D�@ D��HD�D�HD�B�D��HD���D��qD�AHD��HD��qD���D�AHD�� D��qD���D�@ D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�HD�B�D���D���D���D�>�D�� D���D���D�@ D��HD�� D���D�@ D D�� D���D�=qDÀ D�� D�  D�@ DĀ D�� D��D�@ Dŀ D�� D�HD�B�Dƀ Dƾ�D�  D�AHDǀ D�� D�  D�@ DȁHD�� D��qD�@ DɁHD�� D�  D�B�DʁHD�� D��qD�=qDˀ D��HD�  D�@ D́HD�� D�HD�B�D́HD��HD�HD�AHD΀ DνqD���D�@ Dπ DϾ�D�HD�B�DЂ�D�� D���D�@ Dр DѾ�D�  D�B�D҂�D�D�  D�@ Dӂ�DӾ�D���D�@ DԀ D�� D���D�>�DՁHD�� D�  D�B�Dփ�D��HD���D�AHDׁHD׾�D�HD�C�D؁HDؾ�D�HD�C�DفHD�� D�  D�@ Dڀ D�� D�  D�AHDہHD�� D�  D�@ D܁HD�� D�  D�@ D݀ Dݾ�D�  D�>�D�}qD޾�D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�=qD�}qD�� D�HD�B�D�HD�� D�HD�AHD� D�� D�HD�AHD� D�� D�  D�@ D� D徸D�  D�AHD�HD澸D�  D�@ D� D�� D�  D�AHD�HD辸D���D�>�D�~�D�� D���D�>�D� D꾸D���D�AHD낏D�D�HD�B�D삏D�� D�  D�B�D�HD�D�HD�>�D�~�DD�  D�B�D�HD��HD�  D�>�D�~�D�� D���D�>�D� D�� D��D�@ D� D��HD�  D�@ D� D�� D�  D�@ D�HD�D�  D�>�D��HD��HD�HD�>�D�}qD��qD���D�>�D�~�D�� D���D�>�D�� D��HD��D�B�D���D��HD�HD�+�?W
=?aG�?k�?�z�?��R?�{?�p�?��?�G�?�@
=q@
=@!G�@.{@=p�@E�@Y��@c�
@xQ�@�  @���@�\)@���@��R@�=q@�{@�Q�@�(�@Ǯ@���@�@��H@�ff@�{@��HA   AA��A\)A�\A��A�HA!�A%�A+�A.{A5�A8Q�A@  AC�
AJ=qAMp�AS�
AVffA\(�A`  AfffAi��AqG�Atz�Az�HA~{A�=qA�(�A��A���A���A�
=A���A��
A��RA���A�(�A��RA�=qA�z�A��A��A�p�A�
=A��A�(�A�\)A�G�A���A�\)A��HA��Aȣ�A��HAθRA���A���A�\)A�33A�p�A�G�A�(�A�  A�\A�{A�Q�A�(�A��RA�=qA�p�B z�BB�B�B�\Bz�B	��B�B��B�RB(�B=qB�BG�B�\Bz�B��B�B��B�\B�B!��B"�RB$��B%�B'�
B(��B*�RB,  B-p�B/
=B0(�B1�B3
=B4��B6{B7�
B9�B:�HB<(�B=��B?33B@z�BB=qBC\)BEG�BFffBH(�BI�BK
=BLQ�BN{BO33BP��BR=qBS\)BUG�BV=qBX  BY�B[
=B\  B]�B^�HB`��Ba�Bc\)Bd��Bf{Bg�
Bh��Bj�\Bk�Bmp�Bn�\Bp  Bqp�Br�HBt(�Bup�Bw
=Bx  By��Bz�\B|Q�B}�B~�HB�B��RB��B�  B�z�B�G�B��
B�z�B�33B��
B���B��B�  B�ffB�G�B�B��\B�33B��B��\B��B��B�ffB�\)B�B���B��B��B�z�B�33B��
B�z�B�G�B��B��\B��B��
B��\B�
=B��B�ffB�G�B�B��\B�33B�B��\B�
=B�B�ffB���B�B�(�B�
=B�p�B�Q�B��RB�p�B�{B���B�p�B�B���B�
=B��
B�ffB�
=B��B�=qB�
=B��B�Q�B���B���B�  B���B�G�B�  B��RB��B�  B�ffB��B�B�=qB�
=B��B�Q�B��RB��B�{B��RB�p�B��B���B�G�B�{B���B�\)B�  B��\B�p�B��
B¸RB�\)B��Bģ�B�33B�  B�z�B�p�B��BȸRB�\)B�  B���B�G�B�=qB̸RBͅB�=qB���BϮB�=qB���BѮB�=qB��Bә�B�z�B�
=BծB�ffB�
=B��
B�Q�B�33B��
Bڏ\B�p�B��B��HB�p�B�(�B�
=B߅B�z�B�
=B��B��B�G�B�=qB�RB�B�Q�B��B�  B�z�B�p�B�  B�RB�p�B��B���B�33B��B�Q�B���B�p�BB�=qB�ffB��HB�33B�p�B��B�  B�\B��B��B�G�B�B�{B�=qB��RB���B�G�B��B��
B�Q�B�z�B��HB�33B�p�B��B�{B��\B���B�
=B��B��B�(�B�ffB���B�G�B�\)B�B�=qB�ffB�
=B��B���B�  B�=qB���B���B�p�B��
C   C Q�C \)C ��C ��C �C33CG�C�\C��C�
C{C33CffC��C�C��C{C=qCz�C�\CC  C
=CQ�Cz�C��C��C  C{CQ�Cp�C��C�
C�C(�CQ�Cp�C�CC  C33C=qC�\C��CC
=C�CQ�C�\C��C�HC	
=C	�C	p�C	�C	�C	�C
  C
Q�C
ffC
��C
�
C
��C33CffCz�C��C�
C{CG�C\)C��CC�C(�C=qCz�C�CC
=C(�CQ�C��C�C�HC�C33Cz�C��CC
=C33CG�C��C�RC�
C�CG�CffC�C��C  C=qCQ�C��CC�C(�CG�Cz�C�RC�
C
=CG�CffC��C��C  C=qCQ�C�\C��C�C33CffC�C��C��C�CffC�CC  C{CffC�CC  C{CG�C�\C��C�HC�C=qCz�C�RC�
C�C=qCz�C�RC�
C�CQ�Cp�C�RC�C
=CQ�C�C��C��C(�CG�C�\CC�C=qCffC�\C�
C 
=C 33C �C �C �HC!(�C!Q�C!�\C!��C!�C"(�C"z�C"��C"�
C#�C#=qC#�C#�RC#�HC$33C$\)C$�\C$�
C%
=C%33C%�C%�C%�
C&�C&\)C&z�C&C'
=C'(�C'\)C'�C'��C(
=C(Q�C(z�C(�RC)  C)�C)\)C)��C)�
C)��C*Q�C*�C*�C+  C+=qC+ffC+��C+�C,{C,Q�C,��C,��C-  C-Q�C-�C-�C.  C.=qC.ffC.�RC.��C/�C/p�C/��C/��C0�C0ffC0�\C0��C1{C1Q�C1z�C1C2{C2=qC2p�C2C2��C3(�C3p�C3�RC3�HC4{C4p�C4��C4��C5(�C5\)C5�C5�
C6�C6G�C6z�C6��C7
=C733C7p�C7C7�C8�C8p�C8�C8�
C9(�C9p�C9��C9�
C:(�C:Q�C:�\C:�HC;
=C;G�C;��C;C<  C<\)C<�C<�RC=  C=G�C=p�C=�RC>
=C>=qC>ffC>C>��C?�C?p�C?C?�HC@33C@p�C@��C@�HCA33CAffCA��CA��CB�CBQ�CB��CB�HCC
=CCQ�CC��CCCD  CDQ�CDp�CD�CE  CE33CEffCE��CE�CF{CFQ�CF��CF��CG  CG\)CG��CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                            ?�z�@   @@  @��\@��\@�  @�  A ��A\)A�RA+�A@  A^{A\)A�Q�A�Q�A�  A��A�  A�Q�A�  B Q�BQ�B�
B�
B�
B(Q�B0  B7�B?�
BH  BP  BXQ�B`z�BhQ�Bp  Bw�
B�
B�  B�  B�{B�{B�  B�  B�  B��B��
B�{B�(�B�(�B�  B��B��
B�  B�  B�  B�{B��B�  B�{B�{B�  B�{B�=qB�Q�B�Q�B�  B��B��
B��C
=C
=C  C  C
  C
=C  C  C  C
=C
=C  C  C
=C  C   C"  C$  C%��C'��C)��C,  C.
=C0  C2  C4
=C6  C7��C:  C;��C=��C@  CA��CC�CE��CH  CJ
=CL
=CM��CO��CQ��CS�CU��CX  CY��C[��C^  C_��Ca�Cd  Cf
=Ch
=Cj  Cl
=Cn
=Cp
=Cr  Cs��Cu��Cw��Cz
=C|  C}��C��C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C��C���C���C���C���C���C�  C�
=C�
=C�C�  C���C���C�  C�C���C�  C�C�  C�  C�C���C���C���C�  C�C�C�  C�C�C�C�C�  C�C�C�C�
=C�  C�C�
=C�
=C�C�  C�  C�  C�C���C���C�  C�  C�  C�C�  C���C�  C�
=C�
=C�  C�  C�
=C�  C���C���C�  C�C�  C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C�  C�C�C�C�C�
=C�C���C���C�C�  C���C���C���C�  C���C�C�C�  C���C�  C�C���C���C�  C���C���C���C���C��C�  C�C�C���C���C���D �D ��DD�DD�D  D� D�D� D  D� D  D� D�qD}qD�D��D	�D	�D	�qD
� D�D}qD  D� D�D��D  D}qD�qDz�D  D�D�D� D�qDz�D��D}qD  D��D�qD� D�D��D�D��D  D� D�D��D��Dz�D  D��D��D}qD�qD}qD�D�DD� D   D � D!�D!�D"  D"� D#  D#}qD#�qD$}qD%  D%�D&�D&��D&�qD'� D(  D(}qD(�qD)� D*  D*��D+�D+��D+�qD,z�D-  D-� D.  D.�D/�D/}qD0  D0� D1  D1��D2�D2� D3  D3��D4�D4� D4�qD5� D5�qD6}qD7  D7� D8�D8}qD8�qD9� D:  D:}qD;�D;}qD;�qD<� D=  D=��D>  D>}qD?�D?��D?�qD@� DA  DA� DA�qDB}qDB�qDC}qDC�qDD}qDD�qDEz�DE��DF� DF�qDG� DH  DH��DI  DI� DJ�DJ}qDJ�qDK��DL�DL��DMDM� DN  DN� DO  DO� DO�qDP}qDQ�DQ��DQ�qDR� DSDS�DTDT� DU  DU��DV�DV� DW  DW��DXDX��DY�DY� DZ�DZ� D[  D[� D[�qD\z�D\�qD]� D^�D^}qD^�qD_��D_�qD`}qDa�Da� Db  Db}qDc�Dc� Dc��Ddz�Dd�RDe}qDf  Df}qDg  Dg�DhDh�Di  Di� Dj�Dj}qDj�qDk}qDl  Dl��DmDm��Dn�Dn� Do  Do� Dp�Dp� Dp�qDq� Dq�qDr}qDs  Ds}qDt  Dt��Du  Du� Dv  Dv� Dv�qDw}qDw�qDxz�Dy  Dy��Dz�Dz}qDz�qD{}qD{�qD|�D}  D}xRD}��D~� D�D� D�  D�@ D�~�D���D�  D�AHD���D��HD�  D�>�D��HD�D�HD�@ D�~�D���D�  D�AHD��HD�� D�HD�@ D�~�D�� D�HD�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�}qD��qD��qD�>�D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD�� D�HD�B�D�� D���D�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�@ D�}qD���D���D�@ D��HD��HD��D�@ D�~�D�� D�  D�>�D�� D�D�  D�>�D�}qD���D�  D�>�D�~�D�� D���D�@ D��HD�� D�HD�@ D�� D�D��D�B�D�� D��qD���D�>�D��HD��HD���D�>�D�}qD��qD��qD�=qD�~�D�� D�HD�AHD���D�D��D�B�D���D�D�  D�>�D��HD�� D���D�@ D��HD�D�HD�@ D�� D��HD��D�B�D�� D���D���D�@ D�� D�� D���D�>�D�� D��qD���D�@ D�}qD��)D��qD�>�D�� D��HD�HD�@ D��HD�D�HD�@ D�}qD���D�HD�AHD�� D�� D���D�@ D��HD�� D���D�@ D�� D���D���D�@ D�}qD��qD�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�>�D�|)D��qD�  D�AHD�� D�� D��)D�>�D��HD��HD�  D�@ D��HD��HD��D�@ D�}qD��qD���D�>�D�~�D���D��qD�>�D�~�D���D���D�@ D��HD���D�  D�@ D�}qD���D��qD�@ D�� D�� D�  D�@ D��HD�D�HD�B�D��HD���D��qD�AHD��HD��qD���D�AHD�� D��qD���D�@ D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D��HD��HD�HD�B�D���D���D���D�>�D�� D���D���D�@ D��HD�� D���D�@ D D�� D���D�=qDÀ D�� D�  D�@ DĀ D�� D��D�@ Dŀ D�� D�HD�B�Dƀ Dƾ�D�  D�AHDǀ D�� D�  D�@ DȁHD�� D��qD�@ DɁHD�� D�  D�B�DʁHD�� D��qD�=qDˀ D��HD�  D�@ D́HD�� D�HD�B�D́HD��HD�HD�AHD΀ DνqD���D�@ Dπ DϾ�D�HD�B�DЂ�D�� D���D�@ Dр DѾ�D�  D�B�D҂�D�D�  D�@ Dӂ�DӾ�D���D�@ DԀ D�� D���D�>�DՁHD�� D�  D�B�Dփ�D��HD���D�AHDׁHD׾�D�HD�C�D؁HDؾ�D�HD�C�DفHD�� D�  D�@ Dڀ D�� D�  D�AHDہHD�� D�  D�@ D܁HD�� D�  D�@ D݀ Dݾ�D�  D�>�D�}qD޾�D���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�=qD�}qD�� D�HD�B�D�HD�� D�HD�AHD� D�� D�HD�AHD� D�� D�  D�@ D� D徸D�  D�AHD�HD澸D�  D�@ D� D�� D�  D�AHD�HD辸D���D�>�D�~�D�� D���D�>�D� D꾸D���D�AHD낏D�D�HD�B�D삏D�� D�  D�B�D�HD�D�HD�>�D�~�DD�  D�B�D�HD��HD�  D�>�D�~�D�� D���D�>�D� D�� D��D�@ D� D��HD�  D�@ D� D�� D�  D�@ D�HD�D�  D�>�D��HD��HD�HD�>�D�}qD��qD���D�>�D�~�D�� D���D�>�D�� D��HD��D�B�D���D��HD�HG�O�?W
=?aG�?k�?�z�?��R?�{?�p�?��?�G�?�@
=q@
=@!G�@.{@=p�@E�@Y��@c�
@xQ�@�  @���@�\)@���@��R@�=q@�{@�Q�@�(�@Ǯ@���@�@��H@�ff@�{@��HA   AA��A\)A�\A��A�HA!�A%�A+�A.{A5�A8Q�A@  AC�
AJ=qAMp�AS�
AVffA\(�A`  AfffAi��AqG�Atz�Az�HA~{A�=qA�(�A��A���A���A�
=A���A��
A��RA���A�(�A��RA�=qA�z�A��A��A�p�A�
=A��A�(�A�\)A�G�A���A�\)A��HA��Aȣ�A��HAθRA���A���A�\)A�33A�p�A�G�A�(�A�  A�\A�{A�Q�A�(�A��RA�=qA�p�B z�BB�B�B�\Bz�B	��B�B��B�RB(�B=qB�BG�B�\Bz�B��B�B��B�\B�B!��B"�RB$��B%�B'�
B(��B*�RB,  B-p�B/
=B0(�B1�B3
=B4��B6{B7�
B9�B:�HB<(�B=��B?33B@z�BB=qBC\)BEG�BFffBH(�BI�BK
=BLQ�BN{BO33BP��BR=qBS\)BUG�BV=qBX  BY�B[
=B\  B]�B^�HB`��Ba�Bc\)Bd��Bf{Bg�
Bh��Bj�\Bk�Bmp�Bn�\Bp  Bqp�Br�HBt(�Bup�Bw
=Bx  By��Bz�\B|Q�B}�B~�HB�B��RB��B�  B�z�B�G�B��
B�z�B�33B��
B���B��B�  B�ffB�G�B�B��\B�33B��B��\B��B��B�ffB�\)B�B���B��B��B�z�B�33B��
B�z�B�G�B��B��\B��B��
B��\B�
=B��B�ffB�G�B�B��\B�33B�B��\B�
=B�B�ffB���B�B�(�B�
=B�p�B�Q�B��RB�p�B�{B���B�p�B�B���B�
=B��
B�ffB�
=B��B�=qB�
=B��B�Q�B���B���B�  B���B�G�B�  B��RB��B�  B�ffB��B�B�=qB�
=B��B�Q�B��RB��B�{B��RB�p�B��B���B�G�B�{B���B�\)B�  B��\B�p�B��
B¸RB�\)B��Bģ�B�33B�  B�z�B�p�B��BȸRB�\)B�  B���B�G�B�=qB̸RBͅB�=qB���BϮB�=qB���BѮB�=qB��Bә�B�z�B�
=BծB�ffB�
=B��
B�Q�B�33B��
Bڏ\B�p�B��B��HB�p�B�(�B�
=B߅B�z�B�
=B��B��B�G�B�=qB�RB�B�Q�B��B�  B�z�B�p�B�  B�RB�p�B��B���B�33B��B�Q�B���B�p�BB�=qB�ffB��HB�33B�p�B��B�  B�\B��B��B�G�B�B�{B�=qB��RB���B�G�B��B��
B�Q�B�z�B��HB�33B�p�B��B�{B��\B���B�
=B��B��B�(�B�ffB���B�G�B�\)B�B�=qB�ffB�
=B��B���B�  B�=qB���B���B�p�B��
C   C Q�C \)C ��C ��C �C33CG�C�\C��C�
C{C33CffC��C�C��C{C=qCz�C�\CC  C
=CQ�Cz�C��C��C  C{CQ�Cp�C��C�
C�C(�CQ�Cp�C�CC  C33C=qC�\C��CC
=C�CQ�C�\C��C�HC	
=C	�C	p�C	�C	�C	�C
  C
Q�C
ffC
��C
�
C
��C33CffCz�C��C�
C{CG�C\)C��CC�C(�C=qCz�C�CC
=C(�CQ�C��C�C�HC�C33Cz�C��CC
=C33CG�C��C�RC�
C�CG�CffC�C��C  C=qCQ�C��CC�C(�CG�Cz�C�RC�
C
=CG�CffC��C��C  C=qCQ�C�\C��C�C33CffC�C��C��C�CffC�CC  C{CffC�CC  C{CG�C�\C��C�HC�C=qCz�C�RC�
C�C=qCz�C�RC�
C�CQ�Cp�C�RC�C
=CQ�C�C��C��C(�CG�C�\CC�C=qCffC�\C�
C 
=C 33C �C �C �HC!(�C!Q�C!�\C!��C!�C"(�C"z�C"��C"�
C#�C#=qC#�C#�RC#�HC$33C$\)C$�\C$�
C%
=C%33C%�C%�C%�
C&�C&\)C&z�C&C'
=C'(�C'\)C'�C'��C(
=C(Q�C(z�C(�RC)  C)�C)\)C)��C)�
C)��C*Q�C*�C*�C+  C+=qC+ffC+��C+�C,{C,Q�C,��C,��C-  C-Q�C-�C-�C.  C.=qC.ffC.�RC.��C/�C/p�C/��C/��C0�C0ffC0�\C0��C1{C1Q�C1z�C1C2{C2=qC2p�C2C2��C3(�C3p�C3�RC3�HC4{C4p�C4��C4��C5(�C5\)C5�C5�
C6�C6G�C6z�C6��C7
=C733C7p�C7C7�C8�C8p�C8�C8�
C9(�C9p�C9��C9�
C:(�C:Q�C:�\C:�HC;
=C;G�C;��C;C<  C<\)C<�C<�RC=  C=G�C=p�C=�RC>
=C>=qC>ffC>C>��C?�C?p�C?C?�HC@33C@p�C@��C@�HCA33CAffCA��CA��CB�CBQ�CB��CB�HCC
=CCQ�CC��CCCD  CDQ�CDp�CD�CE  CE33CEffCE��CE�CF{CFQ�CF��CF��CG  CG\)CG��CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�JG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA�bA��A� �A��A��A�$�A�-A�&�A�33A�7LA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�9XA�9XA�7LA�7LA�5?A�9XA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�C�A�C�A�C�A�E�A�C�A�G�A�C�A�C�A�G�A�G�A�G�A�E�A�C�A�(�A�oA�AхA�"�A�7LA΅A��Aʰ!A�z�Aơ�A���A�M�A�A���A���A��A�;dA��HA�S�A�{A�1A��A��FA�ƨA���A�~�A��A�(�A��#A��wA���A�1'A�XA���A�v�A��A�K�A�7LA�l�A�z�A��
A�E�A���A��A��;A�+A�  A���A�M�A�oA�p�A��yA��A��A�M�A��A�$�A��A���A�r�A�I�A�+A��A���A��A�^5A��A+A~^5A}�
A{��AwƨAu�Ar{Ap=qAo�-AoG�An �Am+AkS�Ai�Ag�^Ae�;Ac�;Ab  A`E�A]�7AZ��AX��AW33AV�AT��AS�-AQ�
APn�AOVAMx�AKƨAJ�DAI
=AGx�AF1AC�mAB��AAƨA@(�A>��A=��A=ƨA=/A:�!A8��A7�^A5hsA3l�A3�A2�yA2��A1��A1;dA0��A/t�A.$�A-��A-XA,ffA*��A)�
A(��A'�mA'K�A&ffA%C�A#��A!�-A�A;dA�A�A��A5?A�FA��A��AC�A��A��A�-AXA��A�A~�A�A��A�FA��A�AG�Ar�AdZA�DA�A;dA�!A�A�DA�wA7LAZA�7A�jA1'AXA
^5A	��A	+A��A�AJAffA1A�A ff@��;@�n�@���@��@��u@���@�j@�@�1'@�@�ƨ@���@�P@�1@�{@��@�"�@�5?@�-@�7L@�(�@�S�@�!@�G�@�
=@���@݉7@ۮ@�7L@��;@���@��@�X@��@Լj@�(�@�  @���@�|�@���@�M�@ѡ�@�?}@��@ЋD@��@�K�@Ώ\@Ͳ-@��@���@̣�@�r�@�(�@��m@�ƨ@˝�@��@�M�@ɲ-@�bN@Ǖ�@��@�E�@�hs@�X@�7L@��`@ă@�(�@öF@Õ�@�|�@�t�@��@��#@��@���@�1@�t�@�~�@�J@���@�`B@�7L@��D@�@��+@�n�@�^5@�`B@�Z@�\)@��@��+@��#@��@�z�@���@�|�@��@�ff@��#@���@�Ĝ@��u@�I�@�1@��;@���@���@�33@��R@�J@��^@���@�C�@��P@���@� �@��@�(�@�\)@�S�@�S�@�dZ@�|�@�|�@�|�@�|�@��R@��7@�7L@�7L@� �@�K�@��+@���@���@�z�@�I�@�I�@�Q�@�I�@�A�@�1@�t�@���@�$�@�J@�J@��@��h@�O�@��@��@�%@���@���@� �@�t�@�t�@�\)@�ȴ@�n�@���@��h@�p�@�&�@��/@��@�A�@� �@���@��@�b@� �@�A�@�r�@�z�@��9@���@��/@�z�@���@��P@���@��@���@���@��T@��T@���@���@��@�O�@��@���@��`@��@���@���@�l�@���@���@��h@�7L@���@�A�@�  @��
@��w@���@�|�@�33@�
=@���@�^5@��@���@��T@��#@���@���@�p�@�V@��@���@��u@��D@�I�@�ƨ@��@��@��@�C�@��@���@�=q@��@�@��^@�p�@�`B@�`B@�X@�X@�X@�G�@�&�@��j@��D@�z�@�bN@�I�@���@�C�@�o@��R@�v�@�-@�`B@��@�bN@�A�@� �@���@��
@��@�o@�ȴ@���@��\@�~�@��@���@��h@�hs@�G�@�/@�V@��`@��`@���@���@��@�Q�@��w@�dZ@�\)@�;d@���@��H@��R@�M�@��@���@��@��j@�z�@�Q�@��@�  @��@�l�@�
=@��R@���@���@�=q@�{@�@���@�@��h@�X@�V@��j@��D@�z�@�j@�9X@�  @�P@+@~��@~5?@}�-@|�/@|�D@{�@{33@{@z�@y7L@x�9@xQ�@wl�@v$�@u��@u@u�-@u�-@u��@u�h@u`B@t�/@tZ@s�m@s��@r�H@rM�@r=q@rJ@q�#@q��@q�7@qhs@qG�@q%@p�9@p�@pb@o��@ol�@n�@nV@m@m��@m�h@m�@l�@l��@lZ@k�@ko@j~�@j=q@i�^@i7L@i&�@h��@h�9@h�u@hA�@g��@g+@g
=@g
=@f��@f�@fff@f@e�@e�T@e�T@e�T@e�T@e�T@e�T@e��@e��@e`B@eO�@eO�@eO�@e/@eV@eV@d�@d�j@d�D@c��@c@aG�@a�@a%@`��@`r�@_�@^��@^��@^ff@^5?@]�T@]��@]?}@]V@\�@\��@\��@[��@[��@[C�@[@Z��@Z�!@Z~�@Y�@X��@W�@Wl�@W�@V�y@Vȴ@V��@V��@Vv�@VV@V5?@V$�@V{@V@V@U�@U�T@U@U�-@U��@U�@UO�@T��@TZ@T1@S�m@S�F@SC�@S33@R�H@R�\@RJ@Q�7@P��@P�u@PQ�@P1'@O�@O;d@Nȴ@Nv�@NV@M��@M?}@M�@M�@L��@K�m@K�F@K��@K��@K33@J�!@J^5@J=q@I�@I�^@I�7@H�`@H�`@HĜ@HQ�@HA�@H1'@H  @G�@G�P@G|�@Gl�@G+@G
=@Fȴ@Fȴ@F��@FV@E�@E��@E?}@E�@D��@D�j@D9X@C�m@Cƨ@C��@C�@CS�@C"�@B�H@B��@B�@A�#@A�7@A7L@A%@@b@?K�@>�@>ff@=�h@<�/@<Z@<9X@<(�@<�@;��@;C�@;o@:�H@:��@:�\@:=q@:�@9��@8��@8Ĝ@8r�@8  @7��@7K�@7+@6�@6�+@6E�@5�@5��@5��@5�h@5�@5O�@4�j@4j@4Z@4I�@4(�@3��@3C�@3"�@2��@2n�@2-@2J@1��@1�#@1��@1��@1x�@1%@0r�@0 �@/�w@/��@/|�@/\)@/;d@.��@.��@.�+@.ff@.E�@-�@-@-��@-O�@-V@,�j@,��@,j@,9X@,(�@,1@+�m@+��@+C�@*�@*�@)x�@)X@)7L@)&�@)�@)�@(��@(�9@(�@( �@'��@'l�@'K�@'+@&��@&�@&v�@%�@%�-@%p�@%/@%�@$�/@$��@$Z@$9X@$9X@$9X@$1@#ƨ@#t�@#S�@#@"��@"��@"�\@"n�@"=q@"�@!�#@!��@!�@ ��@ A�@�;@��@+@ȴ@�R@v�@5?@$�@@�T@�-@��@��@S�@"�@�@�H@�\@n�@^5@=q@J@J@��@�@7L@A�@ �@b@b@b@ �@b@�;@�;@�;@�;@��@|�@+@ȴ@��@�+@V@�@�/@j@�@�m@�F@dZ@��@=q@�#@��@�7@x�@hs@G�@&�@�@�@�@%@��@�`@�9@��@r�@Q�@l�@ȴ@ff@E�@5?@{@�@��@�-@p�@O�@?}@��@��@�D@z�@j@��@�F@��@��@t�@dZ@33@o@
�@
��@
�!@
�!@
�!@
��@
��@
�\@
~�@
n�@
M�@	�@	��@	�7@	hs@	&�@	%@��@��@Ĝ@Ĝ@Ĝ@�u@A�A�JA�
=A�A�VA�VA��A��A�oA��A�%A�oA�VA�oA�bA��A��A��A��A� �A�"�A� �A��A��A��A��A��A��A��A��A��A�$�A� �A�$�A�+A�/A�+A�(�A�&�A�"�A�+A�-A�5?A�5?A�5?A�7LA�;dA�;dA�;dA�9XA�=qA�9XA�=qA�9XA�=qA�9XA�7LA�7LA�9XA�;dA�9XA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�7LA�=qA�9XA�=qA�7LA�;dA�9XA�=qA�9XA�;dA�9XA�;dA�9XA�=qA�9XA�=qA�;dA�=qA�;dA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�7LA�7LA�5?A�9XA�5?A�;dA�7LA�;dA�5?A�9XA�5?A�;dA�5?A�9XA�5?A�9XA�33A�9XA�5?A�;dA�7LA�=qA�9XA�?}A�;dA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�?}A�=qA�=qA�?}A�;dA�?}A�9XA�?}A�;dA�=qA�=qA�=qA�=qA�=qA�A�A�;dA�?}A�;dA�?}A�;dA�A�A�;dA�A�A�;dA�?}A�?}A�=qA�A�A�;dA�C�A�=qA�C�A�?}A�G�A�A�A�E�A�A�A�E�A�A�A�C�A�G�A�A�A�E�A�A�A�E�A�A�A�I�A�C�A�E�A�G�A�C�A�G�A�C�A�E�A�A�A�E�A�?}A�G�A�C�A�G�A�E�A�G�A�G�A�E�A�G�A�G�A�G�A�E�A�C�A�?}A�C�A�?}A�G�A�A�A�E�A�C�A�A�A�C�A�E�A�G�A�C�A�I�A�C�A�K�A�E�A�K�A�E�A�G�A�G�A�E�A�G�A�E�A�I�A�E�A�I�A�C�A�I�A�G�A�E�A�I�A�C�A�E�A�=qA�E�A�?}A�C�A�G�A�A�A�C�A�;dA�1'A�&�A��A� �A��A��A��A�oA�bA�VA�VA�
=A�VA�%A�1A�A�1A���A��A��mA���Aщ7A�G�A���AЧ�A�5?A�+A��A�1A���A��mAϗ�A�p�A�x�A�JA��#AθRAδ9AΙ�A·+A�v�A�v�A�bNA��A�ȴA�-A���A�?}A��A��
AˁA��#Aʥ�A�VA�oAɏ\A�Aȩ�A�ffA�+A�
=A���A��mAǮA�ZA��AľwA�^5A�+A�VA���A��A�ȴAé�A�z�A�\)A�9XA��A���A��yAPA�5?A��#A��+A��A���A�ZA�G�A�+A��9A��A�
=A���A�v�A�\)A�33A�1'A�(�A�(�A��A�bA�A��HA�ĜA���A���A��FA�VA�G�A���A�|�A�ffA�ZA�^5A�S�A�VA�Q�A�M�A�M�A�C�A�E�A�=qA�A�A�9XA�&�A��A���A��mA��A��A��jA�r�A�XA�A�A�5?A��A�  A��FA���A���A�`BA���A�jA�bNA�33A� �A���A��A��A���A��wA���A�p�A�/A�1A��A��/A���A��7A�G�A��A��A�A�;dA�{A��`A��A�dZA�
=A���A��A�|�A�t�A�n�A�bNA�A�A�=qA�9XA�9XA� �A���A���A��A��`A��TA��#A��#A���A��RA���A��uA��\A��7A�z�A�z�A�t�A�n�A�l�A�^5A�K�A�/A��A�
=A���A��`A��HA���A���A���A��-A�z�A�9XA��A���A���A�ffA�?}A�33A�1A��mA���A�|�A�?}A��;A��PA�`BA�-A��A�1A���A��jA��\A�Q�A���A��hA�oA��A���A��`A���A��-A���A��hA�|�A�r�A�t�A�dZA�E�A� �A���A��A��HA���A���A�ȴA���A�ĜA��-A���A��+A�`BA�K�A�&�A�oA�bA��TA�ƨA���A���A�z�A�x�A�t�A�^5A�^5A�ZA�E�A�-A��A�$�A��A�A���A��TA���A���A��DA�p�A�VA�"�A�JA��mA���A��!A���A�t�A�ZA�9XA�-A�&�A�oA�{A��A��A��A��A��A��A��A���A���A���A�ĜA��jA��FA���A��A�A�A��
A���A�O�A�A��DA�v�A�S�A�K�A�-A��A���A���A���A���A��A���A��uA�~�A�n�A�n�A�VA�oA�ȴA���A���A��jA���A�bNA�M�A�9XA�$�A��A�oA�1A�JA�A��A��TA���A��\A�`BA�n�A�O�A�K�A�M�A�XA�;dA�/A�1'A�1'A�/A�+A���A��A���A���A��`A��TA��TA��`A��A�ȴA��9A��FA��RA��PA�~�A�~�A�z�A�v�A�O�A�JA��TA���A��^A��A�=qA�"�A�VA��#A���A�;dA���A�ĜA��PA�l�A���A�dZA�;dA�A���A��`A��TA�ĜA��hA�`BA�{A���A��A��`A���A���A��wA���A�t�A�jA�`BA�Q�A�I�A�I�A� �A��jA�hsA�O�A�&�A�A���A��yA��#A��A���A���A���A�n�A�S�A�G�A�A�A�5?A�(�A� �A��A�%A��A��HA��RA���A��uA�hsA�O�A�33A�"�A�bA�A��A��;A���A�ĜA��FA��-A��A���A���A���A�z�A�33A��\A�z�A�{A��yA���A���A��jA��wA��^A���A���A�~�A�XA�E�A�7LA�5?A�/A�+A�&�A��A��A��A�oA�bA�JA�  A��/A�ȴA��^A��A���A�v�A�C�A���A���A��!A�p�A�"�A���A��A��`A���A���A��hA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                            A�bA�bA��A� �A��A��A�$�A�-A�&�A�33A�7LA�;dA�9XA�9XA�;dA�9XA�;dA�;dA�9XA�9XA�7LA�7LA�5?A�9XA�;dA�=qA�=qA�=qA�=qA�=qA�?}A�?}A�C�A�C�A�C�A�E�A�C�A�G�A�C�A�C�A�G�A�G�A�G�A�E�A�C�A�(�A�oA�AхA�"�A�7LA΅A��Aʰ!A�z�Aơ�A���A�M�A�A���A���A��A�;dA��HA�S�A�{A�1A��A��FA�ƨA���A�~�A��A�(�A��#A��wA���A�1'A�XA���A�v�A��A�K�A�7LA�l�A�z�A��
A�E�A���A��A��;A�+A�  A���A�M�A�oA�p�A��yA��A��A�M�A��A�$�A��A���A�r�A�I�A�+A��A���A��A�^5A��A+A~^5A}�
A{��AwƨAu�Ar{Ap=qAo�-AoG�An �Am+AkS�Ai�Ag�^Ae�;Ac�;Ab  A`E�A]�7AZ��AX��AW33AV�AT��AS�-AQ�
APn�AOVAMx�AKƨAJ�DAI
=AGx�AF1AC�mAB��AAƨA@(�A>��A=��A=ƨA=/A:�!A8��A7�^A5hsA3l�A3�A2�yA2��A1��A1;dA0��A/t�A.$�A-��A-XA,ffA*��A)�
A(��A'�mA'K�A&ffA%C�A#��A!�-A�A;dA�A�A��A5?A�FA��A��AC�A��A��A�-AXA��A�A~�A�A��A�FA��A�AG�Ar�AdZA�DA�A;dA�!A�A�DA�wA7LAZA�7A�jA1'AXA
^5A	��A	+A��A�AJAffA1A�A ff@��;@�n�@���@��@��u@���@�j@�@�1'@�@�ƨ@���@�P@�1@�{@��@�"�@�5?@�-@�7L@�(�@�S�@�!@�G�@�
=@���@݉7@ۮ@�7L@��;@���@��@�X@��@Լj@�(�@�  @���@�|�@���@�M�@ѡ�@�?}@��@ЋD@��@�K�@Ώ\@Ͳ-@��@���@̣�@�r�@�(�@��m@�ƨ@˝�@��@�M�@ɲ-@�bN@Ǖ�@��@�E�@�hs@�X@�7L@��`@ă@�(�@öF@Õ�@�|�@�t�@��@��#@��@���@�1@�t�@�~�@�J@���@�`B@�7L@��D@�@��+@�n�@�^5@�`B@�Z@�\)@��@��+@��#@��@�z�@���@�|�@��@�ff@��#@���@�Ĝ@��u@�I�@�1@��;@���@���@�33@��R@�J@��^@���@�C�@��P@���@� �@��@�(�@�\)@�S�@�S�@�dZ@�|�@�|�@�|�@�|�@��R@��7@�7L@�7L@� �@�K�@��+@���@���@�z�@�I�@�I�@�Q�@�I�@�A�@�1@�t�@���@�$�@�J@�J@��@��h@�O�@��@��@�%@���@���@� �@�t�@�t�@�\)@�ȴ@�n�@���@��h@�p�@�&�@��/@��@�A�@� �@���@��@�b@� �@�A�@�r�@�z�@��9@���@��/@�z�@���@��P@���@��@���@���@��T@��T@���@���@��@�O�@��@���@��`@��@���@���@�l�@���@���@��h@�7L@���@�A�@�  @��
@��w@���@�|�@�33@�
=@���@�^5@��@���@��T@��#@���@���@�p�@�V@��@���@��u@��D@�I�@�ƨ@��@��@��@�C�@��@���@�=q@��@�@��^@�p�@�`B@�`B@�X@�X@�X@�G�@�&�@��j@��D@�z�@�bN@�I�@���@�C�@�o@��R@�v�@�-@�`B@��@�bN@�A�@� �@���@��
@��@�o@�ȴ@���@��\@�~�@��@���@��h@�hs@�G�@�/@�V@��`@��`@���@���@��@�Q�@��w@�dZ@�\)@�;d@���@��H@��R@�M�@��@���@��@��j@�z�@�Q�@��@�  @��@�l�@�
=@��R@���@���@�=q@�{@�@���@�@��h@�X@�V@��j@��D@�z�@�j@�9X@�  @�P@+@~��@~5?@}�-@|�/@|�D@{�@{33@{@z�@y7L@x�9@xQ�@wl�@v$�@u��@u@u�-@u�-@u��@u�h@u`B@t�/@tZ@s�m@s��@r�H@rM�@r=q@rJ@q�#@q��@q�7@qhs@qG�@q%@p�9@p�@pb@o��@ol�@n�@nV@m@m��@m�h@m�@l�@l��@lZ@k�@ko@j~�@j=q@i�^@i7L@i&�@h��@h�9@h�u@hA�@g��@g+@g
=@g
=@f��@f�@fff@f@e�@e�T@e�T@e�T@e�T@e�T@e�T@e��@e��@e`B@eO�@eO�@eO�@e/@eV@eV@d�@d�j@d�D@c��@c@aG�@a�@a%@`��@`r�@_�@^��@^��@^ff@^5?@]�T@]��@]?}@]V@\�@\��@\��@[��@[��@[C�@[@Z��@Z�!@Z~�@Y�@X��@W�@Wl�@W�@V�y@Vȴ@V��@V��@Vv�@VV@V5?@V$�@V{@V@V@U�@U�T@U@U�-@U��@U�@UO�@T��@TZ@T1@S�m@S�F@SC�@S33@R�H@R�\@RJ@Q�7@P��@P�u@PQ�@P1'@O�@O;d@Nȴ@Nv�@NV@M��@M?}@M�@M�@L��@K�m@K�F@K��@K��@K33@J�!@J^5@J=q@I�@I�^@I�7@H�`@H�`@HĜ@HQ�@HA�@H1'@H  @G�@G�P@G|�@Gl�@G+@G
=@Fȴ@Fȴ@F��@FV@E�@E��@E?}@E�@D��@D�j@D9X@C�m@Cƨ@C��@C�@CS�@C"�@B�H@B��@B�@A�#@A�7@A7L@A%@@b@?K�@>�@>ff@=�h@<�/@<Z@<9X@<(�@<�@;��@;C�@;o@:�H@:��@:�\@:=q@:�@9��@8��@8Ĝ@8r�@8  @7��@7K�@7+@6�@6�+@6E�@5�@5��@5��@5�h@5�@5O�@4�j@4j@4Z@4I�@4(�@3��@3C�@3"�@2��@2n�@2-@2J@1��@1�#@1��@1��@1x�@1%@0r�@0 �@/�w@/��@/|�@/\)@/;d@.��@.��@.�+@.ff@.E�@-�@-@-��@-O�@-V@,�j@,��@,j@,9X@,(�@,1@+�m@+��@+C�@*�@*�@)x�@)X@)7L@)&�@)�@)�@(��@(�9@(�@( �@'��@'l�@'K�@'+@&��@&�@&v�@%�@%�-@%p�@%/@%�@$�/@$��@$Z@$9X@$9X@$9X@$1@#ƨ@#t�@#S�@#@"��@"��@"�\@"n�@"=q@"�@!�#@!��@!�@ ��@ A�@�;@��@+@ȴ@�R@v�@5?@$�@@�T@�-@��@��@S�@"�@�@�H@�\@n�@^5@=q@J@J@��@�@7L@A�@ �@b@b@b@ �@b@�;@�;@�;@�;@��@|�@+@ȴ@��@�+@V@�@�/@j@�@�m@�F@dZ@��@=q@�#@��@�7@x�@hs@G�@&�@�@�@�@%@��@�`@�9@��@r�@Q�@l�@ȴ@ff@E�@5?@{@�@��@�-@p�@O�@?}@��@��@�D@z�@j@��@�F@��@��@t�@dZ@33@o@
�@
��@
�!@
�!@
�!@
��@
��@
�\@
~�@
n�@
M�@	�@	��@	�7@	hs@	&�@	%@��@��@Ĝ@Ĝ@Ĝ@�uG�O�A�JA�
=A�A�VA�VA��A��A�oA��A�%A�oA�VA�oA�bA��A��A��A��A� �A�"�A� �A��A��A��A��A��A��A��A��A��A�$�A� �A�$�A�+A�/A�+A�(�A�&�A�"�A�+A�-A�5?A�5?A�5?A�7LA�;dA�;dA�;dA�9XA�=qA�9XA�=qA�9XA�=qA�9XA�7LA�7LA�9XA�;dA�9XA�7LA�7LA�9XA�9XA�9XA�9XA�;dA�7LA�=qA�9XA�=qA�7LA�;dA�9XA�=qA�9XA�;dA�9XA�;dA�9XA�=qA�9XA�=qA�;dA�=qA�;dA�9XA�7LA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�9XA�7LA�7LA�5?A�9XA�5?A�;dA�7LA�;dA�5?A�9XA�5?A�;dA�5?A�9XA�5?A�9XA�33A�9XA�5?A�;dA�7LA�=qA�9XA�?}A�;dA�=qA�;dA�=qA�=qA�=qA�=qA�?}A�=qA�?}A�=qA�=qA�?}A�;dA�?}A�9XA�?}A�;dA�=qA�=qA�=qA�=qA�=qA�A�A�;dA�?}A�;dA�?}A�;dA�A�A�;dA�A�A�;dA�?}A�?}A�=qA�A�A�;dA�C�A�=qA�C�A�?}A�G�A�A�A�E�A�A�A�E�A�A�A�C�A�G�A�A�A�E�A�A�A�E�A�A�A�I�A�C�A�E�A�G�A�C�A�G�A�C�A�E�A�A�A�E�A�?}A�G�A�C�A�G�A�E�A�G�A�G�A�E�A�G�A�G�A�G�A�E�A�C�A�?}A�C�A�?}A�G�A�A�A�E�A�C�A�A�A�C�A�E�A�G�A�C�A�I�A�C�A�K�A�E�A�K�A�E�A�G�A�G�A�E�A�G�A�E�A�I�A�E�A�I�A�C�A�I�A�G�A�E�A�I�A�C�A�E�A�=qA�E�A�?}A�C�A�G�A�A�A�C�A�;dA�1'A�&�A��A� �A��A��A��A�oA�bA�VA�VA�
=A�VA�%A�1A�A�1A���A��A��mA���Aщ7A�G�A���AЧ�A�5?A�+A��A�1A���A��mAϗ�A�p�A�x�A�JA��#AθRAδ9AΙ�A·+A�v�A�v�A�bNA��A�ȴA�-A���A�?}A��A��
AˁA��#Aʥ�A�VA�oAɏ\A�Aȩ�A�ffA�+A�
=A���A��mAǮA�ZA��AľwA�^5A�+A�VA���A��A�ȴAé�A�z�A�\)A�9XA��A���A��yAPA�5?A��#A��+A��A���A�ZA�G�A�+A��9A��A�
=A���A�v�A�\)A�33A�1'A�(�A�(�A��A�bA�A��HA�ĜA���A���A��FA�VA�G�A���A�|�A�ffA�ZA�^5A�S�A�VA�Q�A�M�A�M�A�C�A�E�A�=qA�A�A�9XA�&�A��A���A��mA��A��A��jA�r�A�XA�A�A�5?A��A�  A��FA���A���A�`BA���A�jA�bNA�33A� �A���A��A��A���A��wA���A�p�A�/A�1A��A��/A���A��7A�G�A��A��A�A�;dA�{A��`A��A�dZA�
=A���A��A�|�A�t�A�n�A�bNA�A�A�=qA�9XA�9XA� �A���A���A��A��`A��TA��#A��#A���A��RA���A��uA��\A��7A�z�A�z�A�t�A�n�A�l�A�^5A�K�A�/A��A�
=A���A��`A��HA���A���A���A��-A�z�A�9XA��A���A���A�ffA�?}A�33A�1A��mA���A�|�A�?}A��;A��PA�`BA�-A��A�1A���A��jA��\A�Q�A���A��hA�oA��A���A��`A���A��-A���A��hA�|�A�r�A�t�A�dZA�E�A� �A���A��A��HA���A���A�ȴA���A�ĜA��-A���A��+A�`BA�K�A�&�A�oA�bA��TA�ƨA���A���A�z�A�x�A�t�A�^5A�^5A�ZA�E�A�-A��A�$�A��A�A���A��TA���A���A��DA�p�A�VA�"�A�JA��mA���A��!A���A�t�A�ZA�9XA�-A�&�A�oA�{A��A��A��A��A��A��A��A���A���A���A�ĜA��jA��FA���A��A�A�A��
A���A�O�A�A��DA�v�A�S�A�K�A�-A��A���A���A���A���A��A���A��uA�~�A�n�A�n�A�VA�oA�ȴA���A���A��jA���A�bNA�M�A�9XA�$�A��A�oA�1A�JA�A��A��TA���A��\A�`BA�n�A�O�A�K�A�M�A�XA�;dA�/A�1'A�1'A�/A�+A���A��A���A���A��`A��TA��TA��`A��A�ȴA��9A��FA��RA��PA�~�A�~�A�z�A�v�A�O�A�JA��TA���A��^A��A�=qA�"�A�VA��#A���A�;dA���A�ĜA��PA�l�A���A�dZA�;dA�A���A��`A��TA�ĜA��hA�`BA�{A���A��A��`A���A���A��wA���A�t�A�jA�`BA�Q�A�I�A�I�A� �A��jA�hsA�O�A�&�A�A���A��yA��#A��A���A���A���A�n�A�S�A�G�A�A�A�5?A�(�A� �A��A�%A��A��HA��RA���A��uA�hsA�O�A�33A�"�A�bA�A��A��;A���A�ĜA��FA��-A��A���A���A���A�z�A�33A��\A�z�A�{A��yA���A���A��jA��wA��^A���A���A�~�A�XA�E�A�7LA�5?A�/A�+A�&�A��A��A��A�oA�bA�JA�  A��/A�ȴA��^A��A���A�v�A�C�A���A���A��!A�p�A�"�A���A��A��`A���A���A��hA��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�3B�3B�hB��B��B��B�hB�-B��B�aB�aB��B��B��B��B��B��B�3B��B��B��B��B�hB�hB��B�hB��B��B��B��B��B��B��B��B�9B�B�nB�nB�9B�B��B�B��B��B�nB��B��BÖB��B��B�jB�?B��B�MB�B�BOB&�B�BCB.}B5?B'�B'�B-B�B
	B�PB��B�B�iB��B��B��B�LB��B��B��B��B�0B��B�SB��B�Bz�BqBm�BiyBb�BS[B>B2-B*eB!B�B B	7B;B�B�QB��B�vB�mB��B��B��B��B~�BbNBS�B<jB*eB&�B#B!�B~B
��B
��B
��B
خB
�B
�B
ŢB
�<B
��B
��B
��B
��B
��B
zB
t�B
Z�B
W
B
G�B
AUB
8�B
2aB
+�B
$�B
1B
B

�B	�]B	��B	�cB	�B	��B	��B	ٴB	��B	�B	��B	�EB	��B	�B	��B	��B	��B	��B	�B	��B	�@B	��B	�VB	�xB	�CB	�SB	�{B	�4B	�4B	��B	�fB	��B	�AB	�iB	.B	|�B	}�B	v`B	{B	u�B	uZB	t�B	v+B	xB	w�B	wfB	w2B	y	B	xlB	yrB	.B	�oB	�AB	�rB	�rB	�JB	��B	��B	�(B	�4B	�hB	�VB	�B	��B	��B	��B	��B	�AB	yrB	y�B	|�B	��B	�B	��B	�B	�xB	��B	�OB	�'B	�!B	�7B	�_B	�_B	��B	�"B	�=B	�MB	zxB	|�B	� B	�B	�oB	|PB	zxB	v+B	sB	r|B	sB	s�B	y�B	o�B	tB	qvB	q�B	p�B	poB	q�B	qAB	r|B	u�B	w2B	rB	qAB	poB	tB	wfB	v`B	x�B	x�B	y�B	{�B	{�B	{B	{�B	{�B	}�B	~�B	��B	�AB	�uB	��B	��B	��B	��B	�xB	��B	�B	�B	�~B	��B	��B	�B	�B	��B	��B	�uB	�1B	�B	�qB	�B	�7B	��B	�B	�7B	��B	�qB	��B	�B	��B	�-B	�B	��B	�wB	�B	�[B	��B	��B	��B	�FB	��B	�UB	ʌB	��B	ȴB	�EB	��B	̘B	�[B	�sB	خB	�KB	�KB	یB	��B	�vB	�vB	��B	��B	� B	�2B	�B	�DB	�B	�B	�B	�TB	�2B	��B	��B	�B	��B
+B
�B
�B
{B
_B
CB
'RB
*�B
,�B
-CB
.B
/B
/OB
0!B
1'B
0�B
/�B
0�B
0�B
5?B
6�B
;0B
?HB
?}B
@�B
B'B
B'B
B�B
B�B
B�B
C-B
DgB
D�B
HB
PB
S&B
T�B
U�B
VmB
W�B
[�B
]�B
_�B
bB
gB
h>B
iyB
j�B
l"B
l�B
l�B
m)B
m�B
n�B
o5B
q�B
rB
r|B
t�B
w2B
z�B
|�B
}�B
cB
��B
�B
��B
�B
��B
��B
��B
��B
�rB
��B
�B
�YB
��B
��B
�kB
�qB
��B
�B
��B
��B
��B
�VB
��B
�B
�VB
��B
�B
��B
�:B
�zB
��B
�LB
��B
�RB
�$B
��B
�6B
��B
��B
�!B
��B
��B
��B
��B
�'B
��B
��B
�9B
�nB
�9B
��B
�B
��B
��B
�*B
�*B
�B
�BB
�BB
�}B
��B
�B
��B
��B
�'B
�'B
�[B
�[B
�[B
B
��B
ĜB
�B
�B
�9B
�B
�EB
ȴB
��B
�#B
ʌB
�^B
�pB
�HB
��B
�B
��B
��B
�TB
�[B
��B
�2B
՛B
՛B
��B
�mB
��B
�B
�B
�#B
��B
�]B
ܒB
�)B
��B
��B
��B
��B
��B
ޞB
ޞB
�jB
�jB
�B
�dB
��B
ݘB
ޞB
�;B
�pB
ߤB
ߤB
�BB
��B
��B
�B
��B
�NB
�B
�B
�B
��B
��B
�ZB
�&B
�&B
��B
�,B
��B
��B
��B
��B
�B
��B
�mB
�B
�
B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�B
��B
�iB
�B
�B
�B
�oB
�B
�B
�oB
�B
�B
�B
��B
�B
�B
�B
�B
�B
�B
��B
�TB
�TB
�TB
��B
�%B
��B
��B
��B
��B
��B
�fB
��B
�fB
�fB
��B
��B
��B
��B
��B
��B
�DB
�xB
�JB
�B
�B
��B
�B
�B
��B
��B
��B
�]B
��B
��B
��B
��B;BoB;B;B;B;BoB;BoB�BB�B�B�B�B�B�BB�B�BuB�BB�B�B�B�BBB�BBBSB�B�B�B�B�B�B�B�B�B�B+B�B�B�B�B	B	7B	lB	�B	�B	�B	�B	�B
	B
	B
	B
	B
	B
	B
	B
=B
=B
=B
=B
=B
=B
�BDBBBxBxBxB�BB~B�B�B�B�B�B�B�B�B�B�B�B�B�B\B�B�B B B�BhB:BB:B�B�BB�B�B�BFBBFB�B�B�B�B�B�BB�BMBMB�B�BBSBSBSB�B�B$B$BYBYBYB�B�B�B+B+B_B�B�B�BBBkB=B�BBCBBB�B�B�BBBBIBIB�B�BOB�B!B�B�B�B�B�B \B �B �B �B �B �B �B!�B!�B!�B!�B!�B!�B"�B"�B#:B#nB#�B#�B#�B#�B#�B#�B$B$@B$�B%B%�B%�B%�B%�B%�B&B&�B&�B&�B&�B'RB'RB'RB'�B'�B(XB($B(�B(�B(�B(�B(�B)*B)�B)�B*�B+6B+6B+6B+kB+kB+kB+kB+�B+�B,=B,�B,�B-B,�B-B-B-wB.B.B.�B.�B.�B/B/OB/�B/�B/OB/OB/�B/�B0!B0!B0�B0�B0�B0�B0�B0�B0�B1'B1'B1�B2-B2�B2�B2�B3hB3�B3�B4B49B49B49B49B49B5?B6�B6zB6�B6�B6�B7LB7LB7�B7�B7�B7�B7�B7�B8�B9$B9XB9XB9XB9XB9$B9XB9XB9XB9XB9$B9$B9$B9�B:*B:*B9�B9�B:^B<B<B<6B<jB<jB<�B=�B=�B>�B>�B>�B?B?HB?HB?}B?}B?}B?}B?}B?}B?}B?�B?�B?�B?}B@�BAUBA�BA�BA�BA�BB'BB�BB�BB�BB�BB�BB�BB�BB�BB�BB�BCaBC-BC-BC-BCaBCaBC�BC�BC�BD3BD3BDgBDgBDgBDgBD�BD�BD�BD�BEmBE�BE�BE�BFBF?BF?BFtBF�BFtBFtBF�BGEB�?B��B��B��B�B�}B��B�?B�!B��B�FB�B�-B��B��B��B�nB�3B��B�aB�3B��B��B��B��B�hB�hB��B�nB�3B��B��B��B�[B�-B�3B�?B��B��B�B��B�[B�3B��B�[B��B�-B�aB�3B��B�3B�'B��B��B��B��B��B�-B��B�aB��B��B�-B��B�aB��B��B��B��B��B��B��B�-B�hB��B��B�aB��B��B�3B�aB��B�-B�hB�-B�3B��B�hB�3B��B��B�3B��B�hB�3B�3B�hB��B��B�B��B��B��B��B�-B��B�-B��B�-B�nB�aB�9B��B�nB��B�B��B�B�-B�nB��B��B��B��B��B�3B��B��B�hB��B��B��B�3B��B�nB��B��B��B�B�hB��B�hB��B��B��B��B��B��B�aB��B�aB�B��B��B�3B�hB�9B��B�B��B�B��B��B��B�B��B��B�3B�B�B��B�B��B�9B��B�B��B�9B��B�3B�B��B�nB��B�?B��B�tB�aB��B��B�nB�hB��B�nB�B�B��B��B�9B�B�hB�tB��B�?B��B�B�B�B�9B��B�tB��B�?B��B�?B��B�B��B�nB�nB��B�nB��B�B��B��B�3B�hB�9B�aB�B�hB�tB�hB��B��B��B��B�aB�B�B��B��B�nB�9B��B�?B�?B�B��B��B�tB��B�B�-B��B��B�B�aB�'B�B��B��B�qB��B�gB��B�gB��B��B� BȴB��B�BĜB�B��B��B��B�<B�B�*B�BǮB�RB��B��BچBɺB�tBѷB�BӏB�B՛B�;B�B�,BߤB�fB�pB�/B��B�/B�BB)*B��B iB�.BMB+B�B�B@B�B�B�BB:B&�B1B�BVB-CB3hB�B�BOB"�BGEB$BMB�B_B�B1BqB7BOBBB#�B!�BH�B.B.�B;dBd�B/�B(�B*�B'�B&�B)�B&�B'�B'RB&LB*0B%�B($B&LB$B$�B&LB'�B$�B+kBDgB/�B7B-B,�B)�B+B+6B-wB$tB"hB/�B/�B�B:B	BMB:BbB"BxBDBBBB�B�BoB�BGB�BPB��B-wB{B��BB��B�BfB�B��B��B��B��B��B��B��B�+B��B��B�lB�TB��B�fB�B�ZB�B�TB��B�`B��B�B�MB��B�GB�B�B�B�vB�MB��B�B�B�iB�vB��B�]B�"B�B�)B��B��B�B�B�;B�KB��B�B��B�B�B�)B�;B��B��B�B�?BϫB�BуB�aB�vB�
B��B��B�mB��B�XB�qB��B�B��B��B��B�B��B��B��B�*B��B��B��B�B��B�B�B�hB��B��B��B�B�XB��B�hB�zB�zB�6B��B�LB��B�LB��B��B�B�B�B��B��B�nB��B��B��B�B�jB�)B�dB�B��B�'B�XB�*B��B�UB��B��B�qB�6B��B�=B�qB�B�B�0B�eB��B��B�0B�_B��B�XB�B�B�tB�FB�tB�B��B��B�B�6B��B��B��B�SB��B��B��B�B��B�:B��B��B��B�~B��B��B�rB��B�FB��B� B�%B��B��B�+BcB.B�4Bz�B|PB{By>B{�BzBwfB�iBz�By�Bk�Bp�Bs�Bs�Bm)Bv`Bm�BqABn�Bo�Bm)BiDBn�Bk�Bk�Bp�Bl�Bh>Bm�Be,Bv�Bi�B_�BgBsBiDBb�Bd�Bb�Bp�Bl"Bh�B[#B]/Bd�B]/BV9BV�BZ�BW�BT�BRTBRTBFBI�Bc�BB'BC�B?HB8�B;�B7�B;�B9�B<�B=qB2�B/B.�B.�B/B-wB.IB/B(XB($B(�B'B$�B,qB2-B*0BVB#�BqB�B=BqB+BeBqB�BBBB�B�B�BoB B�B�B�BoBB(B B�B�B�B�B+B_BBB�BBB iB �B 4B��B�(B	lB�8B�VB�2B��B��B�WB�)B�B��B��B�B��B��B�sB��B�`B�8B�TB��B��B� B�B�&B�B�B� B��B��BߤB��B��B�vBܒB��B�,B�9B��BٴB�XB�XB�B��B��B�B�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021120500450120211205004501IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121501005620211215010056QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021121501005620211215010056QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225820220906072258IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                