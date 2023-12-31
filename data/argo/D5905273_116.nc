CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-05T08:30:09Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210305083009  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               t   tAA  AOAO7314_008642_116                 7314_008642_116                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�cb˧3@�cb˧311  @�c�6z@�c�6z@1��i�4@1��i�4�b�^�s��b�^�s�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @@  @}p�@�G�@�  @�  A ��A��A   A+�A@  A_\)A\)A�Q�A�Q�A��A�  A�  A�Q�A�  A�\)B(�B  B  B (�B(  B0(�B8  B?�BG�
BO�
BW�
B`(�Bh(�Bo�
Bw�
B�
B�{B�(�B�{B�  B��
B��B�{B�  B�  B�{B�(�B�{B��B��B�  B��B��
B�{B�{B�  B�  B�{B�{B�  B��B��
B�  B�  B��B��B��
B��C
=C
=C��C  C	��C  C  C  C  C  C��C  C  C��C  C��C!��C$  C&  C(
=C*{C,
=C.  C/��C2  C4  C6  C8  C:  C<
=C>
=C@  CB  CD  CF{CH{CI��CK��CN  CP  CR
=CT
=CV
=CX  CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Ci��Cl  Cn
=Cp  Cr
=Ct{Cv
=Cx  Cz
=C|  C}��C�C�C�C�C�
=C�
=C�C�  C�  C�C�C�  C���C���C���C���C���C���C���C�  C�  C�C�C�C�  C���C�  C�  C�C�  C�  C���C���C���C�C�C�C�C�  C�C�C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C�C���C�  C�C�
=C�
=C�  C�C�
=C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C�C�C�  C�  C�  C�  C�C�C�  C�  C�  C�C�  C���C���C�  C�  C�  C�
=C�C���C�  C�C�
=C�
=C�C�  C�C�C�  C�  C���C�C�C�  C���C���C���C���C�C�C���C���C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���D ��DD��D�D�D�D��D  D}qD  D��D�D� D�qD� D�D�D	�D	� D
  D
��D
�qDz�D��Dz�D�qD}qD  D��D  D}qD�qD}qD  D��DD�DD�D  Dz�D��D}qD�qD}qD  D� D�D}qD  D��D�D}qD�qD� D�D��D��D}qD�D��D  D}qD �D ��D!  D!}qD"  D"��D#�D#� D#�qD$}qD$�qD%� D&  D&� D'�D'� D'��D(z�D(�qD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.}qD/  D/��D/�qD0� D1�D1� D2  D2� D3  D3� D4  D4� D5  D5��D5�qD6}qD7  D7��D8  D8� D9�D9��D:D:�D;�D;��D<�D<� D=  D=� D>  D>� D?  D?� D@�D@��DA�DA� DA�qDB}qDC  DC}qDD  DD�DE�DE� DF�DF��DGDG� DG�qDH� DH�qDI}qDJ  DJ� DK  DK� DL�DL��DL�qDM}qDN  DN� DO  DO��DP  DP� DQ�DQ� DR  DR� DR�qDS� DT�DT��DU�DU}qDU�qDV}qDV�qDW��DXDX� DX�qDY� DZ  DZ��D[�D[� D[�qD\}qD]  D]�D^D^��D_�D_��D`�D`��Da�Da��DbDb� Db�qDc� Dc�qDdz�Dd�qDe� Df  Df}qDf��Dg� Dh�Dh��Di  Di� Dj  Dj}qDj�qDk��Dl  Dl� Dm  Dm� Dn  Dn� DoDo�Dp  Dp� Dq�Dq��Dr�Dr��Ds�Ds��Dt  Dt� Du�Du� Du�qDv� Dw�Dw� Dw�qDxz�Dy  Dy�Dz�Dz}qDz�qD{� D|  D|� D}�D}��D~D~��D  D}qD��D�@ D��HD�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�B�D���D�� D�HD�AHD�~�D�� D�  D�@ D��HD���D���D�@ D�� D�� D�HD�AHD��HD�D�HD�@ D��HD�D�HD�>�D�� D�� D���D�=qD�~�D�� D�  D�@ D�� D�� D���D�>�D��HD���D�  D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�  D�>�D�� D��HD��D�@ D�� D��HD��D�AHD�� D�� D���D�@ D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D��qD��qD�>�D�� D��HD��D�AHD�~�D�� D�  D�>�D�}qD���D���D�@ D��HD�� D���D�>�D�~�D���D���D�@ D��HD�� D���D�>�D�� D�D�HD�@ D�� D���D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD�D��D�B�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D��HD�HD�AHD���D�� D�  D�AHD��HD��HD�  D�B�D�� D�� D�  D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D���D�  D�@ D�}qD���D�  D�AHD�� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��HD��HD�HD�@ D�~�D���D�  D�B�D��HD��HD�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D��HD��HD�  D�@ D�� D�D�  D�@ D�� D�� D�HD�B�DHD�� D���D�@ DÀ D�� D�  D�>�DĀ D�D�  D�>�DŁHDž�D��qD�=qD�}qDƾ�D���D�>�Dǀ D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ Dʀ Dʾ�D�  D�AHDˁHD�� D�HD�@ D́HD��HD�  D�AHD̀ D;�D�  D�>�D�~�D�� D���D�@ D�~�DϾ�D�  D�AHD�~�Dо�D���D�@ DсHD�� D���D�>�DҀ D�� D�  D�>�DӀ D��HD�  D�>�D�~�DԾ�D���D�>�D�~�D�� D�  D�AHDւ�D�� D���D�=qD�}qD׾�D���D�@ D؁HD�� D���D�>�D�~�D�� D�  D�@ DځHD��HD�  D�AHDۀ D�� D�HD�@ D܀ D��HD�  D�@ D݁HD�� D�  D�@ Dހ D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D� D��HD�HD�>�D�~�D�� D���D�>�D�~�D�� D��D�AHD� D侸D���D�=qD�~�D��HD�  D�>�D� D�� D���D�AHD炏D��HD�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�@ D� D�� D�  D�@ D� D�� D�  D�AHD� D쾸D�  D�AHD� D���D�  D�>�D�~�D�� D�HD�AHD�HD�� D�  D�>�D�~�D�� D�  D�>�D�HD��HD�  D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD�  D�@ D�� D�� D�HD�AHD�~�D���D���D�=qD�~�D�� D���D�@ D��HD��HD�HD�@ D�}qD�� D�HD�1�D�s3?\)?��?aG�?�=q?���?Ǯ?�G�?��H@��@�R@0��@=p�@O\)@aG�@p��@�  @�=q@��@��H@��
@��@�33@�(�@��
@���@�
=@�p�@�ff@�\)@�Q�A ��A�A	��A{A33A�A�A   A#�
A(Q�A,(�A0��A5�A:=qA>�RAC33AHQ�AL(�AP��AU�AY��A^�RAc33Ag�Al(�Ap  Au�Ay��A~�RA��A�(�A�ffA���A��HA�p�A��A���A�(�A��RA�G�A��
A�A���A��HA��A�\)A���A��
A�{A�Q�A��\A��A�\)A���A�(�A�ffA�G�A��
AƸRA���A�33A�p�A�  A�=qA���A�
=Aٙ�A�(�A�ffA���A�33A�p�A�  A�=qA�z�A�ffA���A�A�A�Q�A��\A�z�A�ffB z�BB�HB(�BG�B�\B�
B	�B
=qB\)B��BB�HB  B�B=qB�B��B=qB33BQ�Bp�B�\B�
B��B{B\)B ��B"{B#\)B$z�B%��B&�HB'�
B)�B*=qB+�B,��B.{B/33B0z�B1��B2�RB3�
B5�B6�\B7�
B9�B:=qB;\)B<��B=B>�HB@Q�BA��BB�HBD(�BEG�BF�\BG�BH��BJ=qBK�BL��BM�BO33BPQ�BQp�BR�HBT(�BUp�BV�RBW�
BX��BZ=qB[�B\��B^=qB_�B`��Bb{Bc\)Bd��Bf=qBg�Bh��Bj{Bk\)Bl��Bn{Bo\)Bp��Br=qBs�Bt��Bv{Bw�Bx��Bz{B{�B|��B~ffB�B�ffB�
=B��
B�z�B��B��
B�z�B��B��B�Q�B�
=B�B�ffB���B���B�(�B���B�p�B�{B��RB�\)B��B��\B��B��B�=qB��HB��B�{B���B��B��B�=qB��HB�p�B�{B��\B��B��B�Q�B�
=B�B�Q�B���B��B�=qB���B���B�=qB��HB���B�=qB�
=B��
B��\B�33B�  B���B�p�B�(�B���B��B�z�B�33B��
B���B�p�B�=qB���B��B�Q�B��B��B��RB��B�(�B���B��B��\B�\)B�  B���B�p�B�=qB�
=B��
B��\B�33B��B��RB��B�Q�B���B��B�ffB�G�B�  B£�B�p�B�=qB�
=B�B�z�B�G�B�{B��HBə�B�Q�B�
=B�  Ḅ�B�\)B�{B��HBϙ�B�ffB��B�Bҏ\B�\)B�  BԸRB�p�B�=qB�
=B�B�ffB�33B�  Bڣ�B�G�B�{B���B݅B�(�B��HB߮B�z�B��B��
B�RB�p�B�(�B���B噚B�ffB��B�B�\B�\)B�(�B��HB뙚B�ffB�33B��
B��B�p�B�=qB���B�B�z�B�\)B�{B���B��B�z�B�\)B�{B���B��B��\B�33B�  B���B��B�ffB��C   C Q�C �C{Cp�CC
=CQ�C�C�C(�CffC�C�C�CG�C�CC�C�C\)C��CC�C33CffC��CC  C=qCp�C��C��C{C=qCffC�C�HC	
=C	=qC	z�C	�C	�
C

=C
Q�C
z�C
��C
�HC�CG�Cp�C�C�C�C=qC�C�RC�HC{CG�Cz�C�RC�
C{CQ�Cz�C��C�HC�CG�Cz�CC��C�CQ�C�\CC�C(�CffC��CC  C=qCp�C��C�
C{C=qCp�C�C�HC{CG�C�C�RC�HC�C\)C�\C�RC  C33C\)C��C�
C  C33Cz�C��C�
C
=CG�Cp�C��C�
C{C=qC\)C��C�
C  C�C\)C��C�RC�C(�CQ�Cz�C�RC�HC
=C=qCz�C��C��C
=C=qCffC�\C��C  C�C\)C�\C�RC  C(�C\)C��C�RC��C 33C ffC �\C ��C!
=C!33C!ffC!�C!�HC"
=C"Q�C"�C"�C"��C#33C#\)C#�\C#�
C$  C$33C$z�C$��C$�HC%�C%Q�C%z�C%C%��C&�C&ffC&��C&��C'
=C'G�C'z�C'�C'��C(�C(Q�C(��C(��C(��C)=qC)p�C)��C)�HC*{C*G�C*z�C*�RC*�HC+(�C+\)C+�C+C,  C,(�C,ffC,��C,��C-
=C-G�C-p�C-�C-�C.{C.G�C.�\C.�RC.�C/33C/\)C/�\C/�
C0  C0=qC0z�C0�C0�C1(�C1\)C1��C1�
C2
=C2Q�C2�C2�RC3  C333C3ffC3�RC3�HC4�C4ffC4��C4��C5{C5G�C5�C5��C6  C633C6z�C6�C6�C733C7ffC7��C7�C8{C8Q�C8��C8��C9  C9G�C9�C9C:
=C:=qC:z�C:C:��C;33C;z�C;�C;�C<33C<\)C<�C<�HC=�C=\)C=�\C=�
C>{C>Q�C>��C>��C?
=C?Q�C?�C?��C@
=C@G�C@�\C@��CA
=CAQ�CA�CA��CB
=CBG�CB�\CB��CC
=CCQ�CC�CC�
CD{CDQ�CD��CD�
CE�CEffCE��CE�CF�CFffCF�RCF�HCG(�CGp�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               ?��@   @@  @}p�@�G�@�  @�  A ��A��A   A+�A@  A_\)A\)A�Q�A�Q�A��A�  A�  A�Q�A�  A�\)B(�B  B  B (�B(  B0(�B8  B?�BG�
BO�
BW�
B`(�Bh(�Bo�
Bw�
B�
B�{B�(�B�{B�  B��
B��B�{B�  B�  B�{B�(�B�{B��B��B�  B��B��
B�{B�{B�  B�  B�{B�{B�  B��B��
B�  B�  B��B��B��
B��C
=C
=C��C  C	��C  C  C  C  C  C��C  C  C��C  C��C!��C$  C&  C(
=C*{C,
=C.  C/��C2  C4  C6  C8  C:  C<
=C>
=C@  CB  CD  CF{CH{CI��CK��CN  CP  CR
=CT
=CV
=CX  CY��C\  C^  C`  Cb
=Cd  Cf  Ch  Ci��Cl  Cn
=Cp  Cr
=Ct{Cv
=Cx  Cz
=C|  C}��C�C�C�C�C�
=C�
=C�C�  C�  C�C�C�  C���C���C���C���C���C���C���C�  C�  C�C�C�C�  C���C�  C�  C�C�  C�  C���C���C���C�C�C�C�C�  C�C�C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C�C���C�  C�C�
=C�
=C�  C�C�
=C�C�  C�  C�  C���C�  C�  C�  C�C�  C���C�C�C�  C�  C�  C�  C�C�C�  C�  C�  C�C�  C���C���C�  C�  C�  C�
=C�C���C�  C�C�
=C�
=C�C�  C�C�C�  C�  C���C�C�C�  C���C���C���C���C�C�C���C���C�C�  C�  C�  C�  C�  C�  C���C���C���C���C���D ��DD��D�D�D�D��D  D}qD  D��D�D� D�qD� D�D�D	�D	� D
  D
��D
�qDz�D��Dz�D�qD}qD  D��D  D}qD�qD}qD  D��DD�DD�D  Dz�D��D}qD�qD}qD  D� D�D}qD  D��D�D}qD�qD� D�D��D��D}qD�D��D  D}qD �D ��D!  D!}qD"  D"��D#�D#� D#�qD$}qD$�qD%� D&  D&� D'�D'� D'��D(z�D(�qD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.}qD/  D/��D/�qD0� D1�D1� D2  D2� D3  D3� D4  D4� D5  D5��D5�qD6}qD7  D7��D8  D8� D9�D9��D:D:�D;�D;��D<�D<� D=  D=� D>  D>� D?  D?� D@�D@��DA�DA� DA�qDB}qDC  DC}qDD  DD�DE�DE� DF�DF��DGDG� DG�qDH� DH�qDI}qDJ  DJ� DK  DK� DL�DL��DL�qDM}qDN  DN� DO  DO��DP  DP� DQ�DQ� DR  DR� DR�qDS� DT�DT��DU�DU}qDU�qDV}qDV�qDW��DXDX� DX�qDY� DZ  DZ��D[�D[� D[�qD\}qD]  D]�D^D^��D_�D_��D`�D`��Da�Da��DbDb� Db�qDc� Dc�qDdz�Dd�qDe� Df  Df}qDf��Dg� Dh�Dh��Di  Di� Dj  Dj}qDj�qDk��Dl  Dl� Dm  Dm� Dn  Dn� DoDo�Dp  Dp� Dq�Dq��Dr�Dr��Ds�Ds��Dt  Dt� Du�Du� Du�qDv� Dw�Dw� Dw�qDxz�Dy  Dy�Dz�Dz}qDz�qD{� D|  D|� D}�D}��D~D~��D  D}qD��D�@ D��HD�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�B�D���D�� D�HD�AHD�~�D�� D�  D�@ D��HD���D���D�@ D�� D�� D�HD�AHD��HD�D�HD�@ D��HD�D�HD�>�D�� D�� D���D�=qD�~�D�� D�  D�@ D�� D�� D���D�>�D��HD���D�  D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D�� D�� D�  D�>�D�� D�� D�HD�AHD��HD�� D�HD�@ D�~�D��HD�  D�>�D�� D��HD��D�@ D�� D��HD��D�AHD�� D�� D���D�@ D�� D��HD�  D�>�D�� D�� D�  D�AHD�� D��qD��qD�>�D�� D��HD��D�AHD�~�D�� D�  D�>�D�}qD���D���D�@ D��HD�� D���D�>�D�~�D���D���D�@ D��HD�� D���D�>�D�� D�D�HD�@ D�� D���D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�AHD��HD�D��D�B�D�� D��HD�  D�>�D�� D�� D�  D�@ D�~�D��HD�HD�AHD���D�� D�  D�AHD��HD��HD�  D�B�D�� D�� D�  D�@ D��HD�� D���D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D���D�  D�@ D�}qD���D�  D�AHD�� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��HD��HD�HD�@ D�~�D���D�  D�B�D��HD��HD�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D��HD��HD�  D�@ D�� D�D�  D�@ D�� D�� D�HD�B�DHD�� D���D�@ DÀ D�� D�  D�>�DĀ D�D�  D�>�DŁHDž�D��qD�=qD�}qDƾ�D���D�>�Dǀ D�� D�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ Dʀ Dʾ�D�  D�AHDˁHD�� D�HD�@ D́HD��HD�  D�AHD̀ D;�D�  D�>�D�~�D�� D���D�@ D�~�DϾ�D�  D�AHD�~�Dо�D���D�@ DсHD�� D���D�>�DҀ D�� D�  D�>�DӀ D��HD�  D�>�D�~�DԾ�D���D�>�D�~�D�� D�  D�AHDւ�D�� D���D�=qD�}qD׾�D���D�@ D؁HD�� D���D�>�D�~�D�� D�  D�@ DځHD��HD�  D�AHDۀ D�� D�HD�@ D܀ D��HD�  D�@ D݁HD�� D�  D�@ Dހ D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D� D��HD�HD�>�D�~�D�� D���D�>�D�~�D�� D��D�AHD� D侸D���D�=qD�~�D��HD�  D�>�D� D�� D���D�AHD炏D��HD�  D�>�D�~�D�� D�HD�AHD�HD��HD�HD�@ D� D�� D�  D�@ D� D�� D�  D�AHD� D쾸D�  D�AHD� D���D�  D�>�D�~�D�� D�HD�AHD�HD�� D�  D�>�D�~�D�� D�  D�>�D�HD��HD�  D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D� D��HD�  D�@ D�� D�� D�HD�AHD�~�D���D���D�=qD�~�D�� D���D�@ D��HD��HD�HD�@ D�}qD�� D�HD�1�G�O�?\)?��?aG�?�=q?���?Ǯ?�G�?��H@��@�R@0��@=p�@O\)@aG�@p��@�  @�=q@��@��H@��
@��@�33@�(�@��
@���@�
=@�p�@�ff@�\)@�Q�A ��A�A	��A{A33A�A�A   A#�
A(Q�A,(�A0��A5�A:=qA>�RAC33AHQ�AL(�AP��AU�AY��A^�RAc33Ag�Al(�Ap  Au�Ay��A~�RA��A�(�A�ffA���A��HA�p�A��A���A�(�A��RA�G�A��
A�A���A��HA��A�\)A���A��
A�{A�Q�A��\A��A�\)A���A�(�A�ffA�G�A��
AƸRA���A�33A�p�A�  A�=qA���A�
=Aٙ�A�(�A�ffA���A�33A�p�A�  A�=qA�z�A�ffA���A�A�A�Q�A��\A�z�A�ffB z�BB�HB(�BG�B�\B�
B	�B
=qB\)B��BB�HB  B�B=qB�B��B=qB33BQ�Bp�B�\B�
B��B{B\)B ��B"{B#\)B$z�B%��B&�HB'�
B)�B*=qB+�B,��B.{B/33B0z�B1��B2�RB3�
B5�B6�\B7�
B9�B:=qB;\)B<��B=B>�HB@Q�BA��BB�HBD(�BEG�BF�\BG�BH��BJ=qBK�BL��BM�BO33BPQ�BQp�BR�HBT(�BUp�BV�RBW�
BX��BZ=qB[�B\��B^=qB_�B`��Bb{Bc\)Bd��Bf=qBg�Bh��Bj{Bk\)Bl��Bn{Bo\)Bp��Br=qBs�Bt��Bv{Bw�Bx��Bz{B{�B|��B~ffB�B�ffB�
=B��
B�z�B��B��
B�z�B��B��B�Q�B�
=B�B�ffB���B���B�(�B���B�p�B�{B��RB�\)B��B��\B��B��B�=qB��HB��B�{B���B��B��B�=qB��HB�p�B�{B��\B��B��B�Q�B�
=B�B�Q�B���B��B�=qB���B���B�=qB��HB���B�=qB�
=B��
B��\B�33B�  B���B�p�B�(�B���B��B�z�B�33B��
B���B�p�B�=qB���B��B�Q�B��B��B��RB��B�(�B���B��B��\B�\)B�  B���B�p�B�=qB�
=B��
B��\B�33B��B��RB��B�Q�B���B��B�ffB�G�B�  B£�B�p�B�=qB�
=B�B�z�B�G�B�{B��HBə�B�Q�B�
=B�  Ḅ�B�\)B�{B��HBϙ�B�ffB��B�Bҏ\B�\)B�  BԸRB�p�B�=qB�
=B�B�ffB�33B�  Bڣ�B�G�B�{B���B݅B�(�B��HB߮B�z�B��B��
B�RB�p�B�(�B���B噚B�ffB��B�B�\B�\)B�(�B��HB뙚B�ffB�33B��
B��B�p�B�=qB���B�B�z�B�\)B�{B���B��B�z�B�\)B�{B���B��B��\B�33B�  B���B��B�ffB��C   C Q�C �C{Cp�CC
=CQ�C�C�C(�CffC�C�C�CG�C�CC�C�C\)C��CC�C33CffC��CC  C=qCp�C��C��C{C=qCffC�C�HC	
=C	=qC	z�C	�C	�
C

=C
Q�C
z�C
��C
�HC�CG�Cp�C�C�C�C=qC�C�RC�HC{CG�Cz�C�RC�
C{CQ�Cz�C��C�HC�CG�Cz�CC��C�CQ�C�\CC�C(�CffC��CC  C=qCp�C��C�
C{C=qCp�C�C�HC{CG�C�C�RC�HC�C\)C�\C�RC  C33C\)C��C�
C  C33Cz�C��C�
C
=CG�Cp�C��C�
C{C=qC\)C��C�
C  C�C\)C��C�RC�C(�CQ�Cz�C�RC�HC
=C=qCz�C��C��C
=C=qCffC�\C��C  C�C\)C�\C�RC  C(�C\)C��C�RC��C 33C ffC �\C ��C!
=C!33C!ffC!�C!�HC"
=C"Q�C"�C"�C"��C#33C#\)C#�\C#�
C$  C$33C$z�C$��C$�HC%�C%Q�C%z�C%C%��C&�C&ffC&��C&��C'
=C'G�C'z�C'�C'��C(�C(Q�C(��C(��C(��C)=qC)p�C)��C)�HC*{C*G�C*z�C*�RC*�HC+(�C+\)C+�C+C,  C,(�C,ffC,��C,��C-
=C-G�C-p�C-�C-�C.{C.G�C.�\C.�RC.�C/33C/\)C/�\C/�
C0  C0=qC0z�C0�C0�C1(�C1\)C1��C1�
C2
=C2Q�C2�C2�RC3  C333C3ffC3�RC3�HC4�C4ffC4��C4��C5{C5G�C5�C5��C6  C633C6z�C6�C6�C733C7ffC7��C7�C8{C8Q�C8��C8��C9  C9G�C9�C9C:
=C:=qC:z�C:C:��C;33C;z�C;�C;�C<33C<\)C<�C<�HC=�C=\)C=�\C=�
C>{C>Q�C>��C>��C?
=C?Q�C?�C?��C@
=C@G�C@�\C@��CA
=CAQ�CA�CA��CB
=CBG�CB�\CB��CC
=CCQ�CC�CC�
CD{CDQ�CD��CD�
CE�CEffCE��CE�CF�CFffCF�RCF�HCG(�CGp�CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��
A��
A���A��
A���A��
A��A���A���A�  A�A�A���A���A���A��A��yA�ĜA��!A���A���A��DA�z�A�M�A�A�A�A�A�C�A�E�A�E�A�C�A�(�A��A�VA�-A�n�A���A���A���A���A���A���A��A�(�A�S�A�33A��-A�G�A��HA���A�;dA�;dA��HA�$�A�
=A���A��A�\)A�33A���A��7A���A�$�A�Q�A�"�A�VA�$�A�jA���A��A�oA��uA�
=A�1'A���A��A�E�A�;dA�G�A��;A���A��A�t�A�I�A���A��`Ay�As;dAoAnI�Al�!AlVAhAa�A]hsAX��ATn�AP�/AM
=AH�+AD��AB��A@��A=��A<-A;
=A9�hA7�PA5oA4A�A2�HA1�wA0��A.M�A-33A-S�A-�A+\)A*n�A(��A'hsA&bA${A#
=A!x�A r�A�wA��A\)A1'A��A�A7LAAr�AA��A��A;dA/AK�A��A�/A�jAA�AbNA�A�AK�A
=A��A
��AdZA$�A��A�7A�AZA1'A��AhsA ��A ^5A �A z�A $�@�%@��A  �A~�A��AE�A�PA �A n�@��A 1A {A �\A
=A�A�FA ĜA $�@��P@��A �A n�@��w@�&�@���@���@��m@��H@�=q@��#@��@�Q�@��;@��@��`@�t�@�ff@�/@�E�@�r�@�bN@�1'@�ƨ@�n�@���@�hs@�9@�(�@��
@�(�@�
=@�@�b@�7L@��@���@�Q�@�9X@�p�@�h@�%@��`@�r�@���@���@�$�@��#@�h@�Z@߶F@�K�@ޟ�@އ+@�~�@�V@�$�@��@݉7@�/@ܣ�@��@ڟ�@٩�@�G�@�7L@�&�@��@���@�|�@���@֧�@�E�@��T@Ցh@Ձ@ԛ�@�1@�"�@�v�@���@��@Гu@��@ύP@�
=@θR@�E�@̬@�b@�l�@��@�V@���@ȴ9@��@���@��H@�V@�$�@���@�/@��`@�%@���@�Q�@��@�-@���@�?}@�&�@�V@��@�j@�1@���@��@�l�@�\)@��@�ȴ@�5?@��#@���@�Ĝ@��@��@��@�9X@�ƨ@�K�@�o@���@��\@�J@���@�r�@��@�;d@�~�@��-@�p�@�&�@��j@�j@��m@���@��!@�-@��#@��7@��9@�ƨ@��P@�@��T@���@��7@�x�@�p�@�V@��D@���@�33@�33@�\)@�t�@�S�@�C�@���@��\@�~�@�=q@���@�V@��u@���@�\)@��\@�$�@�@���@�`B@���@���@���@��@�/@��j@�Q�@�Q�@�bN@�r�@��
@��@�dZ@�C�@���@���@�ff@��T@�p�@�X@�O�@�7L@��@���@��j@��u@�z�@�Z@�9X@�b@��m@�dZ@�+@��R@�~�@�v�@�V@���@�G�@��@�bN@�b@��;@��
@�+@��y@���@���@�ff@�{@��#@�@��@�/@���@�z�@��;@���@�dZ@�K�@�33@�+@�"�@���@�ȴ@��!@�-@��@���@�hs@��@�Ĝ@��@�j@�A�@� �@��
@�dZ@�ȴ@���@���@�^5@�E�@�-@���@��^@�x�@��@��@�  @���@�C�@�ȴ@�ff@�^5@�5?@���@�x�@�hs@�G�@��@��j@�r�@�Q�@�(�@��@��F@�|�@�dZ@�\)@�;d@��@�
=@�@�ȴ@���@�M�@�J@��@��@�@���@�x�@�`B@�G�@��@��@�j@�A�@��@�  @��
@���@�\)@�@���@�ff@��h@�`B@�V@��9@��@�I�@�b@��;@��w@���@�33@�o@��H@���@�~�@�^5@�E�@�J@���@��T@���@��^@���@���@���@��u@��D@�bN@��@��@~�@~�R@~�+@}�T@}��@}�@}/@|�/@|�j@|j@|(�@{��@{t�@{dZ@{33@z=q@zJ@y�#@y�^@y��@y��@yx�@yX@x��@xQ�@w�@w\)@wK�@v�y@v��@vE�@v@u�@t�@t(�@s�
@st�@r�H@r�\@r-@q��@p��@p �@o�@o\)@n�R@n$�@m��@mp�@mV@l�/@l��@l(�@kt�@kS�@j�!@j^5@i�@ix�@h��@h�@h �@g|�@g+@f��@fV@f5?@e�T@ep�@e/@d�/@d�@c��@ct�@c"�@b�\@bn�@b-@a�#@a�7@aX@a�@`bN@` �@`b@_�w@_K�@_
=@^�R@^ff@^{@]��@]p�@\�@\z�@\I�@[�m@[o@Z�H@Z�H@Z��@ZM�@Y�@Y�#@YG�@X��@X��@X�9@X��@X1'@W�@W;d@V�y@VV@V$�@U��@U�h@U`B@UV@T�@T�D@T�D@Tz�@TZ@TI�@S�m@SS�@S@R��@RM�@R=q@R-@Q��@Q�@Q��@Qx�@QG�@Q7L@P��@O�P@N��@N5?@M@M`B@MV@L��@Lj@L1@K�F@K��@KdZ@K@J~�@J=q@I�^@I&�@H��@H�9@H�@H1'@G��@G�w@G��@Gl�@GK�@F�@Fff@F{@F@E@EO�@D�j@Dz�@D(�@CdZ@B=q@A��@A��@A�7@A�7@Ax�@Ax�@A%@@bN@@A�@@1'@@  @?�P@?
=@>��@>$�@=��@=/@=V@<�j@<Z@<I�@<9X@<1@;ƨ@;�F@;��@;"�@;@:��@:�!@:^5@:�@9�@9�^@9�7@9�@8Ĝ@8b@7�@7��@7;d@6��@6��@6��@6�@6��@6��@6E�@5��@5p�@5�@4�/@4��@4�@4��@4�D@4Z@4I�@49X@49X@49X@3�
@3dZ@3o@2�H@2�\@2�@1�^@1G�@0��@0��@0bN@/�@/+@.��@.�y@.ȴ@.��@.�+@.$�@-�-@-`B@,��@,�@,��@,�@+��@+dZ@+"�@*��@*~�@)�#@)�7@)G�@)�@(��@(Ĝ@(r�@(A�@'��@'�P@'�@&�R@&v�@&{@%�-@%p�@%?}@$�@$�j@$j@$1@#ƨ@#S�@#o@"�H@"��@"~�@"�@!hs@ ��@ r�@  �@�;@��@�P@\)@+@�@��@ȴ@��@v�@E�@@��@@�-@�h@O�@/@��@j@Z@1@�
@t�@t�@t�@t�@t�@t�@S�@�H@~�@-@�@�#@��@x�@hs@G�@&�@�`@�u@r�@r�@bN@A�@  @�@��@��@l�@K�@+@�y@�R@��@�@�h@�@`B@/@�@�@j@9X@1@�m@ƨ@�F@�@C�@@��@�\@~�@�\@�\@n�@�#@�^@�7@X@%@Ĝ@�u@�u@�@Q�@1'@ �@b@  @�;@�w@l�@K�@+@��@V@E�@E�@�@��@�-@��@`B@V@�@�/@�/@�@�D@Z@(�@1@��@��@�m@��@��@�@�@�@S�@o@
�H@
�!@
�\@
n�@
J@	�#@	�^@	�7@	x�@	X@	&�@��@Ĝ@�@r�@r�@r�@1'@�;@�w@|�@K�@;d@;d@;d@;d@��@ȴ@�R@�R@��@��@ff@$�@�@��@O�@?}@?}@?}@�@�j@Z@(�@�@1@��@dZ@S�@33A���A���A���A��
A��;A��A��A���A���A���A���A�ȴA���A���A��HA��
A��A���A���A���A���A���A���A���A��A��A��A��A��yA��yA�A�A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�  A�  A�A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A��A��yA��mA��yA��yA��yA��yA��A��A��A��A��#A���A���A���A�A��9A��!A��A��!A��9A��-A��FA��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��hA��PA��DA��A��+A��A��A��A��A�z�A�x�A�x�A�dZA�VA�K�A�O�A�Q�A�G�A�K�A�E�A�A�A�?}A�A�A�?}A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�?}A�?}A�?}A�A�A�C�A�E�A�G�A�G�A�E�A�E�A�C�A�A�A�A�A�E�A�G�A�I�A�I�A�I�A�C�A�=qA�C�A�E�A�G�A�E�A�E�A�G�A�E�A�;dA�(�A�+A�+A�-A�+A��A��A��A� �A��A��A� �A�oA�oA�bA�JA�VA�bA�VA�JA�
=A��A��A�9XA�I�A�K�A�O�A�n�A�l�A�p�A�x�A�v�A��\A��\A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��DA��+A��A��A��A�r�A�dZA�^5A�Q�A�G�A�+A���A���A��hA��
A��A��uA��mA�K�A�9XA�1A���A�\)A�+A���A��uA�oA��hA��A�VA���A��-A�VA��`A��A���A��A�7LA�JA��A��
A�ĜA�n�A�S�A�/A��A��^A��!A���A��+A�dZA��A��jA��hA�^5A�;dA��A�%A���A��A��A��A���A��jA���A��\A�l�A�ƨA�bNA�9XA� �A�
=A�  A��yA�ȴA���A��RA��-A��uA�\)A�;dA�oA���A���A��7A��A�v�A�l�A�C�A�7LA�7LA�1'A�/A�-A�5?A�;dA�
=A��`A�ĜA�$�A��/A���A���A���A�~�A�hsA�A�A�A���A�ȴA���A���A��+A�x�A�`BA��A�ȴA�~�A�$�A�9XA���A�bA���A�(�A�
=A�9XA���A�\)A�A��A�hsA�-A�bA���A�`BA�G�A�JA���A��TA�ƨA���A�XA�-A��mA�p�A��RA�jA�VA�C�A�5?A�(�A��A��/A�A�(�A�33A�&�A�%A��yA��^A�t�A�(�A���A���A���A�z�A�XA�G�A�/A�oA�  A��A��/A���A��^A���A�x�A�G�A�&�A��A�JA�%A���A���A��yA��#A���A�ĜA��!A���A��+A�x�A�l�A�^5A�M�A�G�A�-A�oA��A��;A���A���A��FA���A���A��PA�z�A�\)A�I�A�9XA�&�A�{A�A���A���A��TA�ƨA��A�jA�"�A��A��A�t�A�S�A���A��A�Q�A�7LA�+A�$�A��A�
=A�A���A��yA��;A��
A���A�^5A��+A��#A��A�O�A���A�jA�VA��
A���A���A��PA�|�A�dZA�S�A�;dA� �A�JA���A��#A���A�O�A��A���A��uA��A�~�A�|�A�z�A�x�A�jA�hsA�ffA�dZA�bNA�^5A�`BA�\)A�XA�XA�XA�S�A�G�A�"�A�ƨA�A�M�A��A��A�ȴA�9XA�%A���A��TA���A��uA�=qA�A�ĜA�v�A�A�dZA�1'A�mA%A}A|ffA{p�AzAx��Ax~�AxbAwAw�Aw%Av$�Aul�At�RAtbAs�FAsAq�AqhsAq33Ap�Ap�\ApA�Ap1Ao�mAo�-Ao��Aop�AoK�AoAn��An�jAn�RAn��An�\AnjAn5?Am��Am?}Al�yAl�Al��AlĜAl�9Al�!Al��Al�\Al�+Al�+Al�Al~�Al�Alz�Alv�AlVAl9XAl$�Ak�Ak��Akx�Aj�9Ah�AhJAf��AfbNAe�wAdjAc?}Ab�HAbz�Ab$�AaƨAax�AaA`�uA`�A_�A^��A^��A^�+A]�TA]��A\�HA\�A[�7A[7LAZ��AZ�AY�;AYt�AX��AX~�AX1AWAV�AV�`AV�9AV5?ATĜAS��AR�uARVARA�ARbAQAQdZAQ+AP�HAP��AP9XAO�AO�FAOG�AO
=AN��ANQ�AM�PAKVAJ^5AI�AI�
AI�^AIdZAI%AH��AHJAGAGS�AF�`AF�AE�AEG�AE
=ADĜAD�+ADQ�AD(�ADACACt�AB�HABJAA��AAƨAA��AAx�AA"�A@��A@�DA@r�A@VA@=qA@5?A?�^A>�A> �A=ƨA=��A=G�A<�A<�9A<�\A<�A<^5A<E�A<�A<bA;�TA;ƨA;�-A;�hA;�A;XA:�A:�A:�+A:bNA:VA:I�A:-A9�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               A��
A��
A���A��
A���A��
A��A���A���A�  A�A�A���A���A���A��A��yA�ĜA��!A���A���A��DA�z�A�M�A�A�A�A�A�C�A�E�A�E�A�C�A�(�A��A�VA�-A�n�A���A���A���A���A���A���A��A�(�A�S�A�33A��-A�G�A��HA���A�;dA�;dA��HA�$�A�
=A���A��A�\)A�33A���A��7A���A�$�A�Q�A�"�A�VA�$�A�jA���A��A�oA��uA�
=A�1'A���A��A�E�A�;dA�G�A��;A���A��A�t�A�I�A���A��`Ay�As;dAoAnI�Al�!AlVAhAa�A]hsAX��ATn�AP�/AM
=AH�+AD��AB��A@��A=��A<-A;
=A9�hA7�PA5oA4A�A2�HA1�wA0��A.M�A-33A-S�A-�A+\)A*n�A(��A'hsA&bA${A#
=A!x�A r�A�wA��A\)A1'A��A�A7LAAr�AA��A��A;dA/AK�A��A�/A�jAA�AbNA�A�AK�A
=A��A
��AdZA$�A��A�7A�AZA1'A��AhsA ��A ^5A �A z�A $�@�%@��A  �A~�A��AE�A�PA �A n�@��A 1A {A �\A
=A�A�FA ĜA $�@��P@��A �A n�@��w@�&�@���@���@��m@��H@�=q@��#@��@�Q�@��;@��@��`@�t�@�ff@�/@�E�@�r�@�bN@�1'@�ƨ@�n�@���@�hs@�9@�(�@��
@�(�@�
=@�@�b@�7L@��@���@�Q�@�9X@�p�@�h@�%@��`@�r�@���@���@�$�@��#@�h@�Z@߶F@�K�@ޟ�@އ+@�~�@�V@�$�@��@݉7@�/@ܣ�@��@ڟ�@٩�@�G�@�7L@�&�@��@���@�|�@���@֧�@�E�@��T@Ցh@Ձ@ԛ�@�1@�"�@�v�@���@��@Гu@��@ύP@�
=@θR@�E�@̬@�b@�l�@��@�V@���@ȴ9@��@���@��H@�V@�$�@���@�/@��`@�%@���@�Q�@��@�-@���@�?}@�&�@�V@��@�j@�1@���@��@�l�@�\)@��@�ȴ@�5?@��#@���@�Ĝ@��@��@��@�9X@�ƨ@�K�@�o@���@��\@�J@���@�r�@��@�;d@�~�@��-@�p�@�&�@��j@�j@��m@���@��!@�-@��#@��7@��9@�ƨ@��P@�@��T@���@��7@�x�@�p�@�V@��D@���@�33@�33@�\)@�t�@�S�@�C�@���@��\@�~�@�=q@���@�V@��u@���@�\)@��\@�$�@�@���@�`B@���@���@���@��@�/@��j@�Q�@�Q�@�bN@�r�@��
@��@�dZ@�C�@���@���@�ff@��T@�p�@�X@�O�@�7L@��@���@��j@��u@�z�@�Z@�9X@�b@��m@�dZ@�+@��R@�~�@�v�@�V@���@�G�@��@�bN@�b@��;@��
@�+@��y@���@���@�ff@�{@��#@�@��@�/@���@�z�@��;@���@�dZ@�K�@�33@�+@�"�@���@�ȴ@��!@�-@��@���@�hs@��@�Ĝ@��@�j@�A�@� �@��
@�dZ@�ȴ@���@���@�^5@�E�@�-@���@��^@�x�@��@��@�  @���@�C�@�ȴ@�ff@�^5@�5?@���@�x�@�hs@�G�@��@��j@�r�@�Q�@�(�@��@��F@�|�@�dZ@�\)@�;d@��@�
=@�@�ȴ@���@�M�@�J@��@��@�@���@�x�@�`B@�G�@��@��@�j@�A�@��@�  @��
@���@�\)@�@���@�ff@��h@�`B@�V@��9@��@�I�@�b@��;@��w@���@�33@�o@��H@���@�~�@�^5@�E�@�J@���@��T@���@��^@���@���@���@��u@��D@�bN@��@��@~�@~�R@~�+@}�T@}��@}�@}/@|�/@|�j@|j@|(�@{��@{t�@{dZ@{33@z=q@zJ@y�#@y�^@y��@y��@yx�@yX@x��@xQ�@w�@w\)@wK�@v�y@v��@vE�@v@u�@t�@t(�@s�
@st�@r�H@r�\@r-@q��@p��@p �@o�@o\)@n�R@n$�@m��@mp�@mV@l�/@l��@l(�@kt�@kS�@j�!@j^5@i�@ix�@h��@h�@h �@g|�@g+@f��@fV@f5?@e�T@ep�@e/@d�/@d�@c��@ct�@c"�@b�\@bn�@b-@a�#@a�7@aX@a�@`bN@` �@`b@_�w@_K�@_
=@^�R@^ff@^{@]��@]p�@\�@\z�@\I�@[�m@[o@Z�H@Z�H@Z��@ZM�@Y�@Y�#@YG�@X��@X��@X�9@X��@X1'@W�@W;d@V�y@VV@V$�@U��@U�h@U`B@UV@T�@T�D@T�D@Tz�@TZ@TI�@S�m@SS�@S@R��@RM�@R=q@R-@Q��@Q�@Q��@Qx�@QG�@Q7L@P��@O�P@N��@N5?@M@M`B@MV@L��@Lj@L1@K�F@K��@KdZ@K@J~�@J=q@I�^@I&�@H��@H�9@H�@H1'@G��@G�w@G��@Gl�@GK�@F�@Fff@F{@F@E@EO�@D�j@Dz�@D(�@CdZ@B=q@A��@A��@A�7@A�7@Ax�@Ax�@A%@@bN@@A�@@1'@@  @?�P@?
=@>��@>$�@=��@=/@=V@<�j@<Z@<I�@<9X@<1@;ƨ@;�F@;��@;"�@;@:��@:�!@:^5@:�@9�@9�^@9�7@9�@8Ĝ@8b@7�@7��@7;d@6��@6��@6��@6�@6��@6��@6E�@5��@5p�@5�@4�/@4��@4�@4��@4�D@4Z@4I�@49X@49X@49X@3�
@3dZ@3o@2�H@2�\@2�@1�^@1G�@0��@0��@0bN@/�@/+@.��@.�y@.ȴ@.��@.�+@.$�@-�-@-`B@,��@,�@,��@,�@+��@+dZ@+"�@*��@*~�@)�#@)�7@)G�@)�@(��@(Ĝ@(r�@(A�@'��@'�P@'�@&�R@&v�@&{@%�-@%p�@%?}@$�@$�j@$j@$1@#ƨ@#S�@#o@"�H@"��@"~�@"�@!hs@ ��@ r�@  �@�;@��@�P@\)@+@�@��@ȴ@��@v�@E�@@��@@�-@�h@O�@/@��@j@Z@1@�
@t�@t�@t�@t�@t�@t�@S�@�H@~�@-@�@�#@��@x�@hs@G�@&�@�`@�u@r�@r�@bN@A�@  @�@��@��@l�@K�@+@�y@�R@��@�@�h@�@`B@/@�@�@j@9X@1@�m@ƨ@�F@�@C�@@��@�\@~�@�\@�\@n�@�#@�^@�7@X@%@Ĝ@�u@�u@�@Q�@1'@ �@b@  @�;@�w@l�@K�@+@��@V@E�@E�@�@��@�-@��@`B@V@�@�/@�/@�@�D@Z@(�@1@��@��@�m@��@��@�@�@�@S�@o@
�H@
�!@
�\@
n�@
J@	�#@	�^@	�7@	x�@	X@	&�@��@Ĝ@�@r�@r�@r�@1'@�;@�w@|�@K�@;d@;d@;d@;d@��@ȴ@�R@�R@��@��@ff@$�@�@��@O�@?}@?}@?}@�@�j@Z@(�@�@1@��@dZ@S�G�O�A���A���A���A��
A��;A��A��A���A���A���A���A�ȴA���A���A��HA��
A��A���A���A���A���A���A���A���A��A��A��A��A��yA��yA�A�A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�  A�  A�A���A���A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A���A���A��A��yA��mA��yA��yA��yA��yA��A��A��A��A��#A���A���A���A�A��9A��!A��A��!A��9A��-A��FA��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A��hA��PA��DA��A��+A��A��A��A��A�z�A�x�A�x�A�dZA�VA�K�A�O�A�Q�A�G�A�K�A�E�A�A�A�?}A�A�A�?}A�C�A�C�A�C�A�C�A�C�A�C�A�A�A�?}A�?}A�?}A�A�A�C�A�E�A�G�A�G�A�E�A�E�A�C�A�A�A�A�A�E�A�G�A�I�A�I�A�I�A�C�A�=qA�C�A�E�A�G�A�E�A�E�A�G�A�E�A�;dA�(�A�+A�+A�-A�+A��A��A��A� �A��A��A� �A�oA�oA�bA�JA�VA�bA�VA�JA�
=A��A��A�9XA�I�A�K�A�O�A�n�A�l�A�p�A�x�A�v�A��\A��\A���A���A���A���A���A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��DA��+A��A��A��A�r�A�dZA�^5A�Q�A�G�A�+A���A���A��hA��
A��A��uA��mA�K�A�9XA�1A���A�\)A�+A���A��uA�oA��hA��A�VA���A��-A�VA��`A��A���A��A�7LA�JA��A��
A�ĜA�n�A�S�A�/A��A��^A��!A���A��+A�dZA��A��jA��hA�^5A�;dA��A�%A���A��A��A��A���A��jA���A��\A�l�A�ƨA�bNA�9XA� �A�
=A�  A��yA�ȴA���A��RA��-A��uA�\)A�;dA�oA���A���A��7A��A�v�A�l�A�C�A�7LA�7LA�1'A�/A�-A�5?A�;dA�
=A��`A�ĜA�$�A��/A���A���A���A�~�A�hsA�A�A�A���A�ȴA���A���A��+A�x�A�`BA��A�ȴA�~�A�$�A�9XA���A�bA���A�(�A�
=A�9XA���A�\)A�A��A�hsA�-A�bA���A�`BA�G�A�JA���A��TA�ƨA���A�XA�-A��mA�p�A��RA�jA�VA�C�A�5?A�(�A��A��/A�A�(�A�33A�&�A�%A��yA��^A�t�A�(�A���A���A���A�z�A�XA�G�A�/A�oA�  A��A��/A���A��^A���A�x�A�G�A�&�A��A�JA�%A���A���A��yA��#A���A�ĜA��!A���A��+A�x�A�l�A�^5A�M�A�G�A�-A�oA��A��;A���A���A��FA���A���A��PA�z�A�\)A�I�A�9XA�&�A�{A�A���A���A��TA�ƨA��A�jA�"�A��A��A�t�A�S�A���A��A�Q�A�7LA�+A�$�A��A�
=A�A���A��yA��;A��
A���A�^5A��+A��#A��A�O�A���A�jA�VA��
A���A���A��PA�|�A�dZA�S�A�;dA� �A�JA���A��#A���A�O�A��A���A��uA��A�~�A�|�A�z�A�x�A�jA�hsA�ffA�dZA�bNA�^5A�`BA�\)A�XA�XA�XA�S�A�G�A�"�A�ƨA�A�M�A��A��A�ȴA�9XA�%A���A��TA���A��uA�=qA�A�ĜA�v�A�A�dZA�1'A�mA%A}A|ffA{p�AzAx��Ax~�AxbAwAw�Aw%Av$�Aul�At�RAtbAs�FAsAq�AqhsAq33Ap�Ap�\ApA�Ap1Ao�mAo�-Ao��Aop�AoK�AoAn��An�jAn�RAn��An�\AnjAn5?Am��Am?}Al�yAl�Al��AlĜAl�9Al�!Al��Al�\Al�+Al�+Al�Al~�Al�Alz�Alv�AlVAl9XAl$�Ak�Ak��Akx�Aj�9Ah�AhJAf��AfbNAe�wAdjAc?}Ab�HAbz�Ab$�AaƨAax�AaA`�uA`�A_�A^��A^��A^�+A]�TA]��A\�HA\�A[�7A[7LAZ��AZ�AY�;AYt�AX��AX~�AX1AWAV�AV�`AV�9AV5?ATĜAS��AR�uARVARA�ARbAQAQdZAQ+AP�HAP��AP9XAO�AO�FAOG�AO
=AN��ANQ�AM�PAKVAJ^5AI�AI�
AI�^AIdZAI%AH��AHJAGAGS�AF�`AF�AE�AEG�AE
=ADĜAD�+ADQ�AD(�ADACACt�AB�HABJAA��AAƨAA��AAx�AA"�A@��A@�DA@r�A@VA@=qA@5?A?�^A>�A> �A=ƨA=��A=G�A<�A<�9A<�\A<�A<^5A<E�A<�A<bA;�TA;ƨA;�-A;�hA;�A;XA:�A:�A:�+A:bNA:VA:I�A:-A9�mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	qvB	p�B	oiB	p�B	n�B	m�B	r�B	v�B	xB	{�B	y�B	|�B	�iB	��B	��B	�.B	��B	��B	��B	��B	ȴB	ӏB	�KB	�B	��B	�B	�B	�ZB	�&B	�B	�vB	��B	�mB	��B
�B
�B
)*B
-CB
-CB
-wB
-CB
+B
&B
h
B�BaHB�{B|�B��B�=B��B�OB��B�eB��B�@B�VB�	B�4B��B}�Bu�Bg�B)�B
�sB
�tB
�nB
��B
�VB
�`B
�DB
�HB
ںB
�B
��B
�;B
�B
�qB
�+B
l�B
Z�B
DgB
=�B
!�B	�B	��B	��B	��B	�?B	�OB	�qB	��B	m�B	U2B	9�B	 �B	�B��B��B�^B��B�FB�-B��B�VB��B��B��B�=B�_B��B��B��B�uB��B�qB��B�xB�B�	B�=B��B��B�=B��B��B�B��B��B�B�B��B��B��B��B�*B��B�aB�EBȴBбB�B�B��B	4B	�B��B�B�#B� BȀB�#B��B��B�B�eB�qB��B��B��B�B�BȀBѷB� B�)B�|B�B	IB	9XB	5tB	/�B	DgB	=<B	2-B	K�B	PB	Y�B	jB	jB	z�B	t�B	u�B	u�B	v�B	��B	��B	��B	�PB	��B	��B	��B	��B	��B	�eB	�xB	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�0B	�hB	�_B	��B	�tB	�eB	��B	��B	�dB	�^B	�B	��B	��B	��B	�B	�0B	��B	��B	��B	��B	��B	��B	��B	� B	��B	� B	B	�'B	�-B	�aB	�tB	ƨB	�tB	�tB	��B	�9B	ƨB	�B	ÖB	��B	ÖB	�gB	ƨB	�zB	ǮB	��B	�?B	�B	�tB	��B	�B	ĜB	�aB	��B	�'B	�aB	�gB	�'B	�OB	�3B	�OB	�}B	�B	��B	��B	��B	�aB	ÖB	��B	�B	�?B	��B	ʌB	�)B	�B	�zB	�B	ȴB	ȀB	ȀB	�B	��B	͟B	�B	��B	ѷB	уB	��B	�,B	�mB	�9B	�mB	רB	��B	�KB	�B	��B	�B	ݘB	�B	ޞB	��B	�B	��B	��B	��B	��B	چB	یB	��B	��B	�#B	�#B	یB	�WB	یB	ںB	�B	ٴB	�B	՛B	�2B	�2B	՛B	��B	�9B	�mB	�
B	��B	�B	��B	خB	�KB	��B	�/B	�5B	�B	�B	�B	�TB	�ZB	��B	��B	�B	�B	�HB	�B	��B	�NB	�B	�B	�KB	�KB	�"B	��B	�oB	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�"B	��B	��B	��B	��B
  B
  B
�B
GB
MB
�B
�B
�B
SB
�B
�B
�B
fB
�B
fB
�B
	�B

=B

rB

�B

�B

=B
	�B
DB

rB
B

�B
xB
�B
�B
B
DB

rB

=B

rB
�B
�B
�B
B
JB
JB
JB
~B
�B
~B
�B
�B
VB
�B
.B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
B
B
SB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
7B
7B
	B
�B
�B
IB
~B
�B
B
OB
�B
OB
�B
!B
!B
!B
VB
VB
VB
�B
�B
�B
 'B
 \B
 �B
!-B
!-B
!bB
"hB
"�B
"�B
"�B
"�B
"�B
"�B
#:B
#�B
#�B
$B
%FB
&B
%�B
&�B
'RB
'RB
($B
(�B
(�B
(�B
(�B
*0B
)�B
*�B
+B
+kB
,B
,=B
,�B
,�B
,�B
-CB
-wB
-�B
.�B
.IB
.B
-�B
.}B
/OB
/�B
0�B
/�B
0!B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
1'B
1�B
1�B
1�B
2aB
33B
3�B
3�B
3�B
3�B
3�B
3�B
49B
4�B
4B
4�B
4�B
4�B
4�B
5B
5?B
5?B
6B
6zB
7LB
7B
7�B
8RB
8RB
8�B
8�B
9�B
9�B
:�B
:�B
;dB
;�B
;dB
;0B
;0B
;0B
;dB
;�B
<jB
;�B
=B
<�B
>BB
>BB
>�B
>BB
>�B
?B
?HB
?�B
?�B
?�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B'B
B�B
B[B
B�B
C-B
B�B
C-B
C-B
C�B
C�B
C�B
D�B
D�B
EB
E9B
EmB
E�B
E�B
F?B
F�B
F�B
GB
GzB
HKB
G�B
GzB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
JXB
J�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K^B
K�B
K�B
K�B
K�B
L�B
M6B
M�B
M�B
M�B
NB
NB
NB
NB
NpB
NpB
NpB
O�B
PB
P�B
Q�B
QNB
Q�B
Q�B
RTB
R�B
S[B
S[B
S&B
S�B
S�B
T�B
T�B
U�B
U�B
VB
V9B
VmB
V�B
W
B
W
B
W
B
W
B
W
B
W�B
XB
XB
XB
XEB
X�B
YB
YB
YKB
ZB
[#B
[WB
[�B
[�B
[�B
[�B
[WB
[�B
\]B
\]B
\]B
\]B
\�B
]/B
]dB
]�B
^B
^5B
^5B
^�B
^�B
^�B
^�B
_;B
_pB
_pB
_�B
`B
`B
`BB
`vB
`�B
aB
aB
aHB
a|B
a�B
a�B
c B
b�B
c B
c�B
c�B
dZB
d�B
d�B
e,B
d�B
e`B
e�B
f2B
f�B
f2B
ffB
f�B
gB
gmB
gmB
g�B
gmB
g�B
gmB
g�B
gmB
g�B
g�B
h>B
h>B
h>B
h�B
iB
iDB
jB
jB
jKB
jB
jKB
jKB
jKB
jKB
jB
kB
kQB
kQB
kB
k�B
k�B
lWB
lWB
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
n�B
n�B
o5B
pB
p�B
p;B
p�B
qB
p�B
qAB
qvB
q�B
q�B
rB
r�B
r�B
r�B
r�B
sB
s�B
tTB
tTB
t�B
t�B
t�B
u%B
u%B
u�B
v+B
u�B
v+B
v+B
v�B
v�B
v�B
v�B
w2B
w2B
v�B
w�B
w�B
w�B
x8B
xlB
xlB
y	B
y>B
y�B
y�B
y�B
y�B
yrB
yrB
y�B
zB
zxB
z�B
z�B
{B
{JB
{JB
{JB
{JB
{B
{�B
|PB
|PB
|PB
|B
|�B
|�B
|�B
|�B
}"B
}VB
}VB
}VB
}�B
}�B
}�B
~�B
~�B
~�B
.B
cB
.B
�B
�B
�B
� B
�4B
��B
��B
�B
�oB
�oB
�oB
��B
��B
��B
�B
�uB
�AB
�uB
�uB
��B
�{B
��B
�B
�B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�%B
�YB
�YB
�YB
�YB
�+B
�+B
�+B
�+B
��B
��B
�1B
�1B
�1B
�fB
�fB
��B
�B
�7B
�lB
�7B
�lB
�	B
�	B
�	B
�	B
�	B
�=B
��B
��B
�B
�B
�DB
��B
�B
�JB
�~B
�~B
��B
��B
�B
�B
�PB
�B
��B
��B
��B
�"B
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
�bB
�bB
�bB
��B
�4B
��B
��B
��B
��B
�4B
��B
�B
�B
�B
��B
��B
��B
�B	sB	sB	o5B	qB	p�B	o�B	s�B	p;B	rGB	ncB	p�B	o�B	m�B	qvB	o5B	rGB	m]B	m�B	m�B	o B	n�B	p�B	o�B	m]B	rGB	s�B	s�B	rB	s�B	jKB	|�B	�B	{�B	t�B	y>B	v�B	{�B	{B	z�B	{JB	zB	zB	z�B	{�B	|B	}�B	~]B	~�B	cB	~]B	�B	�oB	��B	�B	�B	��B	��B	��B	�+B	��B	�B	�uB	�uB	��B	��B	��B	�%B	�fB	��B	�lB	��B	�xB	��B	��B	��B	�B	��B	��B	�{B	�MB	�MB	��B	�VB	�tB	�B	��B	�OB	��B	�<B	�BB	�B	��B	��B	�^B	�UB	��B	��B	��B	�-B	B	��B	��B	�mB	�B	��B	�zB	��B	��B	�<B	�BB	�HB	ҽB	ӏB	�B	��B	�mB	��B	��B	֡B	�B	�B	�KB	�vB	�&B	�TB	��B	�ZB	��B	��B	�B	�B	��B	�B	��B	��B	�&B	�B	��B	��B	�B	�B	�ZB	�B	�B	�,B	�ZB	� B	�B	��B	��B	�&B	�B	��B	�&B	�ZB	�B	�B	�B	��B	��B	�
B	�B	�ZB	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�BB	�B	�B	�B	�HB	�B	�HB	�B	��B	� B	��B	�`B	�ZB	�,B	�B	�vB	�B	�B	��B
�B
�B
�B
_B
�B
�B
�B
�B
B
qB
B
�B
#:B
%B
&�B
%�B
)*B
'B
+6B
-B
-�B
-wB
-�B
-CB
,�B
,B
,B
,�B
-�B
-CB
-�B
-�B
-CB
,=B
-B
-CB
-wB
.IB
.B
-wB
-B
-B
-CB
-B
-�B
-�B
,�B
,=B
*0B
)�B
)�B
)�B
(�B
(�B
&B
#�B
%FB
$tB
%FB
+�B
N�B
EB
GEB
qB
�~B
��B
�WB
��B�B�BxB!bB7�Bu�BT�B]/Bm�B{�BhsBy�B�B�%B�:B��B��B
��B��B�B�{B��B�FB��B�+B��B�MB�MB��B�@B�4B�VB��B��B�VB��B�~B��B�B��B�B��B�hB�hB�FB�*B��B�B��B�_B��B�eB�eB�nB��B�B�RB�RB�OB�B��B��B��B��B��B��B�hB�IB�B�B�qB�qB��B�_B�@B�'B��B��B�4B�JB��B��B��B��B��B�iB~�B�B{JBu�Bu%Br�Bt�BzDB{�BsBq�B��BZ�BT�B@B<B�BB
�"B
�KB
�B
��B
�EB
�B
�B
�
B
��B
��B
�tB
��B
�$B
�B
�tB
�LB
�B
��B
��B
�6B
��B
�B
}�B
}�B
cB
�B
��B
�B
�B
�ZB
��B
��B
��B
�]B
��B
�JB
��B  B
��B
�B
��B
�B
�B
�B
��B
�jB
ݘB
ޞB
�5B
��B
��B
�vB
��B
�EB
�sB
�B
چB
��B
��B
�B
��B
��B
�jB
��B
ߤB
�B
��B
�
B
��B
�B
�/B
��B
��B
��B
�DB
�8B
��B
�`B
�ZB
� B
�B
�`B
�B
�BB
ݘB
�WB
�B
��B
��B
�sB
�gB
��B
�mB
ѷB
ʌB
�^B
�OB
�B
�9B
��B
�B
�B
�tB
�FB
�B
�LB
��B
�\B
��B
�xB
�	B
�1B
�@B
�B
�bB
�SB
�VB
��B
~(B
t�B
l�B
j�B
dZB
b�B
c�B
d&B
`BB
_�B
_�B
\�B
ZB
\�B
[WB
bNB
O�B
V�B
R B
DgB
E�B
FB
C-B
CaB
GB
B[B
@�B
B�B
A B
AUB
>wB
?HB
>�B
=qB
:*B
:*B
9�B
;0B
?�B
LdB
;dB
!B
"�B
!�B
'RB
�B
B
B
B
%B
	B
B	�B	��B
B	�cB	�B	��B	�B	�B	�8B	�2B	�BB	�B	��B	��B	�*B	��B	��B	�wB	��B	�?B	�!B	��B	�B	��B	��B	��B	�CB	�B	��B	�[B	�[B	��B	��B	��B	��B	��B	��B	�[B	�[B	�9B	��B	�?B	��B	�*B	�?B	��B	�OB	�B	�wB	��B	��B	�wB	��B	�OB	��B	��B	�IB	�qB	�wB	�wB	��B	�kB	��B	�B	�B	��B	��B	��B	�:B	��B	��B	�uB	��B	}VB	uZB	u�B	n/B	o5B	kB	k�B	dZB	hsB	d�B	\�B	XB	X�B	`�B	R�B	V9B	\)B	D�B	C�B	B�B	:�B	R�B	8B	4B	/�B	5?B	/�B	#B	"4B	%FB	,�B	4nB	,qB	IB	�B	
=B		�B	�B	YB	�B	�B	  B	 �B��B�B��B�B��B��B��B	#:B�>B�;B�]B�]B�HB��B��B�B��B��B�BیB�NBɆB�XBʌBŢB�B�[B�'B��B��B�dB��B��B��B��B��B�HB�qB��B��B�hB��B��B�<B�*B�BB�B�}B��B��B��B��B�:B��B��B�:B��B��B�\B��B��B��B��B�:B�B��B��B��B�qB��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021030508300920210305083009IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031509003620210315090036QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031509003620210315090036QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014520210427140145IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162931              CF      PSAL                            ?��G�O�D�s3G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163442    CF                  PSAL            G�O�?\)G�O�CG��G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                