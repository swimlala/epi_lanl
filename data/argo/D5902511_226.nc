CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  (   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-08-26T17:08:54Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     @  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  VH   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     @  \�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  u�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     @  |(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     @  Ԉ   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 P &�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     @ ,�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` F(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   F�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   L�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   R�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T X�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   X�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   X�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   X�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   X�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � X�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   Y|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   Y�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    Y�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        Y�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        Y�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       Y�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    Y�Argo profile    3.1 1.2 19500101000000  20220826170854  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_226                 6810_008521_226                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @���Kۡ@���Kۡ11  @���J#9�@���J#9�@2B�q�xB@2B�q�xB�d�s�����d�s����11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)@�\@B�\@�G�@��\@\@�G�A ��AG�A!G�A,(�A?\)A_\)A\)A��A�Q�A���A���A�Q�A�Q�A�  A��B(�BQ�B�
B (�B(Q�B/�
B7�B@  BH(�BP  BW�B_�Bh  Bo�
Bw�B�
B�  B�{B�  B�{B�(�B�(�B�(�B�  B��B��B��B�  B�  B��B��B�  B��B��B�  B�  B��B�  B�{B�{B�  B�  B�(�B�{B�  B�  B�  C   C��C
=C  C  C
  C��C��C  C��C  C
=C  C{C
=C  C   C"  C#��C%�C'��C*  C,  C.  C0
=C2
=C4
=C6  C8  C:  C<  C=��C@  CB  CD
=CF
=CH
=CJ  CL  CN
=CP  CR  CT  CV
=CW��CY��C[��C]��C`  Cb
=Cd  Ce�Ch  Cj{Cl  Cn  Cp  Cr  Ct  Cu��Cw�Cy��C|  C~
=C�  C�  C�C�C�C�C�
=C�C���C���C�C�  C���C�C�  C���C�  C�C�C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C���C�  C�
=C�C���C�  C�C�  C�  C�
=C�
=C�
=C�
=C�  C���C�  C���C�  C�C�  C�  C���C�  C���C���C���C���C�  C�
=C�
=C�  C���C��C���C���C���C���C�  C�C�
=C�  C���C���C�C�C�  C�  C���C���C�  C�  C���C���C���C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C���C�  C�  C���C�C�  C���C�  C�C�C�  C�  C�  C�  C�  C���C���C���C���C���C���C��C�  C�
=C�
=C�C�  C���C���C�  C�C�C�  D   D � D  D� D�qD� D�D� D  D� D  D� D  D� D  D��D�qD}qD	�D	��D
  D
}qD  D�D  D� D�D�D  D� DD��D�qD}qD  D��D  D� D�qD� D�D� D  D� D�qD}qD�qD}qD  D�D  D� D  Dz�D  D��D  D��DD��D  Dz�D�qD}qD �D �D!D!��D!�qD"� D#D#� D$  D$� D$��D%z�D&  D&� D&�qD'xRD'��D(� D)�D)� D*  D*��D+�D+� D,  D,��D-  D-� D.�D.��D/  D/� D0�D0��D1  D1}qD1��D2� D3�D3��D4  D4}qD5�D5� D5�qD6� D7�D7� D8  D8��D9  D9}qD9�qD:� D;�D;��D<  D<��D=  D=� D>�D>}qD>�qD?� D@  D@� D@�qDA}qDB  DB� DB�qDC� DD  DD}qDE  DE� DE�qDF}qDF�qDG��DH  DH}qDH�qDIz�DI�qDJ� DK  DK}qDK��DL� DM  DM� DN�DN� DO  DO��DP�DP��DQ  DQ}qDR  DR� DS  DS��DT  DT� DU  DU}qDV�DV��DW  DW��DX  DX� DX�qDY}qDZDZ��D[�D[��D\  D\}qD\��D]z�D]�qD^��D_  D_� D`�D`��D`�qDaz�Da�qDb}qDc  Dc��Dd  Dd}qDe�De��De�qDfxRDf��Dg}qDh  Dh� Di  Di� Dj  Dj� Dj�qDk}qDk�qDl}qDl�qDm}qDn  Dn� Do�Do��DpDp� Dq  Dq��Dr  Dr� DsDs� Dt�Dt� Du  Du��Dv�Dv��Dw�Dw}qDx  Dx� Dx�qDy��Dz�Dz� D{  D{}qD{�qD|� D}�D}��D~�D~��D�D��D�HD�@ D�~�D�� D�HD�AHD�� D���D�  D�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�� D���D��qD�@ D���D�� D���D�>�D�� D�� D�  D�>�D�~�D�� D�HD�B�D���D��HD���D�>�D�u�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>L��>�Q�?L��?�=q?�33?�ff@
=q@��@333@J=q@Y��@s33@��
@�{@��H@��@���@���@�ff@�\)@�p�@�ff@��A   Az�A��A��A�A��A ��A%�A(��A/\)A4z�A8Q�A>{AC�
AG
=AMp�AS33AVffA\(�Ab�\AfffAl(�As33Aw
=A|(�A���A��
A�ffA�=qA�z�A�\)A��HA���A�  A��A�A���A�(�A��RA���A��A�\)A�=qA�p�A��A��HA�ffA�Q�A��
A�ffAȣ�A�z�A�ffAљ�A��A�\)A�=qA�A�  A��HA�ffA���A�33A�
=A�G�A��
A�\)A��\A�z�B   Bp�B�\BQ�BB�HB��B
{B33B��B=qB\)B��B�RB�B��B�\B  BG�B�\BQ�B��B�HB ��B!�B#
=B$��B&=qB'\)B(��B*�\B+�B,��B.�\B0  B1�B2ffB4(�B5G�B6ffB8Q�B9p�B:�\B<(�B=B>�RB@(�BA�BC
=BD(�BE�BG
=BHQ�BJ{BK33BLQ�BN{BO�BPz�BR{BS�BT��BV=qBW�
BX��BZffB\  B]�B^ffB`(�Ba�Bb�\Bdz�Be��Bf�RBhz�Bi��Bk
=Bl��BmBn�HBp��BqBs33Bt��BuBw\)Bx��ByB{�B|��B}�B�B�ffB���B�B��\B�
=B��
B���B�G�B��B���B�G�B�(�B���B�\)B�(�B���B�G�B�{B���B�33B�  B�z�B���B���B�=qB��\B��B�B�  B���B��B�\)B��B�ffB���B�
=B���B�B�=qB��RB��B�\)B��B�Q�B�z�B�
=B��B��
B�(�B��RB�33B�\)B��B�ffB���B��B��B��B�=qB���B�G�B��B�  B��\B���B��B�B�{B�ffB���B�G�B��B��
B�z�B���B�
=B��B�  B�Q�B��RB�33B���B��
B�ffB���B�
=B��B�  B�Q�B��\B�
=B���B�B�Q�B���B�
=B�\)B��B�=qB�z�B��B��B�B�=qB���B�
=B�p�B�  B�=qB��\B��B���B�B�=qB���B���B�\)B��B�=qB�z�B�
=B��B��B�  B��\B��HB��B��B�(�B�ffB��RB�G�B���B��
B�ffB��HB��B���B�(�B�ffB��RB�\)B��
B�{B�ffB��HB�p�B�B�{B\B��B�\)B�B�ffBĸRB�
=B�p�B�{B�ffBƸRB�33BǮB�  B�z�B�
=B�G�BɮB�=qBʣ�B���B�\)B��B�z�B̸RB��B͙�B�(�B�z�B���B�\)B��B�(�BУ�B�33B�p�B��B�z�B��HB�33BӮB�=qBԣ�B��HB�p�B�  B�Q�BָRB�G�B��
B�{Bأ�B�33BمB��Bڏ\B�
=B�G�B��B�z�BܸRB�33B�B�=qB�z�B��Bߙ�B��
B�ffB���B�33BᙚB�(�B�RB�
=B�\)B��B�ffB��B�
=B�B�{B�ffB��HB�p�B��
B�(�B�RB�G�B�B�  B�\B��B�\)B�B�ffB��HB�\)B홚B�(�B�RB�33B�B��B��B�
=B�\)B��
B�z�B��HB�33B��
B�Q�B��RB�
=B��B�(�B�z�B��HB�p�B�  B�=qB���B�G�B���B�{B��RB�
=B�\)B��B�z�B��HB��B��B�Q�B���B���B��C 
=C 33C ffC �C �C{CG�C��CC�C33Cz�C��C�
C�CG�Cp�C�RC��C�CQ�C��C�
C��C(�Cz�C�RC�
C
=CG�C�\C�C�HC(�CQ�Cz�CC  C�CQ�C��C�
C	  C	(�C	z�C	�C	�
C
  C
G�C
�\C
�C
�HC(�CffC�\C�RC{CG�CffC��C�C�C=qCp�CC��C{CQ�C��CC��C33Cz�C��C��C{CQ�Cp�C��C�C�CG�Cz�C�RC��C�C=qC�CC��C{CQ�C�\CC�HC(�Cp�C�\C�RC  C=qCz�C�\C��C
=CG�Cz�C��CC
=CG�Cp�C��C�
C�C=qCffC�C�C{C=qC�CC��C�CG�C��CC�C�CffC�\C�RC�C33CffC�C�C��C33CffC�C�C  C33CQ�C�CC��C{C=qCp�C�RC�C �C 33C ffC ��C �HC ��C!�C!\)C!��C!�RC!�
C"�C"Q�C"p�C"�\C"��C#{C#33C#Q�C#��C#�HC$
=C$(�C$ffC$��C$�HC%  C%33C%ffC%�C%��C&�C&G�C&�C&C'
=C'33C'\)C'��C'�HC(�C(G�C(p�C(��C(�HC)(�C)p�C)��C)��C*  C*G�C*�\C*C*�C+(�C+p�C+��C+��C,{C,\)C,�C,�C,�C-33C-ffC-�\C-C.  C.G�C.p�C.��C.�
C/{C/\)C/�\C/�RC/�C033C0z�C0��C0�
C1
=C1Q�C1��C1C1��C233C2z�C2C2�C3{C3G�C3�\C3�
C3��C4(�C4ffC4�RC4�HC5
=C5G�C5�\C5�
C6{C6=qC6ffC6�C6��C733C7p�C7��C7��C8
=C8Q�C8�\C8C8�C9{C9G�C9�\C9C:
=C:G�C:z�C:�C:�HC;{C;Q�C;��C;�
C<  C<33C<ffC<�C<��C=(�C=Q�C=�C=�C=�HC>(�C>ffC>��C>�HC?{C?Q�C?z�C?�C?�C@33C@z�C@�C@�
CA  CA=qCAz�CACB  CB33CBffCB��CB��CC  CCG�CC�\CC��CD{CDQ�CD�CD�RCD��CE(�CEz�CECF
=CFG�CF�CFCF��CG(�CGp�CG�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                            1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�\)@�\@B�\@�G�@��\@\@�G�A ��AG�A!G�A,(�A?\)A_\)A\)A��A�Q�A���A���A�Q�A�Q�A�  A��B(�BQ�B�
B (�B(Q�B/�
B7�B@  BH(�BP  BW�B_�Bh  Bo�
Bw�B�
B�  B�{B�  B�{B�(�B�(�B�(�B�  B��B��B��B�  B�  B��B��B�  B��B��B�  B�  B��B�  B�{B�{B�  B�  B�(�B�{B�  B�  B�  C   C��C
=C  C  C
  C��C��C  C��C  C
=C  C{C
=C  C   C"  C#��C%�C'��C*  C,  C.  C0
=C2
=C4
=C6  C8  C:  C<  C=��C@  CB  CD
=CF
=CH
=CJ  CL  CN
=CP  CR  CT  CV
=CW��CY��C[��C]��C`  Cb
=Cd  Ce�Ch  Cj{Cl  Cn  Cp  Cr  Ct  Cu��Cw�Cy��C|  C~
=C�  C�  C�C�C�C�C�
=C�C���C���C�C�  C���C�C�  C���C�  C�C�C�  C�  C�  C���C���C�  C�  C�  C���C���C�  C���C�  C�
=C�C���C�  C�C�  C�  C�
=C�
=C�
=C�
=C�  C���C�  C���C�  C�C�  C�  C���C�  C���C���C���C���C�  C�
=C�
=C�  C���C��C���C���C���C���C�  C�C�
=C�  C���C���C�C�C�  C�  C���C���C�  C�  C���C���C���C�  C�  C���C�  C�  C���C�C�  C�  C�  C�  C���C�  C�  C���C�C�  C���C�  C�C�C�  C�  C�  C�  C�  C���C���C���C���C���C���C��C�  C�
=C�
=C�C�  C���C���C�  C�C�C�  D   D � D  D� D�qD� D�D� D  D� D  D� D  D� D  D��D�qD}qD	�D	��D
  D
}qD  D�D  D� D�D�D  D� DD��D�qD}qD  D��D  D� D�qD� D�D� D  D� D�qD}qD�qD}qD  D�D  D� D  Dz�D  D��D  D��DD��D  Dz�D�qD}qD �D �D!D!��D!�qD"� D#D#� D$  D$� D$��D%z�D&  D&� D&�qD'xRD'��D(� D)�D)� D*  D*��D+�D+� D,  D,��D-  D-� D.�D.��D/  D/� D0�D0��D1  D1}qD1��D2� D3�D3��D4  D4}qD5�D5� D5�qD6� D7�D7� D8  D8��D9  D9}qD9�qD:� D;�D;��D<  D<��D=  D=� D>�D>}qD>�qD?� D@  D@� D@�qDA}qDB  DB� DB�qDC� DD  DD}qDE  DE� DE�qDF}qDF�qDG��DH  DH}qDH�qDIz�DI�qDJ� DK  DK}qDK��DL� DM  DM� DN�DN� DO  DO��DP�DP��DQ  DQ}qDR  DR� DS  DS��DT  DT� DU  DU}qDV�DV��DW  DW��DX  DX� DX�qDY}qDZDZ��D[�D[��D\  D\}qD\��D]z�D]�qD^��D_  D_� D`�D`��D`�qDaz�Da�qDb}qDc  Dc��Dd  Dd}qDe�De��De�qDfxRDf��Dg}qDh  Dh� Di  Di� Dj  Dj� Dj�qDk}qDk�qDl}qDl�qDm}qDn  Dn� Do�Do��DpDp� Dq  Dq��Dr  Dr� DsDs� Dt�Dt� Du  Du��Dv�Dv��Dw�Dw}qDx  Dx� Dx�qDy��Dz�Dz� D{  D{}qD{�qD|� D}�D}��D~�D~��D�D��D�HD�@ D�~�D�� D�HD�AHD�� D���D�  D�@ D�~�D���D�  D�>�D�� D�� D�  D�@ D�� D���D��qD�@ D���D�� D���D�>�D�� D�� D�  D�>�D�~�D�� D�HD�B�D���D��HD���D�>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>L��>�Q�?L��?�=q?�33?�ff@
=q@��@333@J=q@Y��@s33@��
@�{@��H@��@���@���@�ff@�\)@�p�@�ff@��A   Az�A��A��A�A��A ��A%�A(��A/\)A4z�A8Q�A>{AC�
AG
=AMp�AS33AVffA\(�Ab�\AfffAl(�As33Aw
=A|(�A���A��
A�ffA�=qA�z�A�\)A��HA���A�  A��A�A���A�(�A��RA���A��A�\)A�=qA�p�A��A��HA�ffA�Q�A��
A�ffAȣ�A�z�A�ffAљ�A��A�\)A�=qA�A�  A��HA�ffA���A�33A�
=A�G�A��
A�\)A��\A�z�B   Bp�B�\BQ�BB�HB��B
{B33B��B=qB\)B��B�RB�B��B�\B  BG�B�\BQ�B��B�HB ��B!�B#
=B$��B&=qB'\)B(��B*�\B+�B,��B.�\B0  B1�B2ffB4(�B5G�B6ffB8Q�B9p�B:�\B<(�B=B>�RB@(�BA�BC
=BD(�BE�BG
=BHQ�BJ{BK33BLQ�BN{BO�BPz�BR{BS�BT��BV=qBW�
BX��BZffB\  B]�B^ffB`(�Ba�Bb�\Bdz�Be��Bf�RBhz�Bi��Bk
=Bl��BmBn�HBp��BqBs33Bt��BuBw\)Bx��ByB{�B|��B}�B�B�ffB���B�B��\B�
=B��
B���B�G�B��B���B�G�B�(�B���B�\)B�(�B���B�G�B�{B���B�33B�  B�z�B���B���B�=qB��\B��B�B�  B���B��B�\)B��B�ffB���B�
=B���B�B�=qB��RB��B�\)B��B�Q�B�z�B�
=B��B��
B�(�B��RB�33B�\)B��B�ffB���B��B��B��B�=qB���B�G�B��B�  B��\B���B��B�B�{B�ffB���B�G�B��B��
B�z�B���B�
=B��B�  B�Q�B��RB�33B���B��
B�ffB���B�
=B��B�  B�Q�B��\B�
=B���B�B�Q�B���B�
=B�\)B��B�=qB�z�B��B��B�B�=qB���B�
=B�p�B�  B�=qB��\B��B���B�B�=qB���B���B�\)B��B�=qB�z�B�
=B��B��B�  B��\B��HB��B��B�(�B�ffB��RB�G�B���B��
B�ffB��HB��B���B�(�B�ffB��RB�\)B��
B�{B�ffB��HB�p�B�B�{B\B��B�\)B�B�ffBĸRB�
=B�p�B�{B�ffBƸRB�33BǮB�  B�z�B�
=B�G�BɮB�=qBʣ�B���B�\)B��B�z�B̸RB��B͙�B�(�B�z�B���B�\)B��B�(�BУ�B�33B�p�B��B�z�B��HB�33BӮB�=qBԣ�B��HB�p�B�  B�Q�BָRB�G�B��
B�{Bأ�B�33BمB��Bڏ\B�
=B�G�B��B�z�BܸRB�33B�B�=qB�z�B��Bߙ�B��
B�ffB���B�33BᙚB�(�B�RB�
=B�\)B��B�ffB��B�
=B�B�{B�ffB��HB�p�B��
B�(�B�RB�G�B�B�  B�\B��B�\)B�B�ffB��HB�\)B홚B�(�B�RB�33B�B��B��B�
=B�\)B��
B�z�B��HB�33B��
B�Q�B��RB�
=B��B�(�B�z�B��HB�p�B�  B�=qB���B�G�B���B�{B��RB�
=B�\)B��B�z�B��HB��B��B�Q�B���B���B��C 
=C 33C ffC �C �C{CG�C��CC�C33Cz�C��C�
C�CG�Cp�C�RC��C�CQ�C��C�
C��C(�Cz�C�RC�
C
=CG�C�\C�C�HC(�CQ�Cz�CC  C�CQ�C��C�
C	  C	(�C	z�C	�C	�
C
  C
G�C
�\C
�C
�HC(�CffC�\C�RC{CG�CffC��C�C�C=qCp�CC��C{CQ�C��CC��C33Cz�C��C��C{CQ�Cp�C��C�C�CG�Cz�C�RC��C�C=qC�CC��C{CQ�C�\CC�HC(�Cp�C�\C�RC  C=qCz�C�\C��C
=CG�Cz�C��CC
=CG�Cp�C��C�
C�C=qCffC�C�C{C=qC�CC��C�CG�C��CC�C�CffC�\C�RC�C33CffC�C�C��C33CffC�C�C  C33CQ�C�CC��C{C=qCp�C�RC�C �C 33C ffC ��C �HC ��C!�C!\)C!��C!�RC!�
C"�C"Q�C"p�C"�\C"��C#{C#33C#Q�C#��C#�HC$
=C$(�C$ffC$��C$�HC%  C%33C%ffC%�C%��C&�C&G�C&�C&C'
=C'33C'\)C'��C'�HC(�C(G�C(p�C(��C(�HC)(�C)p�C)��C)��C*  C*G�C*�\C*C*�C+(�C+p�C+��C+��C,{C,\)C,�C,�C,�C-33C-ffC-�\C-C.  C.G�C.p�C.��C.�
C/{C/\)C/�\C/�RC/�C033C0z�C0��C0�
C1
=C1Q�C1��C1C1��C233C2z�C2C2�C3{C3G�C3�\C3�
C3��C4(�C4ffC4�RC4�HC5
=C5G�C5�\C5�
C6{C6=qC6ffC6�C6��C733C7p�C7��C7��C8
=C8Q�C8�\C8C8�C9{C9G�C9�\C9C:
=C:G�C:z�C:�C:�HC;{C;Q�C;��C;�
C<  C<33C<ffC<�C<��C=(�C=Q�C=�C=�C=�HC>(�C>ffC>��C>�HC?{C?Q�C?z�C?�C?�C@33C@z�C@�C@�
CA  CA=qCAz�CACB  CB33CBffCB��CB��CC  CCG�CC�\CC��CD{CDQ�CD�CD�RCD��CE(�CEz�CECF
=CFG�CF�CFCF��CG(�CGp�CG�R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                            1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�A�A�%A�A���A�JA�oA�{A�{A�oA�bA�VA�{A��;A�n�A��`A���A�AټjAٮA١�AّhAًDAه+A�r�A�ZA�O�A�C�A�bA�ffA��A׉7A�VA�A�Aպ^A��AԮA�bNA�-A�K�A�  AύPA��Aβ-A·+A�XA��A�|�A�VA��
A�ƨȀ\A��yAˡ�A�t�A�C�A��A���A��TA�ƨAʩ�A�`BA�C�A�oA���AɾwAɼjA�ĜA���Aɩ�A�E�A��A���Aȣ�A�S�A�A��;A��/Aǲ-A�(�AƼjA��A��Aħ�A�l�A�-A���AA�&�A�jA���A�;dA��A�ĜA�v�A�5?A���A�C�A�VA�C�A��A���A�n�A�1A�l�A�bA���A�;dA��mA��A�5?A���A�M�A���A��9A�  A�G�A�E�A���A�
=A���A�5?A�/A���A��wA��9A�ZA�A�5?A���A�(�A�7LA���A�ffA�=qA��#A`BA~=qA|�DAv��At^5Aq�Ap^5An��Am�Aj��Ag�
Ae��Ac�hA`�9A_7LA]�wA\=qA[C�AZ�\AY��AX �AUAS&�AQ�-AO�TAJ�jAIVAH-AFVAC��A?��A<A�A:�+A:-A9?}A7��A7p�A6�A6�+A6(�A4�\A2��A0�\A/dZA-�;A,A�A*�jA'��A&��A&JA$��A#�7A"�yA"jA!�A!�A �A��A+A��A��A�hA��AO�A��A�-A��A��A�mA`BA  A33AoA�HA(�A��A
�A
ZA	;dA��A�!A��AI�A7LAp�A ��@���@���@�@��@��H@���@�33@�Q�@�9@�~�@畁@��H@�S�@�h@�K�@�z�@���@ޟ�@ߍP@�ƨ@�S�@�K�@�ȴ@�=q@��@�hs@�b@��#@��@�S�@ߍP@�ƨ@ߕ�@�|�@��@��@ܓu@ܴ9@�1@�
=@�@�hs@ՙ�@ԣ�@��;@�t�@�@ҸR@�~�@�5?@��@��@Ѳ-@��@д9@�z�@�j@��m@�p�@�V@���@̛�@��
@˕�@�\)@�
=@ʸR@�5?@ɡ�@�V@ȼj@�bN@�Z@�I�@�1@��@��;@Ǖ�@�o@�@��@�~�@ŉ7@�1'@�ƨ@�|�@�33@�
=@�ff@��#@��/@�bN@� �@��@�b@���@��y@��\@�V@��-@��-@��^@��7@�%@�r�@�Z@�9X@� �@��w@�K�@���@���@�=q@��T@���@��-@���@��7@��@��@�X@�z�@�1'@�|�@�=q@���@�x�@�V@�j@��@�ƨ@�l�@�~�@�-@�J@��@���@��-@��h@���@��u@�Q�@�1@�ƨ@�t�@�S�@��@�33@�"�@��@��!@�-@���@��-@��7@�V@���@�j@�  @��@�"�@�ff@�$�@�{@���@���@��9@���@�I�@��m@��;@���@�S�@�33@��@���@�v�@�$�@��#@���@��#@���@�X@���@��`@���@���@��P@�l�@�\)@�C�@��@��!@���@��\@�$�@��#@���@�/@���@�(�@�ƨ@�l�@�33@��@�ff@���@�`B@���@���@��@��@��
@�ƨ@�ƨ@��P@�C�@�o@�@�o@�o@��@��@�o@�
=@�ȴ@���@�n�@�-@��^@��@�G�@�V@���@��u@�j@�b@�dZ@�33@���@���@���@�^5@�=q@�J@��@�@�x�@���@���@��j@��@��D@�z�@�I�@��;@�|�@�C�@�@��R@�~�@�M�@�{@��@���@���@��@�O�@�%@��/@��9@��@�r�@� �@�  @��
@���@�K�@�33@��@���@���@��\@�n�@�$�@���@��7@��@��/@���@�j@�b@��;@���@���@�S�@�;d@�o@��H@���@��R@��!@�~�@�=q@��@��^@���@�`B@��`@�r�@�A�@�(�@�b@��@�C�@���@�E�@��@��T@�@��^@��^@��7@�X@�?}@��@���@���@��D@�I�@�1'@� �@�1@��mG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�A�A�%A�A�%A�%A�1A�1A�A���A�A���A���A�%A�oA�oA��A�bA�oA��A�oA�oA��A�oA�bA��A�oA�bA�oA�oA�
=A�
=A�bA�JA�bA��A�bA�bA��A�{A�{A��A�%A��HA��#Aں^AڬAڰ!Aک�AڋDA�\)A���A��A��A��mA��`A��`A��#A��
A���A���A���A���A�ĜA�ĜA�ƨAپwA�AپwAټjAپwAٺ^Aٺ^AټjAٴ9Aٲ-Aٲ-AٮA٩�AٮA٬A٣�A٥�Aٟ�Aٙ�Aٛ�Aٗ�Aُ\AٓuAُ\AًDAّhAُ\Aه+AًDAًDAًDAٍPAٍPAه+AمAمA�~�A�x�A�z�A�t�A�jA�hsA�ffA�^5A�ZA�\)A�ZA�Q�A�S�A�S�A�M�A�M�A�O�A�K�A�G�A�I�A�C�A�?}A�=qA�=qA�1'A�&�A��A�1A���Aا�A؟�A�x�A�VA�=qA�-A��A�1A���A��A��mA��#A�ȴAק�Aי�A׋DA�`BA�?}A�-A�&�A�bA�A��A���A�dZA�G�A�;dA��A�  A���A��mA���A�Aՙ�A�t�A�dZA�K�A�1'A�VA��HAԾwAԼjAԴ9AԴ9AԬAԛ�Aԟ�Aԙ�AԍPA�n�A�K�A�1'A�oA���A�z�A�bA��yA���AҬA�dZAѝ�A��AЮA�~�A�t�A�O�A� �A��`A���AϼjAϸRAϰ!Aϝ�Aϙ�AϏ\A�|�A�t�A�r�A�dZA�dZA�E�A�/A�"�A��A�
=A��A��`A��A���A���A�ƨAκ^AζFAάAΣ�AΡ�AΡ�AΝ�AΛ�AΝ�AΛ�AΗ�AΗ�A΅A�r�A�n�A�n�A�hsA�ffA�hsA�dZA�`BA�\)A�XA�I�A�A�A�?}A�7LA�-A� �A�JA��A���A���A�AͼjA͸RAͲ-Aͥ�A͙�A͓uA͉7A�x�A�p�A�ffA�ZA�S�A�K�A�=qA�5?A�-A��A�A�  A���A��A��A��A��mA��/A��#A��A���A���A���A���A���A���A���A�ƨA���A�ȴA�A�ĜA�ƨA���A̼jA�A̾wA̼jA̾wA̾wA̰!A̕�A̅A�p�A�VA�"�A���A���A��A��A��A��yA��mA��TA��#A���A���A�ĜA˸RA˴9AˮA˗�Aˉ7A˅A˃AˁA�z�A�v�A�r�A�n�A�p�A�x�A�v�A�t�A�t�A�r�A�hsA�Q�A�M�A�E�A�7LA�5?A�5?A�/A�&�A�&�A� �A��A��A��A��A�{A�bA�VA�bA�%A�  A�  A���A���A���A���A��A��A���A��A��yA��yA��TA��#A���A���A���A�ȴA�ĜA�ȴA���A�ƨA�ȴA�ȴA�AʾwAʾwAʺ^Aʲ-Aʰ!AʬAʡ�Aʙ�AʍPAʁA�t�A�n�A�dZA�\)A�ZA�XA�O�A�I�A�K�A�K�A�G�A�E�A�G�A�G�A�?}A�7LA�7LA�33A�(�A�$�A� �A��A�bA�
=A�A��A��mA��mA��`A��A�ȴA�ĜA�ƨA�ƨA�ĜA�ƨA�ȴA�ĜAɾwAɼjAɼjAɺ^Aɴ9Aɴ9Aɺ^Aɺ^AɶFAɼjA���A���AɼjA�A�ĜA�A���A�ĜA�ƨA�A�ƨA�ȴA�ĜA�ƨA���A���A���A���A���A���A���A���A���A�AɶFAɴ9Aɲ-Aɩ�AɓuAɍPAɅA�t�A�jA�^5A�E�A�33A�1'A�/A�&�A�(�A�+A�(�A�$�A�"�A� �A��A�bA�%A���A��A��TA��/A��A���A���A���A���A�AȾwAȾwAȴ9AȬAȮAȬAȣ�Aț�Aȗ�AȋDA�r�A�ffA�dZA�ZA�M�A�I�A�M�A�O�A�I�A�?}A�/A��A�
=A�A�A���A��yA��yA��yA��TA��/A��/A��HA��;A��#A��/A��HA��/A��#A��#A��;A��/A��#A��/A��;A��#A��A��A���A�ĜAǼjA���A���AǶFAǩ�AǗ�AǓuA�t�A�\)A�Q�A�O�A�G�A�5?A��A�JA���A��A��TA��/A��A���A�AƼjAƴ9AƬAƣ�Aƛ�AƋDA�x�A�S�A�1'A�&�A�VA�A��A���AŶFAŕ�A�r�A�O�A�/A��A�{A�  A���A��TA���AļjAľwAĲ-Aĩ�Aģ�Aħ�Aģ�Aĝ�Aę�AčPAāA�z�A�r�A�jA�jA�jA�dZA�bNA�dZA�`BA�S�A�K�A�A�A�7LA�-A�(�A�"�A��A��A�oA�  A��A��A��yA��;A��
A���A���A�Aú^AÑhA�`BA�/A��A���A§�A�bNA�E�A��A���A��HA��\A�l�A�VA�I�A�=qA��A���A��;A�A��A��hA��+A�|�A�t�A�n�A�bNA�M�A�33A�"�A��A�  A���A���A�t�A�ZA�Q�A�I�A�C�A�C�A�E�A�C�A�;dA�;dA�;dA�5?A�/A�/A�1'A�/A�-A�&�A��A��A�A�  A���A���A��A��
A�ȴA���A��wA��9A��A���A���A���A��hA��A�v�A�n�A�^5A�`BA�ZA�S�A�K�A�G�A�E�A�33A�1'A�(�A�(�A��A�  A��A��HA�A���A��A�x�A�hsA�^5A�XA�O�A�G�A�C�A�A�A�=qA�9XA�9XA�1'A�+A�&�A�$�A� �A��A��A�VA���A�ĜA�M�A��A��
A��RA�ZA���A���A��A�ZA�-A�"�A��A��A�
=A���A��yA���A�`BA��A��A���A���A��A�l�A�XA�E�A��A���A��A��jA���A��A�bNA�=qA�%A���A���A�l�A�VA�%A�~�A���A���A�$�A�n�A��A��yA���A�M�A�5?A�bA���A���A��hA�ffA�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                            1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�A�%A�A���A�JA�oA�{A�{A�oA�bA�VA�{A��;A�n�A��`A���A�AټjAٮA١�AّhAًDAه+A�r�A�ZA�O�A�C�A�bA�ffA��A׉7A�VA�A�Aպ^A��AԮA�bNA�-A�K�A�  AύPA��Aβ-A·+A�XA��A�|�A�VA��
A�ƨȀ\A��yAˡ�A�t�A�C�A��A���A��TA�ƨAʩ�A�`BA�C�A�oA���AɾwAɼjA�ĜA���Aɩ�A�E�A��A���Aȣ�A�S�A�A��;A��/Aǲ-A�(�AƼjA��A��Aħ�A�l�A�-A���AA�&�A�jA���A�;dA��A�ĜA�v�A�5?A���A�C�A�VA�C�A��A���A�n�A�1A�l�A�bA���A�;dA��mA��A�5?A���A�M�A���A��9A�  A�G�A�E�A���A�
=A���A�5?A�/A���A��wA��9A�ZA�A�5?A���A�(�A�7LA���A�ffA�=qA��#A`BA~=qA|�DAv��At^5Aq�Ap^5An��Am�Aj��Ag�
Ae��Ac�hA`�9A_7LA]�wA\=qA[C�AZ�\AY��AX �AUAS&�AQ�-AO�TAJ�jAIVAH-AFVAC��A?��A<A�A:�+A:-A9?}A7��A7p�A6�A6�+A6(�A4�\A2��A0�\A/dZA-�;A,A�A*�jA'��A&��A&JA$��A#�7A"�yA"jA!�A!�A �A��A+A��A��A�hA��AO�A��A�-A��A��A�mA`BA  A33AoA�HA(�A��A
�A
ZA	;dA��A�!A��AI�A7LAp�A ��@���@���@�@��@��H@���@�33@�Q�@�9@�~�@畁@��H@�S�@�h@�K�@�z�@���@ޟ�@ߍP@�ƨ@�S�@�K�@�ȴ@�=q@��@�hs@�b@��#@��@�S�@ߍP@�ƨ@ߕ�@�|�@��@��@ܓu@ܴ9@�1@�
=@�@�hs@ՙ�@ԣ�@��;@�t�@�@ҸR@�~�@�5?@��@��@Ѳ-@��@д9@�z�@�j@��m@�p�@�V@���@̛�@��
@˕�@�\)@�
=@ʸR@�5?@ɡ�@�V@ȼj@�bN@�Z@�I�@�1@��@��;@Ǖ�@�o@�@��@�~�@ŉ7@�1'@�ƨ@�|�@�33@�
=@�ff@��#@��/@�bN@� �@��@�b@���@��y@��\@�V@��-@��-@��^@��7@�%@�r�@�Z@�9X@� �@��w@�K�@���@���@�=q@��T@���@��-@���@��7@��@��@�X@�z�@�1'@�|�@�=q@���@�x�@�V@�j@��@�ƨ@�l�@�~�@�-@�J@��@���@��-@��h@���@��u@�Q�@�1@�ƨ@�t�@�S�@��@�33@�"�@��@��!@�-@���@��-@��7@�V@���@�j@�  @��@�"�@�ff@�$�@�{@���@���@��9@���@�I�@��m@��;@���@�S�@�33@��@���@�v�@�$�@��#@���@��#@���@�X@���@��`@���@���@��P@�l�@�\)@�C�@��@��!@���@��\@�$�@��#@���@�/@���@�(�@�ƨ@�l�@�33@��@�ff@���@�`B@���@���@��@��@��
@�ƨ@�ƨ@��P@�C�@�o@�@�o@�o@��@��@�o@�
=@�ȴ@���@�n�@�-@��^@��@�G�@�V@���@��u@�j@�b@�dZ@�33@���@���@���@�^5@�=q@�J@��@�@�x�@���@���@��j@��@��D@�z�@�I�@��;@�|�@�C�@�@��R@�~�@�M�@�{@��@���@���@��@�O�@�%@��/@��9@��@�r�@� �@�  @��
@���@�K�@�33@��@���@���@��\@�n�@�$�@���@��7@��@��/@���@�j@�b@��;@���@���@�S�@�;d@�o@��H@���@��R@��!@�~�@�=q@��@��^@���@�`B@��`@�r�@�A�@�(�@�b@��@�C�@���@�E�@��@��T@�@��^@��^@��7@�X@�?}@��@���@���@��D@�I�@�1'@� �@�1G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�A�A�A�A�A�%A�A�%A�%A�1A�1A�A���A�A���A���A�%A�oA�oA��A�bA�oA��A�oA�oA��A�oA�bA��A�oA�bA�oA�oA�
=A�
=A�bA�JA�bA��A�bA�bA��A�{A�{A��A�%A��HA��#Aں^AڬAڰ!Aک�AڋDA�\)A���A��A��A��mA��`A��`A��#A��
A���A���A���A���A�ĜA�ĜA�ƨAپwA�AپwAټjAپwAٺ^Aٺ^AټjAٴ9Aٲ-Aٲ-AٮA٩�AٮA٬A٣�A٥�Aٟ�Aٙ�Aٛ�Aٗ�Aُ\AٓuAُ\AًDAّhAُ\Aه+AًDAًDAًDAٍPAٍPAه+AمAمA�~�A�x�A�z�A�t�A�jA�hsA�ffA�^5A�ZA�\)A�ZA�Q�A�S�A�S�A�M�A�M�A�O�A�K�A�G�A�I�A�C�A�?}A�=qA�=qA�1'A�&�A��A�1A���Aا�A؟�A�x�A�VA�=qA�-A��A�1A���A��A��mA��#A�ȴAק�Aי�A׋DA�`BA�?}A�-A�&�A�bA�A��A���A�dZA�G�A�;dA��A�  A���A��mA���A�Aՙ�A�t�A�dZA�K�A�1'A�VA��HAԾwAԼjAԴ9AԴ9AԬAԛ�Aԟ�Aԙ�AԍPA�n�A�K�A�1'A�oA���A�z�A�bA��yA���AҬA�dZAѝ�A��AЮA�~�A�t�A�O�A� �A��`A���AϼjAϸRAϰ!Aϝ�Aϙ�AϏ\A�|�A�t�A�r�A�dZA�dZA�E�A�/A�"�A��A�
=A��A��`A��A���A���A�ƨAκ^AζFAάAΣ�AΡ�AΡ�AΝ�AΛ�AΝ�AΛ�AΗ�AΗ�A΅A�r�A�n�A�n�A�hsA�ffA�hsA�dZA�`BA�\)A�XA�I�A�A�A�?}A�7LA�-A� �A�JA��A���A���A�AͼjA͸RAͲ-Aͥ�A͙�A͓uA͉7A�x�A�p�A�ffA�ZA�S�A�K�A�=qA�5?A�-A��A�A�  A���A��A��A��A��mA��/A��#A��A���A���A���A���A���A���A���A�ƨA���A�ȴA�A�ĜA�ƨA���A̼jA�A̾wA̼jA̾wA̾wA̰!A̕�A̅A�p�A�VA�"�A���A���A��A��A��A��yA��mA��TA��#A���A���A�ĜA˸RA˴9AˮA˗�Aˉ7A˅A˃AˁA�z�A�v�A�r�A�n�A�p�A�x�A�v�A�t�A�t�A�r�A�hsA�Q�A�M�A�E�A�7LA�5?A�5?A�/A�&�A�&�A� �A��A��A��A��A�{A�bA�VA�bA�%A�  A�  A���A���A���A���A��A��A���A��A��yA��yA��TA��#A���A���A���A�ȴA�ĜA�ȴA���A�ƨA�ȴA�ȴA�AʾwAʾwAʺ^Aʲ-Aʰ!AʬAʡ�Aʙ�AʍPAʁA�t�A�n�A�dZA�\)A�ZA�XA�O�A�I�A�K�A�K�A�G�A�E�A�G�A�G�A�?}A�7LA�7LA�33A�(�A�$�A� �A��A�bA�
=A�A��A��mA��mA��`A��A�ȴA�ĜA�ƨA�ƨA�ĜA�ƨA�ȴA�ĜAɾwAɼjAɼjAɺ^Aɴ9Aɴ9Aɺ^Aɺ^AɶFAɼjA���A���AɼjA�A�ĜA�A���A�ĜA�ƨA�A�ƨA�ȴA�ĜA�ƨA���A���A���A���A���A���A���A���A���A�AɶFAɴ9Aɲ-Aɩ�AɓuAɍPAɅA�t�A�jA�^5A�E�A�33A�1'A�/A�&�A�(�A�+A�(�A�$�A�"�A� �A��A�bA�%A���A��A��TA��/A��A���A���A���A���A�AȾwAȾwAȴ9AȬAȮAȬAȣ�Aț�Aȗ�AȋDA�r�A�ffA�dZA�ZA�M�A�I�A�M�A�O�A�I�A�?}A�/A��A�
=A�A�A���A��yA��yA��yA��TA��/A��/A��HA��;A��#A��/A��HA��/A��#A��#A��;A��/A��#A��/A��;A��#A��A��A���A�ĜAǼjA���A���AǶFAǩ�AǗ�AǓuA�t�A�\)A�Q�A�O�A�G�A�5?A��A�JA���A��A��TA��/A��A���A�AƼjAƴ9AƬAƣ�Aƛ�AƋDA�x�A�S�A�1'A�&�A�VA�A��A���AŶFAŕ�A�r�A�O�A�/A��A�{A�  A���A��TA���AļjAľwAĲ-Aĩ�Aģ�Aħ�Aģ�Aĝ�Aę�AčPAāA�z�A�r�A�jA�jA�jA�dZA�bNA�dZA�`BA�S�A�K�A�A�A�7LA�-A�(�A�"�A��A��A�oA�  A��A��A��yA��;A��
A���A���A�Aú^AÑhA�`BA�/A��A���A§�A�bNA�E�A��A���A��HA��\A�l�A�VA�I�A�=qA��A���A��;A�A��A��hA��+A�|�A�t�A�n�A�bNA�M�A�33A�"�A��A�  A���A���A�t�A�ZA�Q�A�I�A�C�A�C�A�E�A�C�A�;dA�;dA�;dA�5?A�/A�/A�1'A�/A�-A�&�A��A��A�A�  A���A���A��A��
A�ȴA���A��wA��9A��A���A���A���A��hA��A�v�A�n�A�^5A�`BA�ZA�S�A�K�A�G�A�E�A�33A�1'A�(�A�(�A��A�  A��A��HA�A���A��A�x�A�hsA�^5A�XA�O�A�G�A�C�A�A�A�=qA�9XA�9XA�1'A�+A�&�A�$�A� �A��A��A�VA���A�ĜA�M�A��A��
A��RA�ZA���A���A��A�ZA�-A�"�A��A��A�
=A���A��yA���A�`BA��A��A���A���A��A�l�A�XA�E�A��A���A��A��jA���A��A�bNA�=qA�%A���A���A�l�A�VA�%A�~�A���A���A�$�A�n�A��A��yA���A�M�A�5?A�bA���A���A��hA�ffA�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                            1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B
��B
��B
��B
�oB
�B
��B
�hB
�hB
�4B
�4B
��B
�4B
�.B
�(B
��B
��B
��B
��B
��B
��B
�MB
�uB
��B
�AB
�B
~]B
}"B
|B
|B
v�B
sMB
sMB
o�B
m�B
g�B
f�B
a�B
aB
^5B
ZQB
K�B
F�B
F�B
E�B
D�B
C�B
C-B
B�B
FB
GzB
G�B
J�B
QB
ZQB
`B
b�B
`vB
a�B
d&B
l"B
t�B
~]B
��B
��B
�nB
�OB
�0B
�B
�3B
ܒB
�B
�/B
�rB
�VB�B�BeBqB-CB<6BD3BXEBu%B~]B��B��B��B�'B��B�'BбB��B��B�B�B��B��BB�B.�B/�B9�B7LB@�B2-B*�B'BOB B�BGB�B�cB�WB�B�&B�<B�zB��Bx8BF�B%�B�B
	B;B
��B
��B
��B
��B
�B
�'B
��B
�B
kQB
YB
PB
.}B
!�B
+B
oB	�B	רB	�0B	ÖB	��B	��B	�B	��B	��B	tTB	p�B	h�B	_B	XEB	T�B	PHB	K)B	A�B	6�B	,B	+B	$B	�B	MB�xB��B�WB�B�jB�B�pB��B��B�QB��B�sB�]B�|B�B�&B�B��BیB�2B��B�B��B��B�gB��B�#B��BޞB�5B�B�VB�DB�B��B�B�B��BÖB�B�B�UB�FB��B�IB�=B��B�aB��B�UB��B�B�B��B��B�0B��B��B��B�B��B��B��B�B��B��B��B��B��B��B��B�EB��B�dBǮB�EB�^B��B�B��B��B�B�B�B��B��B	fB	JB	PB	�B	�B	qB	#B	&�B	$�B	,B	,�B	.B	-�B	.�B	/B	#�B	*0B	,B	-�B	.�B	0!B	1�B	2aB	2�B	49B	7�B	<6B	>�B	>�B	C�B	J�B	J#B	L�B	O�B	T,B	UgB	W
B	XEB	Z�B	`vB	e�B	kB	m�B	pB	p;B	qAB	sMB	sMB	s�B	uZB	w2B	w2B	xB	zB	~�B	�SB	��B	�_B	��B	�1B	��B	�PB	��B	��B	��B	��B	�YB	�	B	�OB	��B	�B	�0B	��B	�=B	�B	��B	��B	�'B	�3B	�3B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ÖB	�3B	�gB	ƨB	�B	��B	ΥB	��B	��B	ѷB	҉B	�2B	�?B	�sB	�EB	�B	�B	�B	�B	�B	�#B	�]B	�jB	�;B	�B	��B	�B	��B	�yB	�B	��B	��B	� B	��B	�B	�|B	�TB	��B	��B	��B	�fB	�+B	�`B	�2B	�+B	�ZB	��B	�8B	��B	�B	��B	�]B	��B	��B	��B	��B	�(B	��B
 �B
�B
YB
_B
�B
�B
+B
�B
B
�B
�B
�B
�B
�B
_B
_B

�B
B
B

�B
�B
JB
B
"B
�B
\B
�B
�B
�B
�B
�B
uB
{B
�B
�B
{B
�B
SB
YB
�B
�B
	B
qB
B
�B
�B
B
�B
!B
�B
 �B
 �B
 �B
!�B
"�B
#B
#B
$@B
%B
%FB
%�B
%�B
&�B
'B
'B
'�B
'�B
'RB
'�B
'�B
(XB
(�B
)*B
)*B
)_B
)�B
*eB
*eB
*eB
*eB
+B
*�B
+�B
,=B
,=B
,�B
,�B
,�B
-CB
-�B
-�B
.B
.B
.IB
/B
/OB
/�B
/�B
0�B
0�B
0�B
1'B
1�B
1�B
1�B
2-B
2aB
2�B
3�B
3�B
4B
4B
4�B
4�B
4�B
5tB
5tB
5�B
6B
6zB
6FB
6FB
6FB
6zB
6�B
7B
6�B
7B
7�B
8�B
9�B
9$B
9XB
9$B
9XB
:�B
;0B
;0B
;0B
;�B
;�B
;�B
;dB
<B
<6B
<B
<6B
<�B
<�B
<�B
=qB
=<B
=<B
=<B
=�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�4B
�B
��B
��B
��B
�bB
�@B
�B
��B
�oB
�hB
��B
�uB
��B
�oB
�oB
�4B
��B
�B
��B
�:B
��B
��B
��B
�B
�.B
�4B
�B
�.B
��B
�:B
�hB
��B
��B
�B
��B
��B
�bB
��B
��B
��B
��B
�4B
�.B
��B
�uB
�bB
�~B
��B
�=B
�_B
�B
�hB
}VB
��B
�_B
��B
�fB
��B
�YB
�1B
�B
��B
��B
�_B
��B
��B
�+B
��B
��B
��B
��B
��B
��B
��B
�+B
�SB
��B
��B
��B
��B
��B
�GB
��B
��B
�GB
�MB
�B
�AB
��B
��B
�oB
��B
��B
��B
��B
��B
�iB
�B
�{B
��B
�B
�GB
��B
�oB
�B
�uB
��B
�4B
�B
�iB
~]B
~�B
�B
}"B
}�B
~�B
|�B
|�B
~]B
}�B
{�B
}"B
}�B
{�B
z�B
|PB
|�B
z�B
{B
{B
y�B
|B
x�B
v�B
w2B
|B
xB
v�B
u�B
v�B
r�B
tTB
sB
p�B
q�B
s�B
v�B
o�B
r�B
w2B
p�B
p;B
n�B
p�B
o B
o�B
o�B
r�B
l�B
jKB
l�B
iyB
e�B
i�B
hsB
d&B
l�B
gB
f�B
dZB
e�B
c�B
g8B
c�B
a|B
c B
aHB
bB
a�B
_;B
_�B
a|B
dZB
`BB
a|B
`�B
e�B
f�B
e,B
XyB
W
B
UgB
_;B
o�B
cTB
YB
N�B
I�B
M�B
P}B
K�B
J�B
I�B
FtB
F�B
H�B
F�B
FB
G�B
FtB
EmB
F?B
DgB
I�B
GB
EB
D�B
IB
H�B
E�B
F�B
F�B
DgB
E9B
GB
F?B
F?B
GB
F?B
C�B
E9B
E�B
C�B
C�B
EB
D3B
F?B
F?B
D3B
C-B
D3B
D�B
C-B
CaB
D�B
D3B
B�B
C�B
C�B
A�B
B�B
C�B
C�B
D�B
EB
D3B
B'B
A�B
A�B
A�B
A B
B�B
CaB
A�B
B'B
C�B
A�B
A�B
D3B
C�B
C�B
D3B
DgB
D3B
F�B
B�B
HKB
F?B
HB
H�B
F?B
G�B
H�B
GEB
GB
HB
GEB
FtB
G�B
HB
F?B
HB
H�B
F�B
GB
H�B
GzB
FtB
HB
H�B
F?B
GEB
G�B
F�B
FB
IB
L�B
J#B
L0B
NpB
T�B
PB
P�B
QB
OB
PHB
Q�B
QB
P�B
R�B
R�B
RTB
S[B
Y�B
U2B
\�B
^B
^�B
]dB
[�B
]dB
_B
_pB
]dB
\�B
`vB
_�B
`�B
b�B
bNB
bNB
d�B
f�B
c B
c�B
b�B
`�B
_�B
_�B
`�B
_B
_pB
aB
aHB
`B
_B
`vB
a�B
`�B
`B
bNB
b�B
aB
aB
b�B
a|B
aB
b�B
bNB
aB
bB
c�B
b�B
b�B
d�B
gB
gB
gmB
jKB
kQB
j�B
kB
m�B
l�B
m�B
n�B
qAB
o�B
p;B
rB
r�B
t�B
wfB
yrB
{JB
{�B
~]B
}�B
}�B
~�B
}�B
}�B
�B
��B
cB
~�B
� B
�iB
.B
.B
��B
��B
�B
�B
�1B
��B
��B
��B
� B
��B
�B
�B
�B
�B
�CB
��B
�@B
�nB
�zB
��B
�kB
��B
��B
��B
��B
�OB
��B
�UB
��B
�nB
�B
��B
�*B
�0B
�6B
�B
�B
�B
��B
�B
� B
�B
�HB
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
ĜB
ƨB
�XB
��B
ٴB
�)B
�#B
چB
��B
�B
�B
�HB
��B
�2B
�mB
�B
��B
��B
�B
��B
�B
�B
�B
��B
�WB
��B
�B
�iB
�oB
�vB
�%B
��B
�2B
��B
��B
�B
�>B
�>B
��B
�B
�xB
��B
��B
�B
��B
�VB
��B
�(B
�.B�BBABMB�B_B�B�B�B�B(B(BbBbB�B�B�BBMB�B�B�B+B�B�B�B1B�BqB	B�B7B=B�B	BCB�BB�B%�B+6B)�B*eB/B3hB5�B2�B<6B;�B9�B7�B8�B>BB>B:^B=�B@�B?�B?}B?�BB�BD�BD�BEBE�BF�BH�BJ�BK�BR�BV�BW?BY�BY�B^jB^Bb�Bh�BlWBsBu%Bs�Bu�BxlBv�B|B|PB~�B{�B~(B.BcB|�B}"B~�B�B�B��B�iB�B��B�B�B��B�uB�B��B�B��B��B��B�{B��B�B��B��B�B��B�fB�+B��B��B��B�_B��B�fB�B��B��B��B�kB�YB�\B��B��B�XB��B��B�RB��B��B��B��B��B��B��B�6B�B�OB�B�UB�HB�OB�UB�gB�B��B�tBȴB�BB��B��B��BҽBԕB�2B��B�[BӏB�gBԕB��BԕB�mB՛B��B��B�gB֡B�gB�B�EB�B��BخBچB��BݘB�pB�dB�;B�B��B�NB�NB�B�B�B��B�>B�8B�
B�DB�B�KB�KB��B��B�B�B�B��B��B�B��B��B�]B��B �B�cB�cB �B�B�BuB�BBABB�BSBB�B�B�B�BB�B)�B'B!-B&�B6B7�B4�B0�B2�B1�B.B-�B.B.IB,qB+�B-CB;�B5?BG�B=�B<�B6FB5tB4�B4B9�B6FB9$B6�B5?B7�B3�B7�B8�B8�B6�B6zB2-B@�BB�BK^BQ�BO�B4nB9�B2�B=<B/�B-wB-�B-�B0UB1�B-B+�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                            4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                            4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022082617085420220826170854IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022090512024820220905120248QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022090512024820220905120248QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                