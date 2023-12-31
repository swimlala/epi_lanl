CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-09-05T23:25:21Z creation; 2022-02-04T23:30:03Z DMQC;      
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
_FillValue        G�O�     �  c4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � W|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �P   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20210905232521  20220204223516  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_181                 6810_008521_181                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ّ3y|à@ّ3y|à11  @ّ3�?��@ّ3�?��@1-��=@1-��=�d��L�A��d��L�A�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @B�\@�  @�  @�p�@޸RA ��A��A ��A,��A@  A`  A�  A�  A�Q�A�  A��A�  A��A��B (�B(�B  B  B (�B((�B/�
B7�
B@  BG�
BO�BX  B`(�Bh  Bp(�Bx  B�B��B��B�  B�(�B�(�B�(�B�(�B�=qB�  B��B��B��
B�  B�{B�{B�{B�  B�{B��B��B�  B�{B��B�  B�(�B�{B��
B��B�(�B�{B��B��C��C�C��C  C
{C{C  C��C  C  C
=C  C��C�C  C 
=C!��C$  C&  C(  C*  C,  C-��C/�HC1�C4  C6  C7��C:
=C<  C=�HC?��CB
=CD{CF  CG��CI��CL  CN
=CP
=CR
=CT
=CV
=CW��CZ
=C\  C]��C`
=Cb
=Cd
=Cf  Ch  Cj  Cl  Cn
=Cp{Cr  Cs��Cv  Cx  Cz
=C|  C~  C�C�  C�  C�C���C�  C�C�  C���C�C���C�  C�C���C���C�  C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�  C�C�  C���C�  C���C�C�C�C���C�  C�
=C�  C���C�C�C���C���C�C�C�C�  C�C�C���C���C�  C�  C�C���C���C���C�  C�  C�C�C�C�  C�  C�C�C�C�  C���C���C���C���C���C���C���C���C�  C�  C�
=C�
=C�C���C���C���C���C���C�  C�  C�  C�C�C�C�  C�C�  C�  C�
=C�C�  C�  C�  C���C���C���C���C���C�  C�  C�C�C�C�C�  C�C�C�  C�C�  C�C�  C���C���C�  C�  C�C�D   D }qD ��D}qD�qD� D�D� D  D� D�qD}qD  D}qD  D� D�qD� D	  D	}qD
  D
}qD�D� D��D}qD�D}qD��D}qD�qD}qD�D}qD  D� D�qD�DD� D  D� D  D��D  D� D  D�D�D��DD��D  D� D�D}qD�qD� D  D}qD�qD}qD  D� D�qD ��D!  D!z�D!��D"� D#�D#��D#�qD$}qD%  D%}qD&  D&}qD'  D'�D(�D(��D)�D)� D*  D*� D+�D+��D,  D,�D-D-}qD.  D.��D/  D/z�D0  D0��D0�qD1� D2  D2��D3  D3� D4  D4� D5  D5��D6�D6� D7  D7��D8�D8��D9  D9� D:  D:� D:��D;xRD;�qD<�D=�D=}qD=�qD>� D?�D?��D@�D@� D@�qDA� DB�DB� DC  DC��DD�DD}qDE�DE� DF  DF� DF�qDG� DH�DH� DI�DI� DJ  DJ� DK  DK��DL  DL��DM  DM}qDN  DN� DO  DO��DO�qDP� DQ  DQ}qDQ�qDR}qDS  DS� DS�qDT��DU  DU}qDU�qDV��DW  DW}qDW�qDX��DY  DYz�DZ  DZ��D[  D[}qD[�qD\}qD]  D]� D]�qD^z�D^�qD_� D`  D`��Da�Da��Db  Db� Dc  Dc��Dd  Dd}qDe�De� Df�Df��Dg  Dg� Dg�qDhz�Dh�qDi}qDi��Dj� Dk  Dk� Dk�qDlz�Dl�qDm}qDm�qDn� Do  Do}qDo��Dp}qDq  Dq}qDr  Dr� Ds  Ds��Dt  Dt� Du�Du�Dv�Dv� Dw  Dw��Dx�Dx�Dy  Dy� Dz�Dz� D{�D{�D|  D|}qD|�qD}��D~�D~� DD��D�  D�AHD��HD�D��D�B�D���D�� D��qD�=qD�� D��HD�  D�AHD���D���D���D�B�D�� D�� D�  D�@ D�~�D���D�HD�B�D�~�D���D�  D�AHD���D��HD�  D�B�D��HD��HD��D�@ D��HD��HD�HD�AHD�}qD���D�HD�@ D�� D���D�HD�AHD�� D�� D�  D�AHD��HD�� D��D�AHD�}qD���D�HD�@ D�}qD���D�  D�B�D��HD��HD�HD�@ D�� D���D��)D�=qD�~�D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD��D�B�D�� D�� D���D�@ D��HD���D���D�AHD��HD���D�  D�@ D�� D�� D�HD�B�D���D��HD�  D�B�D���D�D��D�>�D�~�D���D���D�@ D��HD���D���D�@ D�~�D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�}qD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�>�D�~�D���D��qD�>�D�~�D��HD�HD�@ D�~�D��HD�  D�AHD��HD��HD�HD�>�D�~�D��HD�HD�@ D�~�D���D���D�>�D��HD�� D��qD�@ D��HD���D�  D�@ D�~�D�� D�  D�>�D��HD�D�HD�@ D�� D��HD�HD�>�D�~�D�� D���D�>�D�~�D�� D�  D�AHD��HD���D���D�AHD���D�� D�  D�=qD�~�D��HD�HD�B�D�~�D���D���D�>�D�~�D��HD�HD�AHD�� D��qD�  D�AHD�� D��HD�HD�@ D�� D�� D���D�@ D��HD�� D��qD�>�D�� D��HD�  D�>�D�~�D��HD���D�>�D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�}qD��HD�HD�@ D��HD�D�HD�AHDHD��HD�  D�@ D�~�D�� D��D�AHD�~�D��HD��D�@ Dŀ D��HD���D�>�Dƀ D��HD�  D�@ Dǀ DǾ�D��qD�=qD�~�D�� D�HD�@ Dɀ Dɾ�D���D�=qD�~�Dʾ�D���D�>�Dˀ D�D��D�@ D̀ D�� D�HD�AHD̀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�DнqD���D�>�D�}qDѾ�D�HD�AHDҁHDҾ�D���D�>�D�}qDӽqD��qD�=qD�}qD�� D�HD�AHDՁHD�� D���D�@ Dր D��HD�HD�@ D׀ D�� D�  D�@ D�~�DؽqD���D�>�Dـ D�� D�  D�>�Dڀ D�� D���D�AHDہHD�� D�HD�@ D�}qDܾ�D�  D�@ D݀ Dݾ�D���D�AHDހ D޾�D�HD�B�D߂�D�D�  D�>�D��HD�D��D�AHD�~�D�� D�  D�@ D� D⾸D���D�@ DわD��HD���D�AHD�HD��HD�HD�@ D� D徸D���D�>�D悏D�D�  D�>�D�~�D��HD�HD�@ D�~�D�� D��D�AHD�~�D龸D���D�=qD�~�D��HD���D�=qD�~�D�� D�  D�AHD� D쾸D�  D�AHD� D���D���D�>�D�HD�� D��qD�@ D� D�� D��D�AHD�� D�� D�HD�AHD�~�D�D�HD�AHD� D�{?��?#�
?aG�?�=q?���?\?�
=@�\@
=q@z�@(��@8Q�@B�\@W
=@c�
@n{@�G�@���@�{@�Q�@�G�@�ff@���@���@��R@Ǯ@У�@�
=@޸R@���@�{@�A ��A�
A�A��AG�A33A��A{A!G�A%�A*=qA.�RA1�A7�A;�A>�RAC33AHQ�AK�AN�RAS33AXQ�A[�A^�RAc33Ag�Aj=qAo\)Atz�AxQ�A{�A�Q�A��\A��
A�{A���A���A��A�A��A���A��A�p�A�
=A���A��HA�p�A�
=A���A��A�A��A���A��
A�{A��A���A�z�A��RA���A��\A�p�A�\)A�G�A��
A�ffA�Q�A��A���AθRA�Q�A��HA��AָRA���AۅA�p�A�
=A��A�(�A�A�  A�\A���A�ffA�  A��HA��A�ffA���A�33A���A�{B z�B��B=qB33B��Bp�B{B�B��B	G�B
=qB�B(�B�B�\B33B  Bp�B=qB
=Bz�BG�B{B33B(�Bp�B=qB
=B(�Bp�B{B\)B Q�B ��B"ffB#33B#�
B%�B&=qB&�RB'�
B)�B)B*�\B+�B,��B-p�B.�\B/�B0z�B1p�B2�\B3�
B4��B5p�B6ffB7�
B8��B9��B:�\B;�
B<��B=��B>�RB?�
B@z�BA��BB�HBD  BD��BE��BF�HBH  BH��BIBJ�HBLQ�BM�BN{BO�BP��BQp�BR�RBT(�BUG�BV{BW\)BX��BY�BZ�HB\  B]G�B^ffB_33B`(�Ba��Bc
=Bd  Bd��Bf{Bg�Bh��Bi�Bk
=Blz�Bm�Bo
=Bp  Bqp�Br�HBs�
Bu�Bv�RBw�
Bx��Bz{B{�B|��B}B33B�Q�B���B�G�B�{B��RB�33B��B���B�33B��B�z�B�33B��B�ffB�33B��
B�ffB��HB��B�ffB���B�p�B�Q�B���B���B�(�B���B�B�=qB��HB��B�z�B��B��B�Q�B��B��B�ffB��B�  B���B��B�  B���B�\)B�  B��HB��B�=qB��HB��B�ffB�33B��
B�ffB�33B�  B���B�\)B�  B��RB���B�=qB���B���B�ffB��HB���B�ffB�33B�B�ffB�G�B�{B��RB�\)B�{B���B��B�=qB��HB��B�z�B�33B��
B�ffB��B��B���B�\)B�  B��\B�G�B�{B��HB��B�{B¸RBÙ�B�Q�B���BŅB�=qB���B�B�z�B�33B�B�ffB�G�B��
B�Q�B�
=B��BΣ�B�G�B��B�z�B�G�B�{BҸRB�\)B�(�B���Bՙ�B�{B���B׮B�=qB���Bٙ�B�ffB�
=Bۙ�B�(�B��HBݮB�ffB��HB�p�B�=qB���B�B�  B�RB�p�B�(�B�\B�33B��B��B�p�B��B�z�B��B��B�\B��B뙚B�=qB���B��B�ffB��HB�p�B�  B�RB�\)B��B�z�B�
=B�B�ffB�
=B��B�  B��\B�33B�B�z�B�
=B���B�  B��\B�G�B��B�ffB���B�G�B�  B���B�
=B��C 
=C ffC �C ��C(�Cp�C�RC{CG�Cz�C�
C(�Cz�CC��C=qC�\C�
C(�Cz�CC��C=qC��C�HC�CQ�C��C��C=qC�CC	  C	=qC	��C	�C
(�C
p�C
�C
��CG�C��C��C33Cp�C�C��CQ�C�C�HC�Cz�C��C(�Cz�C�C��CG�C��C��C33Cp�C�RC{Cp�C�C�C33C��C�HC(�CffC�RC�CffC��C  CffC��C�C=qC��C��C=qC�C�
C=qC�\C�
C�C�C�HC(�CffC�
C33C�\C�HC(�Cp�C�
C33Cz�C��C(�C�\C�
C �C �C �C!=qC!�C!�
C"33C"��C"�C#33C#�\C#��C$Q�C$��C$�C%=qC%�C&
=C&ffC&�RC'
=C'p�C'�
C(=qC(�\C(�HC)=qC)�C*{C*ffC*�RC+�C+�\C+��C,Q�C,��C-
=C-z�C-�C.G�C.��C/  C/p�C/��C0�C0�\C1  C1ffC1�RC2{C2z�C2��C3\)C3�RC4{C4p�C4�
C5G�C5�RC6�C6�C6�
C7G�C7�RC8{C8ffC8��C9=qC9�C:{C:p�C:��C;(�C;�\C<  C<ffC<��C=(�C=�C=�
C>=qC>�C?
=C?ffC?��C@(�C@�C@�HCAG�CA�RCB
=CBffCB�RCC�CC�CC�CDQ�CD�RCE  CEQ�CE�RCF�CFz�CF�HCG33CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                       ?��@   @B�\@�  @�  @�p�@޸RA ��A��A ��A,��A@  A`  A�  A�  A�Q�A�  A��A�  A��A��B (�B(�B  B  B (�B((�B/�
B7�
B@  BG�
BO�BX  B`(�Bh  Bp(�Bx  B�B��B��B�  B�(�B�(�B�(�B�(�B�=qB�  B��B��B��
B�  B�{B�{B�{B�  B�{B��B��B�  B�{B��B�  B�(�B�{B��
B��B�(�B�{B��B��C��C�C��C  C
{C{C  C��C  C  C
=C  C��C�C  C 
=C!��C$  C&  C(  C*  C,  C-��C/�HC1�C4  C6  C7��C:
=C<  C=�HC?��CB
=CD{CF  CG��CI��CL  CN
=CP
=CR
=CT
=CV
=CW��CZ
=C\  C]��C`
=Cb
=Cd
=Cf  Ch  Cj  Cl  Cn
=Cp{Cr  Cs��Cv  Cx  Cz
=C|  C~  C�C�  C�  C�C���C�  C�C�  C���C�C���C�  C�C���C���C�  C�  C�  C���C���C�  C���C���C�  C�  C�  C�C�  C�C�  C���C�  C���C�C�C�C���C�  C�
=C�  C���C�C�C���C���C�C�C�C�  C�C�C���C���C�  C�  C�C���C���C���C�  C�  C�C�C�C�  C�  C�C�C�C�  C���C���C���C���C���C���C���C���C�  C�  C�
=C�
=C�C���C���C���C���C���C�  C�  C�  C�C�C�C�  C�C�  C�  C�
=C�C�  C�  C�  C���C���C���C���C���C�  C�  C�C�C�C�C�  C�C�C�  C�C�  C�C�  C���C���C�  C�  C�C�D   D }qD ��D}qD�qD� D�D� D  D� D�qD}qD  D}qD  D� D�qD� D	  D	}qD
  D
}qD�D� D��D}qD�D}qD��D}qD�qD}qD�D}qD  D� D�qD�DD� D  D� D  D��D  D� D  D�D�D��DD��D  D� D�D}qD�qD� D  D}qD�qD}qD  D� D�qD ��D!  D!z�D!��D"� D#�D#��D#�qD$}qD%  D%}qD&  D&}qD'  D'�D(�D(��D)�D)� D*  D*� D+�D+��D,  D,�D-D-}qD.  D.��D/  D/z�D0  D0��D0�qD1� D2  D2��D3  D3� D4  D4� D5  D5��D6�D6� D7  D7��D8�D8��D9  D9� D:  D:� D:��D;xRD;�qD<�D=�D=}qD=�qD>� D?�D?��D@�D@� D@�qDA� DB�DB� DC  DC��DD�DD}qDE�DE� DF  DF� DF�qDG� DH�DH� DI�DI� DJ  DJ� DK  DK��DL  DL��DM  DM}qDN  DN� DO  DO��DO�qDP� DQ  DQ}qDQ�qDR}qDS  DS� DS�qDT��DU  DU}qDU�qDV��DW  DW}qDW�qDX��DY  DYz�DZ  DZ��D[  D[}qD[�qD\}qD]  D]� D]�qD^z�D^�qD_� D`  D`��Da�Da��Db  Db� Dc  Dc��Dd  Dd}qDe�De� Df�Df��Dg  Dg� Dg�qDhz�Dh�qDi}qDi��Dj� Dk  Dk� Dk�qDlz�Dl�qDm}qDm�qDn� Do  Do}qDo��Dp}qDq  Dq}qDr  Dr� Ds  Ds��Dt  Dt� Du�Du�Dv�Dv� Dw  Dw��Dx�Dx�Dy  Dy� Dz�Dz� D{�D{�D|  D|}qD|�qD}��D~�D~� DD��D�  D�AHD��HD�D��D�B�D���D�� D��qD�=qD�� D��HD�  D�AHD���D���D���D�B�D�� D�� D�  D�@ D�~�D���D�HD�B�D�~�D���D�  D�AHD���D��HD�  D�B�D��HD��HD��D�@ D��HD��HD�HD�AHD�}qD���D�HD�@ D�� D���D�HD�AHD�� D�� D�  D�AHD��HD�� D��D�AHD�}qD���D�HD�@ D�}qD���D�  D�B�D��HD��HD�HD�@ D�� D���D��)D�=qD�~�D�� D�  D�@ D�~�D���D�  D�@ D�� D��HD��D�B�D�� D�� D���D�@ D��HD���D���D�AHD��HD���D�  D�@ D�� D�� D�HD�B�D���D��HD�  D�B�D���D�D��D�>�D�~�D���D���D�@ D��HD���D���D�@ D�~�D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�>�D�}qD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�>�D�~�D���D��qD�>�D�~�D��HD�HD�@ D�~�D��HD�  D�AHD��HD��HD�HD�>�D�~�D��HD�HD�@ D�~�D���D���D�>�D��HD�� D��qD�@ D��HD���D�  D�@ D�~�D�� D�  D�>�D��HD�D�HD�@ D�� D��HD�HD�>�D�~�D�� D���D�>�D�~�D�� D�  D�AHD��HD���D���D�AHD���D�� D�  D�=qD�~�D��HD�HD�B�D�~�D���D���D�>�D�~�D��HD�HD�AHD�� D��qD�  D�AHD�� D��HD�HD�@ D�� D�� D���D�@ D��HD�� D��qD�>�D�� D��HD�  D�>�D�~�D��HD���D�>�D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�}qD��HD�HD�@ D��HD�D�HD�AHDHD��HD�  D�@ D�~�D�� D��D�AHD�~�D��HD��D�@ Dŀ D��HD���D�>�Dƀ D��HD�  D�@ Dǀ DǾ�D��qD�=qD�~�D�� D�HD�@ Dɀ Dɾ�D���D�=qD�~�Dʾ�D���D�>�Dˀ D�D��D�@ D̀ D�� D�HD�AHD̀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�~�DнqD���D�>�D�}qDѾ�D�HD�AHDҁHDҾ�D���D�>�D�}qDӽqD��qD�=qD�}qD�� D�HD�AHDՁHD�� D���D�@ Dր D��HD�HD�@ D׀ D�� D�  D�@ D�~�DؽqD���D�>�Dـ D�� D�  D�>�Dڀ D�� D���D�AHDہHD�� D�HD�@ D�}qDܾ�D�  D�@ D݀ Dݾ�D���D�AHDހ D޾�D�HD�B�D߂�D�D�  D�>�D��HD�D��D�AHD�~�D�� D�  D�@ D� D⾸D���D�@ DわD��HD���D�AHD�HD��HD�HD�@ D� D徸D���D�>�D悏D�D�  D�>�D�~�D��HD�HD�@ D�~�D�� D��D�AHD�~�D龸D���D�=qD�~�D��HD���D�=qD�~�D�� D�  D�AHD� D쾸D�  D�AHD� D���D���D�>�D�HD�� D��qD�@ D� D�� D��D�AHD�� D�� D�HD�AHD�~�D�D�HD�AHD� G�O�?��?#�
?aG�?�=q?���?\?�
=@�\@
=q@z�@(��@8Q�@B�\@W
=@c�
@n{@�G�@���@�{@�Q�@�G�@�ff@���@���@��R@Ǯ@У�@�
=@޸R@���@�{@�A ��A�
A�A��AG�A33A��A{A!G�A%�A*=qA.�RA1�A7�A;�A>�RAC33AHQ�AK�AN�RAS33AXQ�A[�A^�RAc33Ag�Aj=qAo\)Atz�AxQ�A{�A�Q�A��\A��
A�{A���A���A��A�A��A���A��A�p�A�
=A���A��HA�p�A�
=A���A��A�A��A���A��
A�{A��A���A�z�A��RA���A��\A�p�A�\)A�G�A��
A�ffA�Q�A��A���AθRA�Q�A��HA��AָRA���AۅA�p�A�
=A��A�(�A�A�  A�\A���A�ffA�  A��HA��A�ffA���A�33A���A�{B z�B��B=qB33B��Bp�B{B�B��B	G�B
=qB�B(�B�B�\B33B  Bp�B=qB
=Bz�BG�B{B33B(�Bp�B=qB
=B(�Bp�B{B\)B Q�B ��B"ffB#33B#�
B%�B&=qB&�RB'�
B)�B)B*�\B+�B,��B-p�B.�\B/�B0z�B1p�B2�\B3�
B4��B5p�B6ffB7�
B8��B9��B:�\B;�
B<��B=��B>�RB?�
B@z�BA��BB�HBD  BD��BE��BF�HBH  BH��BIBJ�HBLQ�BM�BN{BO�BP��BQp�BR�RBT(�BUG�BV{BW\)BX��BY�BZ�HB\  B]G�B^ffB_33B`(�Ba��Bc
=Bd  Bd��Bf{Bg�Bh��Bi�Bk
=Blz�Bm�Bo
=Bp  Bqp�Br�HBs�
Bu�Bv�RBw�
Bx��Bz{B{�B|��B}B33B�Q�B���B�G�B�{B��RB�33B��B���B�33B��B�z�B�33B��B�ffB�33B��
B�ffB��HB��B�ffB���B�p�B�Q�B���B���B�(�B���B�B�=qB��HB��B�z�B��B��B�Q�B��B��B�ffB��B�  B���B��B�  B���B�\)B�  B��HB��B�=qB��HB��B�ffB�33B��
B�ffB�33B�  B���B�\)B�  B��RB���B�=qB���B���B�ffB��HB���B�ffB�33B�B�ffB�G�B�{B��RB�\)B�{B���B��B�=qB��HB��B�z�B�33B��
B�ffB��B��B���B�\)B�  B��\B�G�B�{B��HB��B�{B¸RBÙ�B�Q�B���BŅB�=qB���B�B�z�B�33B�B�ffB�G�B��
B�Q�B�
=B��BΣ�B�G�B��B�z�B�G�B�{BҸRB�\)B�(�B���Bՙ�B�{B���B׮B�=qB���Bٙ�B�ffB�
=Bۙ�B�(�B��HBݮB�ffB��HB�p�B�=qB���B�B�  B�RB�p�B�(�B�\B�33B��B��B�p�B��B�z�B��B��B�\B��B뙚B�=qB���B��B�ffB��HB�p�B�  B�RB�\)B��B�z�B�
=B�B�ffB�
=B��B�  B��\B�33B�B�z�B�
=B���B�  B��\B�G�B��B�ffB���B�G�B�  B���B�
=B��C 
=C ffC �C ��C(�Cp�C�RC{CG�Cz�C�
C(�Cz�CC��C=qC�\C�
C(�Cz�CC��C=qC��C�HC�CQ�C��C��C=qC�CC	  C	=qC	��C	�C
(�C
p�C
�C
��CG�C��C��C33Cp�C�C��CQ�C�C�HC�Cz�C��C(�Cz�C�C��CG�C��C��C33Cp�C�RC{Cp�C�C�C33C��C�HC(�CffC�RC�CffC��C  CffC��C�C=qC��C��C=qC�C�
C=qC�\C�
C�C�C�HC(�CffC�
C33C�\C�HC(�Cp�C�
C33Cz�C��C(�C�\C�
C �C �C �C!=qC!�C!�
C"33C"��C"�C#33C#�\C#��C$Q�C$��C$�C%=qC%�C&
=C&ffC&�RC'
=C'p�C'�
C(=qC(�\C(�HC)=qC)�C*{C*ffC*�RC+�C+�\C+��C,Q�C,��C-
=C-z�C-�C.G�C.��C/  C/p�C/��C0�C0�\C1  C1ffC1�RC2{C2z�C2��C3\)C3�RC4{C4p�C4�
C5G�C5�RC6�C6�C6�
C7G�C7�RC8{C8ffC8��C9=qC9�C:{C:p�C:��C;(�C;�\C<  C<ffC<��C=(�C=�C=�
C>=qC>�C?
=C?ffC?��C@(�C@�C@�HCAG�CA�RCB
=CBffCB�RCC�CC�CC�CDQ�CD�RCE  CEQ�CE�RCF�CFz�CF�HCG33CGz�CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                       @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�{A�{A�{A�bA��A��A�"�A�"�A�"�A�"�A� �A� �A�$�A�"�A�"�A�"�A�&�A�(�A�(�A�+A�-A�/A�/A�/A�1'A�1'A�1'A�33A�33A�33A�5?A�7LA�9XAڇ+A��Aכ�A�z�A�C�A��;Aֲ-A֏\A�"�A��A�A�bNA�A�A�oA���A�dZA�1A�z�A�%A�
=AэPAГuA�JA��A�ZA̺^A�~�A�JA�A�~�A��`A��Aŧ�A�{A�bAìA�VA�M�A��7A��;A�A��^A���A�5?A�VA��#A��A�;dA�VA�n�A���A���A��hA���A��TA���A��;A�(�A�A�A��A��FA�G�A��7A��;A��mA��A�ZA���A��A�/A�=qA��FA��wA���A��PA�{A�A�A���A�E�A�|�A��PA��A~E�A}XA{�TAy�;Ay/AxVAt�uApbNAm;dAi�Ae/Ac��Aa��A_"�A^^5A]�AZ�jAW�AU��AS;dAP��AOhsAN��AN{AL=qAJ1AHn�AFJABȴA@~�A?�A?%A=A:bNA8A7C�A6��A4�/A2�!A1%A0��A0z�A/hsA-K�A,�\A,bA+C�A'��A%�A$�A#�A"Q�A!�-A!
=A�-AffA=qA�A�TA33A�Az�A�AĜA-A��AƨAoAn�AdZA�
A�A�A��A�A�A�DA�A�FAt�A7LA
�/A
(�A	�PA	�A	C�A�yA�At�A/A��A�RA�+A5?A�RA�HAE�A-A�A �/@�%@�v�@�G�@�z�@��w@���@��P@��H@�D@��H@�R@�dZ@�;d@���@�;d@��@�/@�7L@�=q@�S�@�1'@�
=@�v�@��@�(�@�@�!@��@���@�z�@蛦@�9X@��@�
=@�
=@��@�(�@���@�|�@��@���@���@�9@���@��#@�E�@ܼj@ٲ-@�r�@�;d@��@���@��@ٺ^@�v�@� �@�5?@�+@�"�@֏\@�{@��@ӝ�@�bN@���@Լj@Ԭ@�Z@��@���@ӶF@�|�@�"�@�dZ@�33@��@���@ҧ�@�v�@Ұ!@ҟ�@��@�%@мj@�r�@�9X@϶F@���@Ώ\@�n�@�^5@�5?@�J@���@��@ˮ@�t�@�+@���@��y@�ȴ@ʏ\@�V@�5?@�J@���@ɑh@�%@ȴ9@�9X@�+@�o@��@�{@��/@��@� �@��;@�|�@���@�n�@�=q@��T@�?}@��@�O�@�V@��9@��@�o@��@��H@�ȴ@�E�@�`B@�b@�t�@�ȴ@���@���@��u@��u@��j@���@�j@�ƨ@�dZ@�ȴ@���@�v�@�ff@�{@�`B@���@�Ĝ@�Ĝ@�Ĝ@��@�1'@��@�+@��@�J@��@��@��#@�?}@��@��@�V@���@�(�@�;d@�v�@�5?@�=q@�@��@�@�@���@��D@�A�@�(�@��@� �@�1@���@��@�+@��@�v�@��^@���@�?}@�Ĝ@��D@�I�@�b@��w@�+@��@���@�V@���@�@���@�7L@���@��j@�z�@�b@���@���@�|�@�\)@�C�@��H@�V@��@�@��#@�@��@�V@���@�j@���@�l�@�S�@�o@���@�5?@�@��@��-@��@�O�@��j@�bN@��;@�|�@��!@�=q@�@��@�X@��@�V@��@���@�j@��@���@��@���@�l�@�S�@�"�@��@��R@�^5@�{@��@��-@��h@�hs@�/@���@���@���@�Z@�1'@��;@��F@��@�K�@�ȴ@�$�@��-@�&�@��j@��@�z�@�j@�b@��;@��@���@�t�@�\)@�K�@�o@���@��@��!@�V@�$�@�{@���@�X@�7L@��@���@��`@��9@�z�@�Z@��@�ƨ@��@��P@�l�@�l�@�\)@�K�@�
=@��\@�^5@�M�@�=q@�5?@��-@�x�@�G�@�%@��@���@��9@��@��D@�I�@�ƨ@�\)@��@�@��R@�~�@�5?@�J@���@�x�@�O�@�?}@�%@���@��9@���@��@�j@�Z@�1'@��@�  @��
@�t�@�o@���@�ȴ@��!@�^5@�E�@�-@�{@��^@�?}@���@��@�bN@�(�@~��@}�@|��@|�@|I�@|Z@|�j@|��@{�m@{ƨ@{�
@{�m@{��@y�^@yX@y&�@y�@x��@x�9@x�@xQ�@x  @x  @w��@w\)@v��@v$�@u�-@u�h@u?}@t�@tZ@t1@s�@r��@rn�@r=q@rJ@q�#@qX@o�@oK�@n�R@nv�@m@m`B@m?}@l�@l��@lZ@k��@k��@kC�@j�!@j��@jM�@i�7@h�`@h�9@hr�@h  @g�@gl�@g+@g�@f��@f��@f��@f5?@ep�@e�@d�/@d�D@dI�@dI�@c�m@cdZ@c33@co@bn�@a��@ax�@ahs@aX@aG�@a&�@`Ĝ@` �@_�;@_�;@`  @_�@_��@_��@_+@^�y@^�R@^$�@]p�@\�@\�D@\j@\I�@\1@[�m@[�m@[��@["�@Z��@Z��@Y��@XbN@W�@W�P@W�@V��@V�@Vȴ@V�R@V��@V�+@Vff@V$�@U@U��@U/@T��@T�/@T�D@T�@S�F@So@S@R��@R�!@R~�@R�@Q�#@Qhs@Q7L@Q7L@Q&�@P��@P  @O�@O�P@O\)@O+@N�R@N{@M@Mp�@M/@L��@L��@Lj@LI�@L9X@LI�@L9X@L(�@K��@Kƨ@KS�@Ko@J�\@JM�@JJ@I��@IX@I7L@I%@H�`@H�9@Gl�@Fȴ@FV@E��@E?}@D��@D�@C@B��@BJ@Ahs@A%@@��@@��@@�u@@bN@?��@?|�@?
=@>v�@>E�@=@=p�@=O�@<�@<��@<j@<�@<1@;�
@;�F@;�@;C�@:�H@:��@:~�@:-@9��@9�@9��@9G�@8Ĝ@8�u@8�@8bN@81'@7�@7�w@7��@7+@6�R@6v�@6{@5��@5V@4�j@4�@4�@4��@4z�@4z�@4j@4(�@4�@3�m@3�F@3��@3dZ@3@2�@1��@1�7@1&�@0Ĝ@0��@0r�@01'@0 �@/�;@/�P@.��@.{@.{@.@-�h@,�@,z�@,I�@+�F@+C�@*��@*=q@)�@)�#@)�#@)�#@)�^@)�^@)�7@)x�@)hs@)7L@(Ĝ@(��@(�@(A�@'�;@'�P@'K�@'
=@&�@&�R@&V@&@%��@%�-@%��@%�@%V@$��@$9X@#�m@#��@#�@#t�@#dZ@#S�@#"�@#@#@"�H@"��@"��@"��@"�\@"~�@"n�@"^5@"�@!��@!x�@!&�@ ��@ �u@  �@�;@��@�w@�w@�@��@�P@\)@��@v�@V@E�@{@��@@@�h@O�@/@��@�D@I�@9X@��@��@��@�@S�@33@�@~�@�@��@hs@X@G�@G�@��@��@��@bN@A�@1'@ �@ �@b@  @�;@�P@\)@��@v�@E�@$�@��@p�@��@��@��@(�@��@�
@�
@�
@ƨ@�@S�@@�!@��@n�@M�A�1A�1A�%A�JA�oA��A�{A��A�{A�JA�{A�oA�bA��A��A�oA�{A��A�oA��A�oA�1A��A��A�{A��A��A��A� �A�$�A� �A� �A�&�A�"�A�"�A�$�A�$�A� �A�$�A�$�A� �A� �A�"�A�"�A��A�"�A�"�A��A� �A�$�A� �A��A� �A�$�A�"�A� �A�$�A�$�A��A� �A�$�A�$�A�"�A�$�A�&�A� �A�"�A�&�A�"�A��A�$�A�"�A��A�$�A�&�A�"�A� �A� �A�$�A�$�A�"�A�$�A�&�A�"�A��A� �A�$�A�"�A� �A�"�A�&�A�$�A�$�A�+A�&�A�$�A�&�A�+A�(�A�$�A�+A�(�A�$�A�+A�+A�&�A�(�A�-A�+A�&�A�+A�-A�&�A�(�A�-A�-A�(�A�(�A�/A�-A�(�A�+A�/A�-A�+A�/A�1'A�+A�-A�1'A�/A�-A�/A�1'A�-A�/A�33A�-A�+A�1'A�+A�-A�33A�/A�-A�33A�/A�-A�-A�/A�33A�/A�-A�1'A�1'A�-A�1'A�1'A�/A�33A�1'A�/A�1'A�33A�/A�1'A�5?A�/A�/A�5?A�33A�/A�1'A�5?A�1'A�/A�1'A�5?A�33A�/A�1'A�5?A�33A�/A�1'A�5?A�1'A�1'A�5?A�5?A�1'A�1'A�5?A�5?A�33A�1'A�5?A�7LA�33A�1'A�5?A�7LA�33A�33A�7LA�7LA�33A�5?A�9XA�9XA�33A�5?A�9XA�9XA�5?A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�5?A�33A�A�1A�A���A���A�n�A�-A�
=A��;A�ƨA׮Aץ�Aק�Aף�Aח�A׉7A׋DA׉7A׃AׁA�z�A�l�A�jA�l�A�l�A�^5A�E�A�7LA��A�  A���A��A��mA�ƨA���A���AֶFA֮Aֲ-Aִ9A֮A֮Aֲ-Aֲ-A֣�A֓uA�|�A�`BA�M�A�?}A�/A��A�bA�A���A��A��A��yA��HA��HA��#A�ȴA�Aպ^AնFAծAէ�A�t�A�G�A�A�A�=qA�C�A�E�A�A�A�?}A�A�A�C�A�9XA�-A�%A��A��mA��;A��#A��/A���AԾwAԩ�Aԙ�A�t�A�O�A�G�A�G�A�G�A�9XA��A���A��mA��
AӮAӑhA�x�A�l�A�dZA�\)A�E�A�(�A�bA��A��
Aҩ�A�jA� �A��TA��/A���A���A�ĜAѡ�A�|�A�r�A�VA��A��HAЩ�A�hsA�I�A�7LA�33A�33A��A�
=A��Aϧ�AρA�E�A��AΣ�A�O�A�VAͰ!A�bNA�/A�oA���A��A��TA���A̩�A̕�ȂhȂhA̋DA�~�A�x�A�r�A�l�A�S�A�E�A�A�A�1'A��#AˬAˇ+A�l�A�
=A��HAʝ�A�A�A���A��;A�ȴA�|�A�VA�(�A� �A� �A�
=A�$�A�K�A���A�C�A�1'A�(�A� �A��A�VA�
=A��AżjAŶFAŝ�Aş�Aţ�Ať�AŁA�l�A�ZA�Q�A�7LA���A�l�A�C�A�7LA� �A�bA�%A��A��`A��/A���A���A���AöFA×�AÅA�?}A�(�A�{A�bA�
=A�  A���A���A���A��A���A�x�A��+A���A�I�A��
A���A���A��uA�t�A�1'A�{A�%A���A��A��mA��HA��;A��wA�z�A�5?A��`A�ĜA��wA���A��A�9XA��PA�JA�A�t�A�^5A�C�A�(�A�$�A��A�1A��A���A��^A�9XA�x�A�A�A�{A��A��hA�ZA�7LA��A�{A�oA�1A��A���A��A�VA���A��jA���A�^5A��
A�p�A�A���A�n�A�dZA�^5A�\)A�O�A�C�A�5?A��A�JA�
=A��A��A�{A�%A��A��A���A���A��A�A�n�A�  A��yA��/A���A��FA�x�A�O�A�K�A�=qA���A��PA�~�A�
=A��9A��+A��DA�K�A���A�JA���A�z�A�dZA�M�A�?}A�/A�"�A��A��A��7A�^5A��HA��A�1A���A���A�ƨA�\)A��
A�bNA��A��wA��+A�E�A��mA���A�r�A��A�?}A���A�t�A�ffA�=qA��A�A���A��
A�=qA���A�p�A�E�A�JA��yA���A��A��RA��yA��uA��yA�O�A��
A�`BA�%A���A��;A��FA���A�t�A�M�A�
=A���A�5?A��FA��uA��DA�p�A�?}A���A���A�n�A���A��A��A��A�`BA���A�ZA�A�A��A�A��A��;A���A���A��A�VA�(�A��#A��7A��HA�ffA�1'A�ȴA�hsA�+A��TA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                       A�JA�{A�{A�{A�bA��A��A�"�A�"�A�"�A�"�A� �A� �A�$�A�"�A�"�A�"�A�&�A�(�A�(�A�+A�-A�/A�/A�/A�1'A�1'A�1'A�33A�33A�33A�5?A�7LA�9XAڇ+A��Aכ�A�z�A�C�A��;Aֲ-A֏\A�"�A��A�A�bNA�A�A�oA���A�dZA�1A�z�A�%A�
=AэPAГuA�JA��A�ZA̺^A�~�A�JA�A�~�A��`A��Aŧ�A�{A�bAìA�VA�M�A��7A��;A�A��^A���A�5?A�VA��#A��A�;dA�VA�n�A���A���A��hA���A��TA���A��;A�(�A�A�A��A��FA�G�A��7A��;A��mA��A�ZA���A��A�/A�=qA��FA��wA���A��PA�{A�A�A���A�E�A�|�A��PA��A~E�A}XA{�TAy�;Ay/AxVAt�uApbNAm;dAi�Ae/Ac��Aa��A_"�A^^5A]�AZ�jAW�AU��AS;dAP��AOhsAN��AN{AL=qAJ1AHn�AFJABȴA@~�A?�A?%A=A:bNA8A7C�A6��A4�/A2�!A1%A0��A0z�A/hsA-K�A,�\A,bA+C�A'��A%�A$�A#�A"Q�A!�-A!
=A�-AffA=qA�A�TA33A�Az�A�AĜA-A��AƨAoAn�AdZA�
A�A�A��A�A�A�DA�A�FAt�A7LA
�/A
(�A	�PA	�A	C�A�yA�At�A/A��A�RA�+A5?A�RA�HAE�A-A�A �/@�%@�v�@�G�@�z�@��w@���@��P@��H@�D@��H@�R@�dZ@�;d@���@�;d@��@�/@�7L@�=q@�S�@�1'@�
=@�v�@��@�(�@�@�!@��@���@�z�@蛦@�9X@��@�
=@�
=@��@�(�@���@�|�@��@���@���@�9@���@��#@�E�@ܼj@ٲ-@�r�@�;d@��@���@��@ٺ^@�v�@� �@�5?@�+@�"�@֏\@�{@��@ӝ�@�bN@���@Լj@Ԭ@�Z@��@���@ӶF@�|�@�"�@�dZ@�33@��@���@ҧ�@�v�@Ұ!@ҟ�@��@�%@мj@�r�@�9X@϶F@���@Ώ\@�n�@�^5@�5?@�J@���@��@ˮ@�t�@�+@���@��y@�ȴ@ʏ\@�V@�5?@�J@���@ɑh@�%@ȴ9@�9X@�+@�o@��@�{@��/@��@� �@��;@�|�@���@�n�@�=q@��T@�?}@��@�O�@�V@��9@��@�o@��@��H@�ȴ@�E�@�`B@�b@�t�@�ȴ@���@���@��u@��u@��j@���@�j@�ƨ@�dZ@�ȴ@���@�v�@�ff@�{@�`B@���@�Ĝ@�Ĝ@�Ĝ@��@�1'@��@�+@��@�J@��@��@��#@�?}@��@��@�V@���@�(�@�;d@�v�@�5?@�=q@�@��@�@�@���@��D@�A�@�(�@��@� �@�1@���@��@�+@��@�v�@��^@���@�?}@�Ĝ@��D@�I�@�b@��w@�+@��@���@�V@���@�@���@�7L@���@��j@�z�@�b@���@���@�|�@�\)@�C�@��H@�V@��@�@��#@�@��@�V@���@�j@���@�l�@�S�@�o@���@�5?@�@��@��-@��@�O�@��j@�bN@��;@�|�@��!@�=q@�@��@�X@��@�V@��@���@�j@��@���@��@���@�l�@�S�@�"�@��@��R@�^5@�{@��@��-@��h@�hs@�/@���@���@���@�Z@�1'@��;@��F@��@�K�@�ȴ@�$�@��-@�&�@��j@��@�z�@�j@�b@��;@��@���@�t�@�\)@�K�@�o@���@��@��!@�V@�$�@�{@���@�X@�7L@��@���@��`@��9@�z�@�Z@��@�ƨ@��@��P@�l�@�l�@�\)@�K�@�
=@��\@�^5@�M�@�=q@�5?@��-@�x�@�G�@�%@��@���@��9@��@��D@�I�@�ƨ@�\)@��@�@��R@�~�@�5?@�J@���@�x�@�O�@�?}@�%@���@��9@���@��@�j@�Z@�1'@��@�  @��
@�t�@�o@���@�ȴ@��!@�^5@�E�@�-@�{@��^@�?}@���@��@�bN@�(�@~��@}�@|��@|�@|I�@|Z@|�j@|��@{�m@{ƨ@{�
@{�m@{��@y�^@yX@y&�@y�@x��@x�9@x�@xQ�@x  @x  @w��@w\)@v��@v$�@u�-@u�h@u?}@t�@tZ@t1@s�@r��@rn�@r=q@rJ@q�#@qX@o�@oK�@n�R@nv�@m@m`B@m?}@l�@l��@lZ@k��@k��@kC�@j�!@j��@jM�@i�7@h�`@h�9@hr�@h  @g�@gl�@g+@g�@f��@f��@f��@f5?@ep�@e�@d�/@d�D@dI�@dI�@c�m@cdZ@c33@co@bn�@a��@ax�@ahs@aX@aG�@a&�@`Ĝ@` �@_�;@_�;@`  @_�@_��@_��@_+@^�y@^�R@^$�@]p�@\�@\�D@\j@\I�@\1@[�m@[�m@[��@["�@Z��@Z��@Y��@XbN@W�@W�P@W�@V��@V�@Vȴ@V�R@V��@V�+@Vff@V$�@U@U��@U/@T��@T�/@T�D@T�@S�F@So@S@R��@R�!@R~�@R�@Q�#@Qhs@Q7L@Q7L@Q&�@P��@P  @O�@O�P@O\)@O+@N�R@N{@M@Mp�@M/@L��@L��@Lj@LI�@L9X@LI�@L9X@L(�@K��@Kƨ@KS�@Ko@J�\@JM�@JJ@I��@IX@I7L@I%@H�`@H�9@Gl�@Fȴ@FV@E��@E?}@D��@D�@C@B��@BJ@Ahs@A%@@��@@��@@�u@@bN@?��@?|�@?
=@>v�@>E�@=@=p�@=O�@<�@<��@<j@<�@<1@;�
@;�F@;�@;C�@:�H@:��@:~�@:-@9��@9�@9��@9G�@8Ĝ@8�u@8�@8bN@81'@7�@7�w@7��@7+@6�R@6v�@6{@5��@5V@4�j@4�@4�@4��@4z�@4z�@4j@4(�@4�@3�m@3�F@3��@3dZ@3@2�@1��@1�7@1&�@0Ĝ@0��@0r�@01'@0 �@/�;@/�P@.��@.{@.{@.@-�h@,�@,z�@,I�@+�F@+C�@*��@*=q@)�@)�#@)�#@)�#@)�^@)�^@)�7@)x�@)hs@)7L@(Ĝ@(��@(�@(A�@'�;@'�P@'K�@'
=@&�@&�R@&V@&@%��@%�-@%��@%�@%V@$��@$9X@#�m@#��@#�@#t�@#dZ@#S�@#"�@#@#@"�H@"��@"��@"��@"�\@"~�@"n�@"^5@"�@!��@!x�@!&�@ ��@ �u@  �@�;@��@�w@�w@�@��@�P@\)@��@v�@V@E�@{@��@@@�h@O�@/@��@�D@I�@9X@��@��@��@�@S�@33@�@~�@�@��@hs@X@G�@G�@��@��@��@bN@A�@1'@ �@ �@b@  @�;@�P@\)@��@v�@E�@$�@��@p�@��@��@��@(�@��@�
@�
@�
@ƨ@�@S�@@�!@��@n�G�O�A�1A�1A�%A�JA�oA��A�{A��A�{A�JA�{A�oA�bA��A��A�oA�{A��A�oA��A�oA�1A��A��A�{A��A��A��A� �A�$�A� �A� �A�&�A�"�A�"�A�$�A�$�A� �A�$�A�$�A� �A� �A�"�A�"�A��A�"�A�"�A��A� �A�$�A� �A��A� �A�$�A�"�A� �A�$�A�$�A��A� �A�$�A�$�A�"�A�$�A�&�A� �A�"�A�&�A�"�A��A�$�A�"�A��A�$�A�&�A�"�A� �A� �A�$�A�$�A�"�A�$�A�&�A�"�A��A� �A�$�A�"�A� �A�"�A�&�A�$�A�$�A�+A�&�A�$�A�&�A�+A�(�A�$�A�+A�(�A�$�A�+A�+A�&�A�(�A�-A�+A�&�A�+A�-A�&�A�(�A�-A�-A�(�A�(�A�/A�-A�(�A�+A�/A�-A�+A�/A�1'A�+A�-A�1'A�/A�-A�/A�1'A�-A�/A�33A�-A�+A�1'A�+A�-A�33A�/A�-A�33A�/A�-A�-A�/A�33A�/A�-A�1'A�1'A�-A�1'A�1'A�/A�33A�1'A�/A�1'A�33A�/A�1'A�5?A�/A�/A�5?A�33A�/A�1'A�5?A�1'A�/A�1'A�5?A�33A�/A�1'A�5?A�33A�/A�1'A�5?A�1'A�1'A�5?A�5?A�1'A�1'A�5?A�5?A�33A�1'A�5?A�7LA�33A�1'A�5?A�7LA�33A�33A�7LA�7LA�33A�5?A�9XA�9XA�33A�5?A�9XA�9XA�5?A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�5?A�33A�A�1A�A���A���A�n�A�-A�
=A��;A�ƨA׮Aץ�Aק�Aף�Aח�A׉7A׋DA׉7A׃AׁA�z�A�l�A�jA�l�A�l�A�^5A�E�A�7LA��A�  A���A��A��mA�ƨA���A���AֶFA֮Aֲ-Aִ9A֮A֮Aֲ-Aֲ-A֣�A֓uA�|�A�`BA�M�A�?}A�/A��A�bA�A���A��A��A��yA��HA��HA��#A�ȴA�Aպ^AնFAծAէ�A�t�A�G�A�A�A�=qA�C�A�E�A�A�A�?}A�A�A�C�A�9XA�-A�%A��A��mA��;A��#A��/A���AԾwAԩ�Aԙ�A�t�A�O�A�G�A�G�A�G�A�9XA��A���A��mA��
AӮAӑhA�x�A�l�A�dZA�\)A�E�A�(�A�bA��A��
Aҩ�A�jA� �A��TA��/A���A���A�ĜAѡ�A�|�A�r�A�VA��A��HAЩ�A�hsA�I�A�7LA�33A�33A��A�
=A��Aϧ�AρA�E�A��AΣ�A�O�A�VAͰ!A�bNA�/A�oA���A��A��TA���A̩�A̕�ȂhȂhA̋DA�~�A�x�A�r�A�l�A�S�A�E�A�A�A�1'A��#AˬAˇ+A�l�A�
=A��HAʝ�A�A�A���A��;A�ȴA�|�A�VA�(�A� �A� �A�
=A�$�A�K�A���A�C�A�1'A�(�A� �A��A�VA�
=A��AżjAŶFAŝ�Aş�Aţ�Ať�AŁA�l�A�ZA�Q�A�7LA���A�l�A�C�A�7LA� �A�bA�%A��A��`A��/A���A���A���AöFA×�AÅA�?}A�(�A�{A�bA�
=A�  A���A���A���A��A���A�x�A��+A���A�I�A��
A���A���A��uA�t�A�1'A�{A�%A���A��A��mA��HA��;A��wA�z�A�5?A��`A�ĜA��wA���A��A�9XA��PA�JA�A�t�A�^5A�C�A�(�A�$�A��A�1A��A���A��^A�9XA�x�A�A�A�{A��A��hA�ZA�7LA��A�{A�oA�1A��A���A��A�VA���A��jA���A�^5A��
A�p�A�A���A�n�A�dZA�^5A�\)A�O�A�C�A�5?A��A�JA�
=A��A��A�{A�%A��A��A���A���A��A�A�n�A�  A��yA��/A���A��FA�x�A�O�A�K�A�=qA���A��PA�~�A�
=A��9A��+A��DA�K�A���A�JA���A�z�A�dZA�M�A�?}A�/A�"�A��A��A��7A�^5A��HA��A�1A���A���A�ƨA�\)A��
A�bNA��A��wA��+A�E�A��mA���A�r�A��A�?}A���A�t�A�ffA�=qA��A�A���A��
A�=qA���A�p�A�E�A�JA��yA���A��A��RA��yA��uA��yA�O�A��
A�`BA�%A���A��;A��FA���A�t�A�M�A�
=A���A�5?A��FA��uA��DA�p�A�?}A���A���A�n�A���A��A��A��A�`BA���A�ZA�A�A��A�A��A��;A���A���A��A�VA�(�A��#A��7A��HA�ffA�1'A�ȴA�hsA�+A��TA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�<B
��B
�<B
��B
��B
�qB
��B
��B
��B
��B
B
�aB
�aB
B
�'B
�[B
�UB
�aB
�aB
�aB
�aB
�aB
�aB
��B
ÖB
�aB
ÖB
ÖB
�aB
�aB
�-B
��B
��B
�HB
�qB
��B
��B
�B
��B
��B
��B
��B
��B
�\B
�B
��B
��B
��B
��B
��B
�!B
�uB
�%B
z�B
�B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�BBBB(�B7LBJ�BUgBy�B�$B��B�B �B�cBAB�"B�B�.B��B�]B�B��B� B�B�B͟B�aBϫB��B�$B��B�CB��B|PBm�Bd&BL�BHB"4B BIBB�B�BkB
�TB
�mB
��B�B
��B
� B
��B
��B
��B
�YB
cB
xB
n/B
g8B
\�B
FB
#:B
xB	�fB	�B	�B	�KB	ɺB	��B	�qB	�hB	��B	�_B	��B	�AB	y�B	u�B	rB	iyB	a|B	YB	V9B	QB	D�B	@OB	:�B	<jB	1�B	 �B	�B	�B	�B	VB	�B	�B	�B	PB	�B	�B	VB	DB	�B�]B�xB�B�8B	B	B	
�B	B��B�B�B�DB�B��B��B��B�B�yB�B�"B�B�;B�]B��B�B�B��B	�B	_B	JB	(B	\B	�B	�B	qB	�B	qB	%B	+kB	)�B	&�B	%FB	%FB	#�B	#�B	!bB	!-B	B	�B	�B	hB	bB	
=B	�B	B	�B		7B	�B	hB	�B	FB	�B	�B	 �B	($B	(�B	(�B	'�B	.�B	5�B	B�B	RTB	e�B	h>B	g8B	g�B	f2B	f�B	gB	g�B	h�B	m�B	q�B	tB	z�B	~�B	�uB	�B	�DB	��B	�=B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�"B	�~B	��B	��B	�6B	��B	�UB	��B	�zB	��B	�tB	�aB	��B	��B	�!B	��B	�B	��B	�B	�aB	�9B	רB	�EB	�B	�)B	�B	�B	�sB	�KB	��B	�|B	��B	��B	��B	�`B	�	B	��B	��B
 4B
 �B	��B
�B
AB
�B
AB
�B
B
AB
�B
oB
oB
�B
�B
�B
	B
	7B
	7B
	�B

=B
B

�B
xB

�B

�B
�B
B
1B
�B
	lB
xB
�B
B
~B
�B
(B
�B
B
B
�B
�B
B
{B
�B
B
�B
MB
�B
SB
�B
�B
�B
�B
uB
:B
B
�B
	B
kB
�B
�B
�B
IB
�B
�B
�B
�B
�B
�B
�B
OB
�B
xB
B
B
B
xB
�B
#B
#B
#B
#:B
"�B
#:B
$tB
#�B
"�B
#B
&LB
&B
%�B
%B
(XB
'RB
&�B
%�B
%�B
%�B
%�B
&B
%�B
%�B
%�B
$�B
$�B
$B
%zB
%�B
%�B
$�B
$tB
$@B
$@B
%FB
$�B
%�B
&LB
&�B
'RB
)*B
+�B
+�B
+�B
-CB
-�B
.}B
.�B
.�B
/�B
0!B
1�B
1�B
2-B
1�B
2aB
1�B
2�B
4B
3�B
4�B
5�B
5?B
5�B
6B
8B
8�B
9�B
9�B
9�B
9XB
9�B
:�B
:�B
:�B
:�B
:�B
9�B
9�B
:^B
:�B
<�B
>wB
>wB
@OB
B[B
C-B
C-B
B�B
D3B
D�B
EB
E�B
E�B
E�B
F�B
GEB
GEB
G�B
G�B
HB
H�B
H�B
IB
IRB
I�B
I�B
J�B
JXB
J�B
J�B
L�B
L�B
M6B
MB
MB
L�B
LdB
L�B
L�B
L�B
K�B
M6B
M6B
MB
M6B
MB
MB
MB
M�B
M�B
M�B
M�B
NpB
OBB
N�B
N�B
OB
NpB
N�B
N�B
N�B
OBB
OvB
O�B
OvB
OBB
OBB
OB
OB
P}B
P�B
P}B
P�B
PHB
O�B
Q�B
QB
Q�B
Q�B
QNB
R B
Q�B
Q�B
R B
R�B
TaB
U2B
U2B
U�B
W�B
XB
YB
YKB
ZQB
Z�B
Z�B
Z�B
[WB
[WB
[�B
[WB
[�B
[�B
[�B
\)B
[�B
\]B
\]B
]dB
]dB
]dB
]�B
^jB
^�B
^�B
_�B
_�B
`BB
_B
]�B
\�B
\�B
[�B
]/B
Z�B
ZQB
Z�B
ZQB
Z�B
[�B
^B
\�B
\]B
\)B
\)B
\�B
[�B
[WB
[WB
[�B
\]B
\�B
\�B
]/B
]/B
]�B
_B
`BB
`BB
aHB
aB
`�B
a|B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
bNB
b�B
d&B
d�B
e`B
e`B
ffB
f�B
f�B
gB
gB
g8B
g�B
g�B
h
B
h>B
h>B
h>B
iDB
iDB
iyB
iDB
i�B
jB
jB
j�B
j�B
kQB
k�B
kQB
k�B
k�B
kQB
k�B
k�B
k�B
m)B
m�B
n�B
n�B
n�B
pB
p�B
p�B
p�B
qAB
qB
qB
qAB
q�B
qvB
q�B
rGB
s�B
tB
t�B
u�B
v+B
v`B
v�B
v�B
v�B
wfB
w�B
w�B
xlB
x�B
xlB
x�B
y	B
y>B
y>B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
{B
{B
{JB
{JB
{B
{�B
|�B
|�B
|�B
|�B
}VB
}�B
~(B
}�B
}�B
~]B
�4B
�4B
�4B
�4B
�4B
��B
��B
��B
��B
�;B
��B
��B
��B
�B
�B
�uB
��B
�B
�{B
��B
�B
�B
�MB
�MB
��B
�MB
�MB
�MB
�MB
�MB
��B
��B
��B
��B
��B
�YB
��B
��B
�+B
��B
��B
��B
�B
�B
�7B
�lB
��B
�7B
��B
��B
�fB
�fB
�B
�7B
�7B
�7B
��B
�rB
�=B
��B
��B
��B
�DB
�xB
�xB
�B
�B
�~B
�B
�PB
��B
��B
�"B
�VB
��B
��B
�(B
�\B
�(B
�(B
�\B
��B
�.B
�bB
�bB
��B
��B
� B
� B
� B
�4B
�4B
��B
��B
�:B
��B
�:B
�:B
�:B
�:B
��B
�oB
��B
��B
�B
�@B
�@B
�@B
�uB
��B
�{B
�{B
�{B
�B
�B
�MB
�MB
��B
��B
��B
�B
�YB
��B
�YB
�YB
��B
�_B
��B
��B
��B
�kB
�	B
�=B
�=B
�=B
�=B
�=B
�qB
�qB
��B
�qB
�qB
��B
�B
�B
�B
�xB
��B
��B
��B
�B
��B
��B
�~B
�~B
��B
�~B
�~B
��B
�OB
��B
��B
�!B
�!B
�VB
�VB
�VB
�VB
��B
��B
��B
��B
��B
��B
�'B
��B
�'B
�'B
�'B
��B
��B
��B
�-B
�-B
��B
�4B
��B
��B
��B
��B
��B
��B
��B
��B
�nB
��B
��B
��B
�B
�tB
�@B
�@B
��B
��B
�B
�zB
�B
�B
�B
��B
��B
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
�_B
�_B
�_B
��B
��B
��B
��B
��B
��B
��B
��B
�0B
�0B
��B
�B
�6B
�6B
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
�wB
�CB
�}B
�}B
�}B
��B
�}B
��B
��B
�HB
��B
��B
�qB
� B
�B
��B
�B
��B
�6B
�B
��B
��B
�wB
��B
�B
�}B
�B
�<B
�*B
��B
��B
��B
�qB
�B
�'B
��B
�OB
ÖB
ÖB
��B
��B
�aB
�'B
�[B
ÖB
B
��B
�aB
��B
B
��B
�3B
��B
��B
��B
��B
��B
ÖB
�gB
ÖB
�'B
�-B
�gB
��B
��B
��B
��B
��B
��B
��B
ÖB
��B
�3B
�-B
� B
�-B
�-B
�UB
� B
��B
��B
�UB
��B
�-B
ÖB
��B
�[B
�gB
ÖB
� B
�UB
��B
��B
�B
�'B
�[B
��B
�HB
��B
�3B
�'B
�aB
�gB
ÖB
�[B
�-B
�gB
�[B
��B
ĜB
��B
B
ĜB
��B
�[B
�aB
�gB
��B
B
�gB
�3B
B
B
�3B
��B
�[B
��B
�gB
ÖB
�[B
�aB
�3B
�-B
B
�3B
��B
�[B
�aB
��B
�aB
�[B
ÖB
��B
B
�3B
��B
B
�gB
�3B
B
�aB
�gB
B
ÖB
�3B
��B
�-B
�'B
�aB
�gB
��B
B
�3B
�[B
�aB
�3B
B
ÖB
��B
��B
��B
ĜB
��B
�[B
�3B
�gB
�[B
�-B
ĜB
��B
�[B
ÖB
��B
��B
B
��B
�gB
�3B
�'B
�-B
�gB
��B
B
�3B
�3B
B
��B
�gB
��B
�[B
�[B
��B
�3B
��B
��B
�aB
��B
B
��B
�aB
�aB
��B
��B
�-B
��B
�UB
��B
�-B
B
��B
��B
��B
��B
�OB
�HB
��B
��B
�wB
�<B
�qB
�qB
��B
ȴB
�-B
��B
��B
�B
�kB
��B
��B
��B
�4B
�4B
��B
��B
��B
��B
�'B
��B
�~B
��B
��B
��B
��B
�B
��B
�qB
��B
�OB
��B
��B
��B
�B
��B
�kB
�=B
�kB
��B
��B
�\B
�OB
��B
��B
��B
��B
�VB
��B
��B
�zB
�hB
�B
�nB
��B
��B
��B
��B
�-B
��B
��B
��B
��B
�B
�OB
�bB
��B
�B
�xB
��B
�~B
�zB
��B
�-B
�bB
��B
��B
��B
�4B
��B
�'B
��B
��B
�*B
�B
�-B
��B
�bB
�VB
�\B
�hB
�:B
��B
�B
�B
��B
��B
�OB
��B
��B
�!B
��B
��B
��B
�YB
��B
�(B
�"B
�B
��B
�JB
��B
�B
�oB
~�B
��B
v�B
wfB
v�B
y>B
z�B
}�B
�SB
cB
}�B
~�B
��B
�+B
�B
�B
��B
��B
�B
�GB
�+B
��B
�SB
�xB
�SB
��B
��B
��B
��B
�+B
�.B
�fB
�YB
��B
��B
�B
��B
��B
�1B
�fB
��B
�B
��B
��B
��B
��B
�MB
��B
�{B
�B
��B
��B
�DB
��B
��B
��B
�4B
��B
�CB
�=B
�=B
��B
�zB
��B
��B
��B
��B
��B
�B
�NB
�;B
�B
�B
��B�B�B�B%B
rB�B�B BbBoB�B \B$�B%zB#B+�B-�B/�B0�B1[B5B5�B5�B<�BA�BA�B?}B?BAUBMjBQ�BU2B\)B[#BW?BT�BS&BS�BR�BQNBQNBO�BX�Bm�B��B�4B�OB�[B�B��B�B��B�}B��B�RB��B��B��B��B�B��B�[B�gB�RB��BʌB�B��B�&BB�B�xB�B��B�BoB�(B  B �B�B�.B��B{B��B�lB�xB�BuB��BoB�(B��B��B�PB�VB�(B�B
rB��B�JB�B{BJB�B�B��B��B�B�B�B��B�TB��B�fB�B��B�B��B�B�B�BB%B{B�B=B�B��B�GB�5B�)B�|B�%B��B��B�B�B�B� B��B�
B�)B��B�GB�vB�BYB�B�gB�NB�aBуB�B�yB�B�mB��BԕB�ZB�tB��B�BȴBΥB�B�6B��B��B��B�XB�0B��B��B�LB��B��B��B��B�:B��B�=B��B�\B��B��B��B�uB�VB�lB�AB��B��B��Bx8B�{B~�Bv`B{Bx8BkQBiDBl�Bf�Bf�Bc Be�Bd�B^�Be,BK)BHKBHBH�Bc�BG�B?}BO�B7�B,�BOBeB7�BeB�B{B�B�B.B�B{B�B�B=B*�B.IBYKB=�B=B?�B3�B+B($B"�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                       G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021090523252120210905232521IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092016012420210920160124QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092016012420210920160124QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365220220126093652IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295420220204232954IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                