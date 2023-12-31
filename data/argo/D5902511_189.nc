CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-16T08:49:27Z creation; 2022-02-04T23:30:05Z DMQC;      
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20211116084927  20220204223519  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_189                 6810_008521_189                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @٣W�i�@٣W�i�11  @٣���$@٣���$@0)z�N��@0)z�N���dO灃��dO灃�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@}p�@�p�@�p�@�p�@�p�A\)A\)A,(�A@��Aa�A���A���A�Q�A�  A�Q�AУ�A��A�\)A�\)B  B(�B(�B   B(  B0(�B8Q�B@Q�BG�
BO�BXQ�B`Q�Bg�Bo�
Bx  B�  B�{B�{B��B��B��B�  B�{B��B��
B�  B�  B��B��B�{B��B��
B��B�  B�  B�{B�(�B�(�B�  B�{B�{B��B��B��
B�  B�{B�=qC 
=C��C  C
=C  C
  C��C  C  C  C
=C��C��C  C  C
=C {C!��C$  C&  C(  C*  C,  C.
=C0
=C2
=C4
=C6
=C8{C:
=C<
=C>
=C@
=CB  CD
=CF
=CH  CJ
=CL
=CN  CP
=CR
=CT  CV
=CX
=CZ  C[�C]�C`  Cb
=Cc�Ce��Ch  Ci��Cl
=Cn
=Cp
=Cr{Ct  Cu��Cw��Cy��C{��C}��C�  C�C�  C���C���C���C�  C���C���C�C�  C�  C�C���C�  C�  C�  C���C���C�  C�
=C���C���C���C���C�C�C�C�C�  C�  C���C���C�C���C�C�  C���C���C���C�C�  C���C���C���C�C�
=C�
=C�C�  C�C�C�  C���C�C�
=C���C�  C�  C���C�
=C���C���C���C�C�  C�  C���C�  C�C�C���C���C�  C�  C�  C�  C���C�  C���C���C���C�  C���C�  C�
=C�
=C�  C�  C�  C�
=C�C�C�C�  C�  C�C���C���C�  C�  C���C�  C�C�C�C���C���C�  C�C�C�C�  C�  C���C�  C�  C�  C���C���C�  C���C���C���C���C���C�  C�D   D }qD�D��D�D�D  D��D�D��DD� D��D}qD�qD}qD��Dz�D	  D	� D	�qD
� D  D��D�qD� D�D� D  D� D  D� D  D��D�D��D�qD� D  D}qD  D��D  D� D  D}qD�qD}qD�D��D�D}qD�qD}qD�qD}qD��Dz�D��D� DD��D  D}qD�qD }qD ��D!}qD"  D"��D"�qD#z�D#�qD$}qD$�qD%� D&�D&� D'  D'��D(�D(��D)�D)}qD)��D*z�D*��D+}qD,  D,� D-�D-}qD.  D.�D/�D/��D/�qD0xRD0�qD1� D1�qD2}qD3  D3��D4  D4}qD5  D5��D6�D6}qD6�qD7��D7�qD8z�D8�RD9}qD:�D:��D;  D;z�D;��D<xRD<�qD=}qD=�qD>�D?  D?� D@�D@��DA�DA�DB�DBz�DC  DC� DD�DD� DE  DE}qDE�qDF}qDF��DG� DH�DH� DH�qDI��DJ�DJ��DK�DK� DL�DL�DM  DM}qDM�qDN}qDN�qDO}qDP  DP��DQ�DQ� DR  DR��DSDS�DT  DT��DUDU��DV�DV� DW�DW}qDX  DX� DX��DYz�DY��DZ� D[�D[� D\  D\}qD\�qD]z�D^  D^�D_  D_� D`�D`z�D`��Da� Db  Db� Dc  Dc� Dd  Dd}qDd�qDe� Df  Df� Df�qDg��Dh�Dh��DiDi� Di�qDj�Dk�Dk� Dl  Dl� Dm  Dm� Dm�qDn}qDn��Do}qDp�Dp}qDq�Dq�Dr  Dr}qDr�qDs}qDt�Dt� Dt��Du}qDu�qDv� Dw�Dw� Dx  Dx� Dy  Dy��Dz�Dz}qDz��D{z�D{�qD|� D}  D}}qD~  D~}qD�D�D��D�AHD�� D��HD�  D�=qD�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D��HD�HD�B�D��HD���D���D�AHD��HD�� D��D�AHD�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D��qD���D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D���D�@ D��HD���D���D�@ D�� D��qD��qD�@ D��HD��HD���D�@ D���D��HD�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D���D���D�AHD�� D���D�  D�B�D���D��HD�  D�>�D�� D�D�  D�=qD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�@ D��HD���D���D�@ D��HD���D���D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D�� D�HD�@ D�}qD�� D�  D�AHD�� D���D���D�>�D�}qD���D�HD�AHD�� D���D��qD�@ D�� D���D���D�@ D�� D��HD���D�=qD�}qD��qD���D�@ D�� D�� D��D�AHD�� D�� D�HD�B�D���D��HD�HD�AHD��HD�� D��D�AHD�� D��qD�  D�B�D���D�D�HD�>�D�� D�D�HD�AHD��HD��HD�HD�AHD�~�D��)D���D�@ D�� D�D�HD�@ D�� D��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�=qD�~�D��HD�HD�AHD�� D��qD�  D�@ D��HD���D�  D�@ D�~�D��HD�  D�AHD��HD���D�  D�>�D�}qD���D�  D�AHD��HD�� D���D�=qD�� D��HD�HD�AHD��HD��HD���D�AHD D¾�D�HD�@ DÀ D��HD�  D�>�DĀ D��HD�HD�>�Dŀ D�� D���D�>�Dƀ D��HD�HD�>�D�~�DǽqD���D�AHDȂ�D��HD���D�>�Dɀ DɽqD��qD�@ Dʀ D�� D�HD�AHDˁHD��HD��D�AHD�~�D̽qD�  D�B�D͂�D�� D���D�>�D�~�Dξ�D���D�@ DρHD�� D���D�AHDЁHD��HD��D�AHDр DѾ�D�  D�>�DҀ D��HD���D�>�DӀ D��HD�HD�B�DԀ DԾ�D�  D�@ DՀ D�� D�HD�AHD�}qDֽqD��qD�>�D�~�D�� D�HD�@ D�~�DؽqD��qD�>�Dـ D�� D�  D�@ Dڀ D�� D�  D�AHDۀ D۾�D���D�B�D܀ Dܾ�D���D�>�D݁HD�� D��qD�>�D�~�D޾�D���D�>�D�~�D�� D��D�@ D�� DྸD���D�@ D�HD�D�HD�>�D�~�D⾸D�HD�AHD�HD�D�  D�>�D�HD��HD��qD�=qD�~�D�� D�HD�@ D� D��HD�  D�@ D�~�D羸D���D�>�D�~�D�� D�HD�AHD邏D�D�  D�AHDꂏD��HD��D�C�D낏D�� D�  D�AHD��D�D���D�>�D�~�D��qD��)D�>�D� DD��D�C�D�HD�� D�  D�=qD��HD�� D���D�@ D�HD��HD�HD�@ D�~�D�?�?#�
?u?�{?���?��@\)@!G�@.{@J=q@Y��@k�@��\@�{@�@��R@���@�
=@��R@���@�
=@޸R@�=q@�z�@�(�Az�AQ�A��A33AA(�A ��A$z�A+�A/\)A333A8Q�A>�RAB�\AH��AN{AQG�AXQ�A^{Ab�\Ah��Amp�Ar�\Ax��A~{A�G�A�(�A�
=A���A�(�A�ffA���A�(�A�ffA���A��
A�ffA���A��
A��RA���A��HA�ffA�G�A�33A�ffA�G�A�33A�ffA�G�AÅA�{A�G�A˅A�{Aљ�A�(�A�ffA�G�A���A޸RAᙚA���A�\)A陚A��A�  A��A���A�Q�A�=qA���B (�Bp�B�RBQ�BB�RB(�B	�B
�HBQ�B�B
=B(�B�B33BQ�B�B\)BQ�BB
=B(�B�B
=B (�B!�B#
=B$(�B%B'\)B(Q�B)��B+33B,z�B-��B/\)B0z�B1��B333B4��B5�B733B8��B:=qB;\)B<��B>=qB?\)B@z�BB{BC�BDz�BEBG\)BHz�BI��BK33BL��BM��BO
=BP��BQ��BR�HBTz�BU�BW
=BX(�BYB[
=B\  B]B_
=B`(�Bap�Bc
=Bd(�BeG�Bf�RBhQ�BiG�Bj�\Bl(�BmG�Bn=qBp  BqG�BrffBs�
BuG�Bv=qBw�
ByG�Bz=qB{�B}G�B~=qB�B���B��B�B��\B��B���B�Q�B�
=B���B�=qB�
=B��B�(�B���B�p�B�  B��RB��B�  B���B�p�B�{B��\B�G�B�  B��\B�
=B��
B��\B�
=B��B��\B���B���B�z�B���B�p�B�{B��HB��B�  B���B�p�B�  B�z�B�33B��B�Q�B��HB��B�Q�B���B�\)B�{B���B��B��
B��\B�
=B���B�ffB�
=B�p�B�{B��HB�p�B��B��\B�\)B��B�ffB���B�B�z�B���B��B�=qB��HB�G�B�(�B��RB�33B��B��RB�33B�B�z�B�33B�B�=qB�
=B���B�{B��HB���B�{B���B�\)B�{B��\B�33B�  B��\B�
=B���B�z�B�
=BÅB�=qB���B�p�B�  BƸRB�p�B�{Bȏ\B��B��B�z�B�
=B˙�B�Q�B�
=B͙�B�(�B��HBϙ�B�(�BУ�B�G�B�  Bң�B��B��
Bԏ\B���Bՙ�B�Q�B���BׅB�  B���B�p�B��B�z�B�
=B��
B�ffB���B�p�B�(�B���B�33B��
B��\B�G�B�B�=qB�
=B�B�(�B��B�G�B��B�ffB��HB�B�=qB�\B�
=B�B�ffB���B�\)B�{B���B�G�B��B�Q�B�
=B�B�(�B��B�\)B�{B��B�
=B�B�ffB�
=B��B��B�ffB�
=B��B�(�B��\B�
=B��B�(�B��\B��HB�G�B�B�=qB�z�B��RB�33B���B��
B�  B�Q�B���B�
=B�33B�p�B��C {C (�C Q�C �C �C C �HC
=C=qCQ�Cp�C�C�
C�HC
=C=qCp�Cz�C��C�HC
=C�CG�Cz�C��C�RC�HC�C33CG�Cz�C�C�
C�C�CffC�C��C�HC{C33CQ�C�\C��C�HC�C\)Cz�C��C��C  C=qC\)Cz�CC��C	�C	G�C	p�C	�RC	�C
  C
33C
p�C
��C
�RC
�C(�C\)Cz�C��C�C{C33CffC��C�HC
=C(�C\)C�\C��C
=C(�CG�C��CC�HC{CQ�C�\C�C�
C{CQ�Cz�C��C�HC{C33Cp�C�RC�HC
=C=qC�\C�C�HC�CffC��C�RC��C33Cp�C��CC
=CQ�Cz�C��C�HC(�C\)C�C�RC  CG�Cz�C��C�HC(�CQ�C�C�
C  C33Cz�C�C�
C
=CQ�C�C�C�
C�C\)C�C��C�C33CffC�C�RC
=C33C\)C��C�HC
=C33Cp�C�C�HC  CG�C�C��C�HC �C G�C p�C �RC �C!
=C!G�C!�C!�RC!�
C"
=C"Q�C"�C"��C"�
C#�C#=qC#ffC#��C#�HC${C$33C$\)C$�\C$�
C$��C%{C%Q�C%�\C%C%�HC&
=C&Q�C&p�C&��C&�
C'{C'=qC'\)C'�\C'��C'��C({C(Q�C(�\C(�RC(�HC){C)G�C)ffC)�\C)C*  C*=qC*\)C*z�C*�RC*��C+{C+=qC+z�C+�C+�
C,  C,(�C,ffC,��C,�RC,�HC-{C-\)C-�\C-��C-�
C.{C.Q�C.�C.��C.��C/
=C/G�C/�C/�C/�
C0
=C0G�C0z�C0�C0�
C1{C1\)C1��C1�RC1�HC2�C2\)C2��C2C2�C3�C3ffC3��C3C3�C433C4z�C4��C4��C4��C5=qC5z�C5�C5�
C6
=C6\)C6�\C6C6�C733C7p�C7�C7��C8  C8G�C8�C8�RC8�HC9{C9Q�C9��C9��C9��C:�C:p�C:�C:��C;
=C;Q�C;�\C;C;�C<(�C<p�C<�C<�
C=  C==qC=z�C=C=��C>�C>\)C>��C>�
C?  C?(�C?ffC?�C?�C@{C@G�C@z�C@�RCA  CA33CAffCA�\CA�HCB�CBG�CBp�CB��CB�CC(�CCffCC��CCCD  CDQ�CD�\CD�RCD�CE�CEffCE��CE�HCF{CFG�CFp�CF�CF��CG33CGffCG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ?�  @   @=p�@}p�@�p�@�p�@�p�@�p�A\)A\)A,(�A@��Aa�A���A���A�Q�A�  A�Q�AУ�A��A�\)A�\)B  B(�B(�B   B(  B0(�B8Q�B@Q�BG�
BO�BXQ�B`Q�Bg�Bo�
Bx  B�  B�{B�{B��B��B��B�  B�{B��B��
B�  B�  B��B��B�{B��B��
B��B�  B�  B�{B�(�B�(�B�  B�{B�{B��B��B��
B�  B�{B�=qC 
=C��C  C
=C  C
  C��C  C  C  C
=C��C��C  C  C
=C {C!��C$  C&  C(  C*  C,  C.
=C0
=C2
=C4
=C6
=C8{C:
=C<
=C>
=C@
=CB  CD
=CF
=CH  CJ
=CL
=CN  CP
=CR
=CT  CV
=CX
=CZ  C[�C]�C`  Cb
=Cc�Ce��Ch  Ci��Cl
=Cn
=Cp
=Cr{Ct  Cu��Cw��Cy��C{��C}��C�  C�C�  C���C���C���C�  C���C���C�C�  C�  C�C���C�  C�  C�  C���C���C�  C�
=C���C���C���C���C�C�C�C�C�  C�  C���C���C�C���C�C�  C���C���C���C�C�  C���C���C���C�C�
=C�
=C�C�  C�C�C�  C���C�C�
=C���C�  C�  C���C�
=C���C���C���C�C�  C�  C���C�  C�C�C���C���C�  C�  C�  C�  C���C�  C���C���C���C�  C���C�  C�
=C�
=C�  C�  C�  C�
=C�C�C�C�  C�  C�C���C���C�  C�  C���C�  C�C�C�C���C���C�  C�C�C�C�  C�  C���C�  C�  C�  C���C���C�  C���C���C���C���C���C�  C�D   D }qD�D��D�D�D  D��D�D��DD� D��D}qD�qD}qD��Dz�D	  D	� D	�qD
� D  D��D�qD� D�D� D  D� D  D� D  D��D�D��D�qD� D  D}qD  D��D  D� D  D}qD�qD}qD�D��D�D}qD�qD}qD�qD}qD��Dz�D��D� DD��D  D}qD�qD }qD ��D!}qD"  D"��D"�qD#z�D#�qD$}qD$�qD%� D&�D&� D'  D'��D(�D(��D)�D)}qD)��D*z�D*��D+}qD,  D,� D-�D-}qD.  D.�D/�D/��D/�qD0xRD0�qD1� D1�qD2}qD3  D3��D4  D4}qD5  D5��D6�D6}qD6�qD7��D7�qD8z�D8�RD9}qD:�D:��D;  D;z�D;��D<xRD<�qD=}qD=�qD>�D?  D?� D@�D@��DA�DA�DB�DBz�DC  DC� DD�DD� DE  DE}qDE�qDF}qDF��DG� DH�DH� DH�qDI��DJ�DJ��DK�DK� DL�DL�DM  DM}qDM�qDN}qDN�qDO}qDP  DP��DQ�DQ� DR  DR��DSDS�DT  DT��DUDU��DV�DV� DW�DW}qDX  DX� DX��DYz�DY��DZ� D[�D[� D\  D\}qD\�qD]z�D^  D^�D_  D_� D`�D`z�D`��Da� Db  Db� Dc  Dc� Dd  Dd}qDd�qDe� Df  Df� Df�qDg��Dh�Dh��DiDi� Di�qDj�Dk�Dk� Dl  Dl� Dm  Dm� Dm�qDn}qDn��Do}qDp�Dp}qDq�Dq�Dr  Dr}qDr�qDs}qDt�Dt� Dt��Du}qDu�qDv� Dw�Dw� Dx  Dx� Dy  Dy��Dz�Dz}qDz��D{z�D{�qD|� D}  D}}qD~  D~}qD�D�D��D�AHD�� D��HD�  D�=qD�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D��HD�HD�B�D��HD���D���D�AHD��HD�� D��D�AHD�~�D���D�  D�>�D�~�D��HD�HD�AHD�� D��qD���D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�HD�@ D�� D�� D���D�@ D��HD���D���D�@ D�� D��qD��qD�@ D��HD��HD���D�@ D���D��HD�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D���D���D�AHD�� D���D�  D�B�D���D��HD�  D�>�D�� D�D�  D�=qD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�@ D��HD���D���D�@ D��HD���D���D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D�� D�HD�@ D�}qD�� D�  D�AHD�� D���D���D�>�D�}qD���D�HD�AHD�� D���D��qD�@ D�� D���D���D�@ D�� D��HD���D�=qD�}qD��qD���D�@ D�� D�� D��D�AHD�� D�� D�HD�B�D���D��HD�HD�AHD��HD�� D��D�AHD�� D��qD�  D�B�D���D�D�HD�>�D�� D�D�HD�AHD��HD��HD�HD�AHD�~�D��)D���D�@ D�� D�D�HD�@ D�� D��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�HD�AHD�� D���D���D�=qD�~�D��HD�HD�AHD�� D��qD�  D�@ D��HD���D�  D�@ D�~�D��HD�  D�AHD��HD���D�  D�>�D�}qD���D�  D�AHD��HD�� D���D�=qD�� D��HD�HD�AHD��HD��HD���D�AHD D¾�D�HD�@ DÀ D��HD�  D�>�DĀ D��HD�HD�>�Dŀ D�� D���D�>�Dƀ D��HD�HD�>�D�~�DǽqD���D�AHDȂ�D��HD���D�>�Dɀ DɽqD��qD�@ Dʀ D�� D�HD�AHDˁHD��HD��D�AHD�~�D̽qD�  D�B�D͂�D�� D���D�>�D�~�Dξ�D���D�@ DρHD�� D���D�AHDЁHD��HD��D�AHDр DѾ�D�  D�>�DҀ D��HD���D�>�DӀ D��HD�HD�B�DԀ DԾ�D�  D�@ DՀ D�� D�HD�AHD�}qDֽqD��qD�>�D�~�D�� D�HD�@ D�~�DؽqD��qD�>�Dـ D�� D�  D�@ Dڀ D�� D�  D�AHDۀ D۾�D���D�B�D܀ Dܾ�D���D�>�D݁HD�� D��qD�>�D�~�D޾�D���D�>�D�~�D�� D��D�@ D�� DྸD���D�@ D�HD�D�HD�>�D�~�D⾸D�HD�AHD�HD�D�  D�>�D�HD��HD��qD�=qD�~�D�� D�HD�@ D� D��HD�  D�@ D�~�D羸D���D�>�D�~�D�� D�HD�AHD邏D�D�  D�AHDꂏD��HD��D�C�D낏D�� D�  D�AHD��D�D���D�>�D�~�D��qD��)D�>�D� DD��D�C�D�HD�� D�  D�=qD��HD�� D���D�@ D�HD��HD�HD�@ D�~�G�O�?�?#�
?u?�{?���?��@\)@!G�@.{@J=q@Y��@k�@��\@�{@�@��R@���@�
=@��R@���@�
=@޸R@�=q@�z�@�(�Az�AQ�A��A33AA(�A ��A$z�A+�A/\)A333A8Q�A>�RAB�\AH��AN{AQG�AXQ�A^{Ab�\Ah��Amp�Ar�\Ax��A~{A�G�A�(�A�
=A���A�(�A�ffA���A�(�A�ffA���A��
A�ffA���A��
A��RA���A��HA�ffA�G�A�33A�ffA�G�A�33A�ffA�G�AÅA�{A�G�A˅A�{Aљ�A�(�A�ffA�G�A���A޸RAᙚA���A�\)A陚A��A�  A��A���A�Q�A�=qA���B (�Bp�B�RBQ�BB�RB(�B	�B
�HBQ�B�B
=B(�B�B33BQ�B�B\)BQ�BB
=B(�B�B
=B (�B!�B#
=B$(�B%B'\)B(Q�B)��B+33B,z�B-��B/\)B0z�B1��B333B4��B5�B733B8��B:=qB;\)B<��B>=qB?\)B@z�BB{BC�BDz�BEBG\)BHz�BI��BK33BL��BM��BO
=BP��BQ��BR�HBTz�BU�BW
=BX(�BYB[
=B\  B]B_
=B`(�Bap�Bc
=Bd(�BeG�Bf�RBhQ�BiG�Bj�\Bl(�BmG�Bn=qBp  BqG�BrffBs�
BuG�Bv=qBw�
ByG�Bz=qB{�B}G�B~=qB�B���B��B�B��\B��B���B�Q�B�
=B���B�=qB�
=B��B�(�B���B�p�B�  B��RB��B�  B���B�p�B�{B��\B�G�B�  B��\B�
=B��
B��\B�
=B��B��\B���B���B�z�B���B�p�B�{B��HB��B�  B���B�p�B�  B�z�B�33B��B�Q�B��HB��B�Q�B���B�\)B�{B���B��B��
B��\B�
=B���B�ffB�
=B�p�B�{B��HB�p�B��B��\B�\)B��B�ffB���B�B�z�B���B��B�=qB��HB�G�B�(�B��RB�33B��B��RB�33B�B�z�B�33B�B�=qB�
=B���B�{B��HB���B�{B���B�\)B�{B��\B�33B�  B��\B�
=B���B�z�B�
=BÅB�=qB���B�p�B�  BƸRB�p�B�{Bȏ\B��B��B�z�B�
=B˙�B�Q�B�
=B͙�B�(�B��HBϙ�B�(�BУ�B�G�B�  Bң�B��B��
Bԏ\B���Bՙ�B�Q�B���BׅB�  B���B�p�B��B�z�B�
=B��
B�ffB���B�p�B�(�B���B�33B��
B��\B�G�B�B�=qB�
=B�B�(�B��B�G�B��B�ffB��HB�B�=qB�\B�
=B�B�ffB���B�\)B�{B���B�G�B��B�Q�B�
=B�B�(�B��B�\)B�{B��B�
=B�B�ffB�
=B��B��B�ffB�
=B��B�(�B��\B�
=B��B�(�B��\B��HB�G�B�B�=qB�z�B��RB�33B���B��
B�  B�Q�B���B�
=B�33B�p�B��C {C (�C Q�C �C �C C �HC
=C=qCQ�Cp�C�C�
C�HC
=C=qCp�Cz�C��C�HC
=C�CG�Cz�C��C�RC�HC�C33CG�Cz�C�C�
C�C�CffC�C��C�HC{C33CQ�C�\C��C�HC�C\)Cz�C��C��C  C=qC\)Cz�CC��C	�C	G�C	p�C	�RC	�C
  C
33C
p�C
��C
�RC
�C(�C\)Cz�C��C�C{C33CffC��C�HC
=C(�C\)C�\C��C
=C(�CG�C��CC�HC{CQ�C�\C�C�
C{CQ�Cz�C��C�HC{C33Cp�C�RC�HC
=C=qC�\C�C�HC�CffC��C�RC��C33Cp�C��CC
=CQ�Cz�C��C�HC(�C\)C�C�RC  CG�Cz�C��C�HC(�CQ�C�C�
C  C33Cz�C�C�
C
=CQ�C�C�C�
C�C\)C�C��C�C33CffC�C�RC
=C33C\)C��C�HC
=C33Cp�C�C�HC  CG�C�C��C�HC �C G�C p�C �RC �C!
=C!G�C!�C!�RC!�
C"
=C"Q�C"�C"��C"�
C#�C#=qC#ffC#��C#�HC${C$33C$\)C$�\C$�
C$��C%{C%Q�C%�\C%C%�HC&
=C&Q�C&p�C&��C&�
C'{C'=qC'\)C'�\C'��C'��C({C(Q�C(�\C(�RC(�HC){C)G�C)ffC)�\C)C*  C*=qC*\)C*z�C*�RC*��C+{C+=qC+z�C+�C+�
C,  C,(�C,ffC,��C,�RC,�HC-{C-\)C-�\C-��C-�
C.{C.Q�C.�C.��C.��C/
=C/G�C/�C/�C/�
C0
=C0G�C0z�C0�C0�
C1{C1\)C1��C1�RC1�HC2�C2\)C2��C2C2�C3�C3ffC3��C3C3�C433C4z�C4��C4��C4��C5=qC5z�C5�C5�
C6
=C6\)C6�\C6C6�C733C7p�C7�C7��C8  C8G�C8�C8�RC8�HC9{C9Q�C9��C9��C9��C:�C:p�C:�C:��C;
=C;Q�C;�\C;C;�C<(�C<p�C<�C<�
C=  C==qC=z�C=C=��C>�C>\)C>��C>�
C?  C?(�C?ffC?�C?�C@{C@G�C@z�C@�RCA  CA33CAffCA�\CA�HCB�CBG�CBp�CB��CB�CC(�CCffCC��CCCD  CDQ�CD�\CD�RCD�CE�CEffCE��CE�HCF{CFG�CFp�CF�CF��CG33CGffCG�CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�JA�JA�JA�JA���A�A��A��TA՟�A�v�A�v�A�ZAԾwAԩ�Aԟ�Aԗ�AԋDAԅAԁA�~�A�z�A�v�A�n�A�jA�hsA�ffA�bNA�\)A�K�A�E�A�A�A�1'A�-A�+A�(�A�+A�+A�(�A�&�A�$�A�"�A� �A� �A� �A� �A� �A� �A��A��A�{A�VA�A���A���A���A��A�ƨA�^5A���A�oA���A���A�1A���Aɏ\A�ĜA�S�A���A�l�A��/A�XA���A��`A��7A��A�r�A��^A�$�A��A�K�A�/A�dZA�A�"�A���A��A�dZA��TA�dZA�  A�  A�33A���A� �A���A�A�A�VA�  A��A�x�A��jA��A�^5A�ZA�5?A��jA�VA��9A�VA�7LA���A�/A�p�A�A�A{�hAyt�AtjAp^5Al�`Ai�AhVAg�Ag�FAfQ�Adz�Aa��A^=qA]/A[33AW\)AT�9ASC�AR$�AP  AO�AN�AM|�AJ��AH��AF�`AD^5AAVA=S�A8Q�A4^5A3��A3\)A1��A/�TA.-A.�A.1A-��A-G�A,~�A+�A*�`A)�PA(��A'��A&�HA%`BA$�\A$VA$5?A$$�A#�A"��A!oA�AAA&�A��A�A�PAoA�A��AĜA��A��A�hAVAx�A�\A��A��A\)A
=A�DA\)A(�A��AG�A
��A	�PA	?}A	�A��AVA-AO�A�A\)A�jA�RA&�A��A{A�wAhsAAA�A�#A��A&�A A�A ffA ^5@��
@���A ��A �@���@�S�@���@���@�{@��^@���@� �@��;@�v�@�@���@�|�@�E�@�hs@�n�@�7L@�j@��@�7@�hs@�5?@�V@�j@۾w@���@�z�@���@�V@�Ĝ@�A�@�1@�l�@��y@�M�@�?}@ԓu@��m@�dZ@��H@҇+@�^5@��@��`@ЋD@�9X@��@�b@�1@ϕ�@��@��@ΰ!@�~�@�^5@�$�@��@�p�@�V@���@�z�@�(�@�33@ʸR@�V@��#@ə�@�?}@��/@���@Ȭ@� �@�E�@Ų-@��@��T@��/@���@�t�@�S�@�@��H@�V@��@��T@��-@�?}@��@�(�@���@�;d@�ff@���@���@�p�@�O�@��@��j@��@�A�@��@�  @��@��
@�l�@�o@�ȴ@�n�@��T@���@��`@��/@��j@�1'@��;@���@��F@�|�@�+@���@�~�@��@��^@�hs@�V@���@��@�ƨ@��w@���@�C�@��@�o@�@��!@�V@���@��^@���@�/@��@��@�%@��/@�I�@�1'@�1@��;@���@�;d@�V@��T@��-@���@��h@��@�p�@�`B@�?}@��@��j@��@��w@���@�\)@���@�~�@�J@���@���@�hs@�%@���@��j@�z�@�Q�@�1@���@�ff@�$�@��@���@���@�`B@���@���@��@�I�@���@��@�S�@�"�@��@��!@�M�@�J@���@�X@��`@��9@���@��@�9X@���@�\)@�C�@��@���@���@�v�@�ff@�V@�=q@�J@��@���@�`B@�`B@�?}@��/@�A�@�  @��
@��F@���@�o@�E�@��7@�Ĝ@�j@��@���@��@�l�@�33@��@��y@���@���@��@�@��h@�O�@���@���@� �@��m@��
@��w@��F@��F@��P@�C�@��!@��+@�V@�=q@��@�@��T@��^@��-@�X@��@��@���@���@��@�z�@�Q�@� �@��P@�K�@�+@�
=@���@���@�^5@�{@���@���@��^@��h@�p�@�/@��`@���@�Q�@�  @�|�@�C�@�+@�"�@��H@��!@�M�@�=q@�5?@��@��@���@�G�@�/@���@�  @���@�|�@�ȴ@�^5@�5?@�J@��T@��^@��-@���@���@���@��7@�O�@�V@���@��@�r�@� �@�  @��@��P@�l�@�S�@�
=@��R@���@���@��@�{@���@��T@���@�@���@�/@��@��@��`@���@��9@��@��@��@��@�r�@�A�@�P@;d@~��@~E�@~5?@~5?@~{@}�T@|�j@|9X@|(�@|�@|�@{�
@{33@z�\@z�\@zn�@y��@yG�@x�u@w�@w|�@w�@u`B@t��@uO�@u�@u�h@u/@t��@t�@s�F@st�@sdZ@sC�@so@r�!@r�@q��@qG�@o+@n�+@nV@m@l��@l��@lZ@k��@kt�@j^5@jJ@i�#@i�^@iX@h��@hĜ@h��@hbN@g�w@f��@e��@d��@d�/@d�D@dI�@c��@c33@a��@a�@a%@`��@`A�@` �@_�@_��@_K�@_+@_
=@^�+@]��@]/@\�@\I�@[��@[ƨ@[��@["�@Zn�@Y��@X�`@X��@XbN@X  @W�@Wl�@W�@V�R@V�+@V5?@U�@U��@U/@UV@T��@Tz�@S�
@S�@St�@So@R^5@R-@Q�#@Q�7@P�u@O�@O�;@O�P@N��@NV@N@M@M�@L�j@L9X@K�m@K�@KC�@J�@J~�@I��@I�^@I��@Ix�@IG�@HbN@Hb@G�@G�@G�P@G|�@Gl�@F��@F��@Fff@FV@F{@E�@EO�@EO�@EV@D�@D9X@C�m@C��@C33@B�!@B-@A�#@AG�@@��@@��@@�@@bN@@A�@@ �@@  @?�@?|�@?l�@?;d@?
=@>�y@>ff@>$�@>@=��@=�h@=`B@=V@<�j@<I�@<1@<1@;��@;"�@:�H@:��@:�!@:^5@:�@9�^@9X@8��@8�u@8 �@7|�@7�@7
=@6�R@6@5��@5�@5`B@5/@5�@4�@4��@4�@4z�@4j@4Z@49X@3��@3��@3�F@3t�@3C�@3"�@3o@2��@2n�@2�@1��@1X@0�`@0�9@0r�@0 �@0  @/�w@/��@/|�@/;d@/
=@.�+@.$�@-?}@,�j@,�j@,�@,Z@+�F@+33@*�H@*n�@*M�@)�#@)�7@)X@(��@(��@(�u@(Q�@'�@'�P@'l�@';d@&�y@&ȴ@&��@&v�@&5?@&@%�T@%p�@%O�@%V@$��@$�@$z�@$1@#�
@#��@#o@#o@"��@"�\@"=q@!x�@!G�@ �9@ Q�@  �@�;@�w@�P@��@��@$�@@��@�h@p�@O�@?}@/@V@�/@Z@�@��@��@t�@S�@33@@�@��@�!@~�@^5@M�@=q@-@�@�@�#@�7@��@��@��@��@�u@b@��@�w@�w@�@+@�y@�R@E�@$�@$�@@�T@��@�h@�h@O�@�@�@��@�/@�j@z�@Z@(�@�
@�F@33@�@��@M�@M�@=q@=q@=q@-@J@��@�7@x�@X@&�@��@�@bN@1'@�@�@�;@�;@�@��@�P@�P@�P@\)@�@�@�R@�R@�R@�R@ȴ@�R@�+@V@E�@E�@{@@�@�T@��@p�@`B@O�@O�A�
=A�1A�
=A�{A�VA�JA�VA�VA�1A�bA�
=A�
=A�VA�1A�
=A���A��A���A�A�JA���A��`A��mA��TA��HA��TA���A�AՍPA�^5A�ffAՅAՁA�z�A�n�A�n�A�p�A�p�A�jA�S�A�"�A��A���A���AԴ9AԲ-AԮAԬA԰!Aԩ�Aԥ�Aԩ�Aԧ�Aԡ�Aԧ�Aԟ�Aԛ�Aԟ�Aԙ�Aԕ�Aԙ�Aԙ�AԓuAԗ�Aԗ�AԓuAԏ\Aԏ\AԋDAԅAԉ7Aԉ7AԅAԇ+Aԉ7AԃAԃAԅA�~�A�~�AԃAԁA�|�A�~�AԃA�|�A�|�A�~�A�|�A�z�A�|�A�~�A�x�A�v�A�z�A�v�A�t�A�x�A�v�A�r�A�r�A�r�A�n�A�l�A�p�A�l�A�jA�l�A�hsA�ffA�l�A�hsA�ffA�jA�jA�ffA�jA�hsA�dZA�jA�hsA�dZA�hsA�ffA�`BA�bNA�dZA�`BA�^5A�dZA�`BA�\)A�bNA�^5A�XA�S�A�VA�O�A�I�A�K�A�K�A�E�A�E�A�I�A�C�A�A�A�E�A�E�A�A�A�A�A�E�A�A�A�=qA�;dA�9XA�1'A�1'A�5?A�/A�-A�/A�/A�-A�+A�/A�+A�&�A�+A�-A�&�A�(�A�-A�+A�&�A�(�A�-A�&�A�&�A�-A�(�A�&�A�-A�+A�(�A�+A�/A�(�A�-A�-A�(�A�+A�/A�(�A�(�A�+A�&�A�&�A�(�A�(�A�$�A�&�A�(�A�$�A�$�A�(�A�$�A�$�A�&�A�"�A� �A�$�A�&�A� �A� �A�$�A�"�A��A� �A�$�A� �A��A�"�A�"�A��A� �A�$�A� �A� �A�$�A� �A��A��A�"�A�"�A��A��A�"�A�"�A��A� �A�"�A��A��A�"�A�"�A� �A��A�$�A�"�A��A� �A�"�A��A��A� �A�"�A��A��A�"�A��A��A��A� �A��A��A��A��A��A��A�oA�{A�{A�bA�oA�bA�JA�VA�bA�VA�
=A�
=A�JA�A�  A�A�  A���A���A�  A�  A���A�  A�  A���A���A�A�  A���A���A�A�  A���A���A�  A���A���A���A���A���A��A��TA��TA��;A��#A�ȴAӾwAӸRAӬAӟ�Aӕ�AӃA�ffA�?}A�&�A�1A���A��HA��
A�ƨAҥ�A�v�A�\)A�M�A�7LA�(�A��A�ȴAч+A�bNA�S�A�I�A�1'Aд9A�$�A��AϺ^A�$�A��HAΣ�A�x�A�S�A�1'A��A�VA���A���A���A���A���A�  A��A�%A���A͇+A��A�p�Aˉ7A���A�7LA�"�A�A�A�VA��A�  A��AƼjAƍPA�XA��A�n�A� �A�1A�A��;A���AÑhA���A�jA�/A���A�E�A� �A�VA��`A���A���A��RA��+A�|�A�r�A�XA�G�A�{A��A���A�t�A�;dA�1'A�-A�-A�/A��A���A���A��A�x�A�VA��A�VA��/A��^A��DA��A��A�G�A�5?A��A�VA�A���A��mA��RA��7A�dZA�O�A�;dA�(�A���A���A���A�ĜA��jA���A�n�A�G�A�VA�M�A�ȴA�A�A��TA���A�A�A���A��A�ȴA��^A��FA��9A���A���A��uA��7A��A�z�A�l�A�^5A�K�A�/A�{A���A��`A��/A��
A��
A��#A��A���A���A��
A��
A���A���A�ȴA��RA�ZA�"�A��TA�n�A�1'A��TA���A���A���A��wA���A���A���A��DA��A�x�A�t�A�l�A�\)A�C�A��A�A��HA�ĜA��9A��hA�^5A��A���A��7A�K�A�9XA�JA��^A�~�A��A�;dA�ƨA�XA���A���A��A���A��A��A�t�A�bNA�Q�A�7LA��A�VA��A��;A���A��RA��A��PA�p�A�A�A�&�A��A�
=A�%A�%A�
=A�1A�A�A�A�A���A���A��`A���A���A�A��jA��\A�
=A�{A�/A�A���A��A��`A��
A���A��wA��jA��RA��9A��A��PA�bNA�M�A�7LA� �A���A��HA���A��wA���A���A��+A�x�A�dZA�Q�A�A�A�A�A�;dA�5?A� �A���A��`A��TA��`A��TA��#A���A���A���A��jA��-A���A���A��+A�v�A�^5A�K�A�7LA��A�  A��;A���A�v�A�`BA�A�A�-A�JA��A���A�=qA��yA��jA��\A�|�A�bNA�&�A��A��mA��
A��A�E�A���A��FA��A�`BA�I�A�-A�&�A��A�A���A��A��#A��wA��A�;dA���A�9XA�K�A��A�  A��A���A���A�x�A�bNA�A�A��A���A���A��A��`A��A���A�ĜA��9A���A���A��\A��7A��A�v�A�t�A�r�A�l�A�bNA�VA�M�A�E�A�9XA��A�  A��A��`A��;A���A��9A�I�A���A��
A��-A��7A�ZA�-A�
=A��A��A��mA��HA��#A���A���A���A�ƨA��jA��!A���A�~�A�+A�A��hA�dZA� �A��;A��A�33A��A�ƨA��A��A�r�A�oA���A���A�t�A�jA�^5A�O�A�$�A�A���A��#A���A�ĜA���A�`BA�C�A� �A���A��jA��uA�n�A�I�A� �A�A���A��#A��!A���A��A�dZA�S�A�I�A�7LA� �A��A�1A���A���A�v�A�bNA�E�A��DA�C�A�7LA�{A��A���A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            A�JA�JA�JA�JA���A�A��A��TA՟�A�v�A�v�A�ZAԾwAԩ�Aԟ�Aԗ�AԋDAԅAԁA�~�A�z�A�v�A�n�A�jA�hsA�ffA�bNA�\)A�K�A�E�A�A�A�1'A�-A�+A�(�A�+A�+A�(�A�&�A�$�A�"�A� �A� �A� �A� �A� �A� �A��A��A�{A�VA�A���A���A���A��A�ƨA�^5A���A�oA���A���A�1A���Aɏ\A�ĜA�S�A���A�l�A��/A�XA���A��`A��7A��A�r�A��^A�$�A��A�K�A�/A�dZA�A�"�A���A��A�dZA��TA�dZA�  A�  A�33A���A� �A���A�A�A�VA�  A��A�x�A��jA��A�^5A�ZA�5?A��jA�VA��9A�VA�7LA���A�/A�p�A�A�A{�hAyt�AtjAp^5Al�`Ai�AhVAg�Ag�FAfQ�Adz�Aa��A^=qA]/A[33AW\)AT�9ASC�AR$�AP  AO�AN�AM|�AJ��AH��AF�`AD^5AAVA=S�A8Q�A4^5A3��A3\)A1��A/�TA.-A.�A.1A-��A-G�A,~�A+�A*�`A)�PA(��A'��A&�HA%`BA$�\A$VA$5?A$$�A#�A"��A!oA�AAA&�A��A�A�PAoA�A��AĜA��A��A�hAVAx�A�\A��A��A\)A
=A�DA\)A(�A��AG�A
��A	�PA	?}A	�A��AVA-AO�A�A\)A�jA�RA&�A��A{A�wAhsAAA�A�#A��A&�A A�A ffA ^5@��
@���A ��A �@���@�S�@���@���@�{@��^@���@� �@��;@�v�@�@���@�|�@�E�@�hs@�n�@�7L@�j@��@�7@�hs@�5?@�V@�j@۾w@���@�z�@���@�V@�Ĝ@�A�@�1@�l�@��y@�M�@�?}@ԓu@��m@�dZ@��H@҇+@�^5@��@��`@ЋD@�9X@��@�b@�1@ϕ�@��@��@ΰ!@�~�@�^5@�$�@��@�p�@�V@���@�z�@�(�@�33@ʸR@�V@��#@ə�@�?}@��/@���@Ȭ@� �@�E�@Ų-@��@��T@��/@���@�t�@�S�@�@��H@�V@��@��T@��-@�?}@��@�(�@���@�;d@�ff@���@���@�p�@�O�@��@��j@��@�A�@��@�  @��@��
@�l�@�o@�ȴ@�n�@��T@���@��`@��/@��j@�1'@��;@���@��F@�|�@�+@���@�~�@��@��^@�hs@�V@���@��@�ƨ@��w@���@�C�@��@�o@�@��!@�V@���@��^@���@�/@��@��@�%@��/@�I�@�1'@�1@��;@���@�;d@�V@��T@��-@���@��h@��@�p�@�`B@�?}@��@��j@��@��w@���@�\)@���@�~�@�J@���@���@�hs@�%@���@��j@�z�@�Q�@�1@���@�ff@�$�@��@���@���@�`B@���@���@��@�I�@���@��@�S�@�"�@��@��!@�M�@�J@���@�X@��`@��9@���@��@�9X@���@�\)@�C�@��@���@���@�v�@�ff@�V@�=q@�J@��@���@�`B@�`B@�?}@��/@�A�@�  @��
@��F@���@�o@�E�@��7@�Ĝ@�j@��@���@��@�l�@�33@��@��y@���@���@��@�@��h@�O�@���@���@� �@��m@��
@��w@��F@��F@��P@�C�@��!@��+@�V@�=q@��@�@��T@��^@��-@�X@��@��@���@���@��@�z�@�Q�@� �@��P@�K�@�+@�
=@���@���@�^5@�{@���@���@��^@��h@�p�@�/@��`@���@�Q�@�  @�|�@�C�@�+@�"�@��H@��!@�M�@�=q@�5?@��@��@���@�G�@�/@���@�  @���@�|�@�ȴ@�^5@�5?@�J@��T@��^@��-@���@���@���@��7@�O�@�V@���@��@�r�@� �@�  @��@��P@�l�@�S�@�
=@��R@���@���@��@�{@���@��T@���@�@���@�/@��@��@��`@���@��9@��@��@��@��@�r�@�A�@�P@;d@~��@~E�@~5?@~5?@~{@}�T@|�j@|9X@|(�@|�@|�@{�
@{33@z�\@z�\@zn�@y��@yG�@x�u@w�@w|�@w�@u`B@t��@uO�@u�@u�h@u/@t��@t�@s�F@st�@sdZ@sC�@so@r�!@r�@q��@qG�@o+@n�+@nV@m@l��@l��@lZ@k��@kt�@j^5@jJ@i�#@i�^@iX@h��@hĜ@h��@hbN@g�w@f��@e��@d��@d�/@d�D@dI�@c��@c33@a��@a�@a%@`��@`A�@` �@_�@_��@_K�@_+@_
=@^�+@]��@]/@\�@\I�@[��@[ƨ@[��@["�@Zn�@Y��@X�`@X��@XbN@X  @W�@Wl�@W�@V�R@V�+@V5?@U�@U��@U/@UV@T��@Tz�@S�
@S�@St�@So@R^5@R-@Q�#@Q�7@P�u@O�@O�;@O�P@N��@NV@N@M@M�@L�j@L9X@K�m@K�@KC�@J�@J~�@I��@I�^@I��@Ix�@IG�@HbN@Hb@G�@G�@G�P@G|�@Gl�@F��@F��@Fff@FV@F{@E�@EO�@EO�@EV@D�@D9X@C�m@C��@C33@B�!@B-@A�#@AG�@@��@@��@@�@@bN@@A�@@ �@@  @?�@?|�@?l�@?;d@?
=@>�y@>ff@>$�@>@=��@=�h@=`B@=V@<�j@<I�@<1@<1@;��@;"�@:�H@:��@:�!@:^5@:�@9�^@9X@8��@8�u@8 �@7|�@7�@7
=@6�R@6@5��@5�@5`B@5/@5�@4�@4��@4�@4z�@4j@4Z@49X@3��@3��@3�F@3t�@3C�@3"�@3o@2��@2n�@2�@1��@1X@0�`@0�9@0r�@0 �@0  @/�w@/��@/|�@/;d@/
=@.�+@.$�@-?}@,�j@,�j@,�@,Z@+�F@+33@*�H@*n�@*M�@)�#@)�7@)X@(��@(��@(�u@(Q�@'�@'�P@'l�@';d@&�y@&ȴ@&��@&v�@&5?@&@%�T@%p�@%O�@%V@$��@$�@$z�@$1@#�
@#��@#o@#o@"��@"�\@"=q@!x�@!G�@ �9@ Q�@  �@�;@�w@�P@��@��@$�@@��@�h@p�@O�@?}@/@V@�/@Z@�@��@��@t�@S�@33@@�@��@�!@~�@^5@M�@=q@-@�@�@�#@�7@��@��@��@��@�u@b@��@�w@�w@�@+@�y@�R@E�@$�@$�@@�T@��@�h@�h@O�@�@�@��@�/@�j@z�@Z@(�@�
@�F@33@�@��@M�@M�@=q@=q@=q@-@J@��@�7@x�@X@&�@��@�@bN@1'@�@�@�;@�;@�@��@�P@�P@�P@\)@�@�@�R@�R@�R@�R@ȴ@�R@�+@V@E�@E�@{@@�@�T@��@p�@`B@O�G�O�A�
=A�1A�
=A�{A�VA�JA�VA�VA�1A�bA�
=A�
=A�VA�1A�
=A���A��A���A�A�JA���A��`A��mA��TA��HA��TA���A�AՍPA�^5A�ffAՅAՁA�z�A�n�A�n�A�p�A�p�A�jA�S�A�"�A��A���A���AԴ9AԲ-AԮAԬA԰!Aԩ�Aԥ�Aԩ�Aԧ�Aԡ�Aԧ�Aԟ�Aԛ�Aԟ�Aԙ�Aԕ�Aԙ�Aԙ�AԓuAԗ�Aԗ�AԓuAԏ\Aԏ\AԋDAԅAԉ7Aԉ7AԅAԇ+Aԉ7AԃAԃAԅA�~�A�~�AԃAԁA�|�A�~�AԃA�|�A�|�A�~�A�|�A�z�A�|�A�~�A�x�A�v�A�z�A�v�A�t�A�x�A�v�A�r�A�r�A�r�A�n�A�l�A�p�A�l�A�jA�l�A�hsA�ffA�l�A�hsA�ffA�jA�jA�ffA�jA�hsA�dZA�jA�hsA�dZA�hsA�ffA�`BA�bNA�dZA�`BA�^5A�dZA�`BA�\)A�bNA�^5A�XA�S�A�VA�O�A�I�A�K�A�K�A�E�A�E�A�I�A�C�A�A�A�E�A�E�A�A�A�A�A�E�A�A�A�=qA�;dA�9XA�1'A�1'A�5?A�/A�-A�/A�/A�-A�+A�/A�+A�&�A�+A�-A�&�A�(�A�-A�+A�&�A�(�A�-A�&�A�&�A�-A�(�A�&�A�-A�+A�(�A�+A�/A�(�A�-A�-A�(�A�+A�/A�(�A�(�A�+A�&�A�&�A�(�A�(�A�$�A�&�A�(�A�$�A�$�A�(�A�$�A�$�A�&�A�"�A� �A�$�A�&�A� �A� �A�$�A�"�A��A� �A�$�A� �A��A�"�A�"�A��A� �A�$�A� �A� �A�$�A� �A��A��A�"�A�"�A��A��A�"�A�"�A��A� �A�"�A��A��A�"�A�"�A� �A��A�$�A�"�A��A� �A�"�A��A��A� �A�"�A��A��A�"�A��A��A��A� �A��A��A��A��A��A��A�oA�{A�{A�bA�oA�bA�JA�VA�bA�VA�
=A�
=A�JA�A�  A�A�  A���A���A�  A�  A���A�  A�  A���A���A�A�  A���A���A�A�  A���A���A�  A���A���A���A���A���A��A��TA��TA��;A��#A�ȴAӾwAӸRAӬAӟ�Aӕ�AӃA�ffA�?}A�&�A�1A���A��HA��
A�ƨAҥ�A�v�A�\)A�M�A�7LA�(�A��A�ȴAч+A�bNA�S�A�I�A�1'Aд9A�$�A��AϺ^A�$�A��HAΣ�A�x�A�S�A�1'A��A�VA���A���A���A���A���A�  A��A�%A���A͇+A��A�p�Aˉ7A���A�7LA�"�A�A�A�VA��A�  A��AƼjAƍPA�XA��A�n�A� �A�1A�A��;A���AÑhA���A�jA�/A���A�E�A� �A�VA��`A���A���A��RA��+A�|�A�r�A�XA�G�A�{A��A���A�t�A�;dA�1'A�-A�-A�/A��A���A���A��A�x�A�VA��A�VA��/A��^A��DA��A��A�G�A�5?A��A�VA�A���A��mA��RA��7A�dZA�O�A�;dA�(�A���A���A���A�ĜA��jA���A�n�A�G�A�VA�M�A�ȴA�A�A��TA���A�A�A���A��A�ȴA��^A��FA��9A���A���A��uA��7A��A�z�A�l�A�^5A�K�A�/A�{A���A��`A��/A��
A��
A��#A��A���A���A��
A��
A���A���A�ȴA��RA�ZA�"�A��TA�n�A�1'A��TA���A���A���A��wA���A���A���A��DA��A�x�A�t�A�l�A�\)A�C�A��A�A��HA�ĜA��9A��hA�^5A��A���A��7A�K�A�9XA�JA��^A�~�A��A�;dA�ƨA�XA���A���A��A���A��A��A�t�A�bNA�Q�A�7LA��A�VA��A��;A���A��RA��A��PA�p�A�A�A�&�A��A�
=A�%A�%A�
=A�1A�A�A�A�A���A���A��`A���A���A�A��jA��\A�
=A�{A�/A�A���A��A��`A��
A���A��wA��jA��RA��9A��A��PA�bNA�M�A�7LA� �A���A��HA���A��wA���A���A��+A�x�A�dZA�Q�A�A�A�A�A�;dA�5?A� �A���A��`A��TA��`A��TA��#A���A���A���A��jA��-A���A���A��+A�v�A�^5A�K�A�7LA��A�  A��;A���A�v�A�`BA�A�A�-A�JA��A���A�=qA��yA��jA��\A�|�A�bNA�&�A��A��mA��
A��A�E�A���A��FA��A�`BA�I�A�-A�&�A��A�A���A��A��#A��wA��A�;dA���A�9XA�K�A��A�  A��A���A���A�x�A�bNA�A�A��A���A���A��A��`A��A���A�ĜA��9A���A���A��\A��7A��A�v�A�t�A�r�A�l�A�bNA�VA�M�A�E�A�9XA��A�  A��A��`A��;A���A��9A�I�A���A��
A��-A��7A�ZA�-A�
=A��A��A��mA��HA��#A���A���A���A�ƨA��jA��!A���A�~�A�+A�A��hA�dZA� �A��;A��A�33A��A�ƨA��A��A�r�A�oA���A���A�t�A�jA�^5A�O�A�$�A�A���A��#A���A�ĜA���A�`BA�C�A� �A���A��jA��uA�n�A�I�A� �A�A���A��#A��!A���A��A�dZA�S�A�I�A�7LA� �A��A�1A���A���A�v�A�bNA�E�A��DA�C�A�7LA�{A��A���A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�kB�kB��B�6B��B�qB�!B�!B��B��B��B�'B��B�'B��B��B�[B�'B��B��B�'B��B��B�UB�UB�UB�UB��B��B�B�B�}B��B��B��B��B�B�B��B��B��B�B�B�B�B�IB�IB�B�B�B��B��B��B�B��B�qB��B��B�~B�oB��B|�Bt�B��B�OB[�B�B�RB�pBیB�B�JB \B�BVB 'B!B!B$�B�BB{B BCBeB�B�B@BhB.B;B�B�DB�B�vB�)B�aB�B�OB��B}VBi�B`BPHBGEB8�B0�B'�B�B
��B
ݘB
уB
�B
��B
QNB
L�B
7�B
+�B
�B
�B	��B	��B	�B	�cB	��B	�gB	�<B	�[B	�eB	��B	��B	}�B	{B	o�B	jKB	f�B	c�B	X�B	M6B	?�B	<�B	.�B	�B	IB	�B�VB��B�B��B�B��B��B��B�2B��B�PB��B�JB�"B�lB��B�(B�VB�.B��B	  B	;B	 �B�xB��B��B��B��B��B��B��B��B�lB�rB�JB�]B�(B��B�B�xB�"B��B�	B�B��B�2B��B��B	�B	�B		�B	�B	
�B	�B	.B	bB		�B	
�B	
=B	B	�B	$B	$�B	0!B	:�B	=<B	=qB	AUB	CaB	S�B	ZB	XEB	T�B	Z�B	^5B	k�B	j�B	��B	�B	�bB	�B	��B	��B	�B	�kB	��B	�FB	��B	��B	�@B	��B	��B	��B	��B	�SB	��B	�rB	�_B	t�B	s�B	w�B	gB	`�B	_;B	m�B	x8B	x�B	|�B	�GB	��B	�@B	�SB	�$B	�1B	�_B	��B	�MB	��B	��B	��B	��B	��B	��B	��B	��B	�kB	�}B	��B	��B	��B	�zB	�RB	��B	��B	�B	�BB	�aB	��B	ÖB	�B	��B	ϫB	�HB	�B	��B	ӏB	՛B	֡B	�B	�mB	�9B	�aB	ҽB	�B	�B	�B	�QB	�)B	ݘB	�vB	�B	� B	�,B	�fB	�8B	�yB	��B	�iB	�B	�AB	�B	�%B	�lB	�B
;B
�B
�B
�B
YB
�B
+B
+B
�B
	B
	�B
	lB

�B
�B
�B
�B
�B
VB
(B
�B
VB
"B
VB
VB
�B
�B
bB
�B
�B
hB
B
�B
B
MB
�B
�B
1B
�B
7B
�B
~B
B
B
~B
OB
B
B
B
!B
 \B
 �B
!�B
!�B
!�B
#�B
$�B
%B
%FB
%FB
%FB
%FB
%FB
%FB
%FB
%FB
&B
'B
&�B
&�B
&�B
(�B
(�B
*eB
*�B
+B
+6B
,=B
+�B
,�B
,�B
,qB
-�B
0�B
0!B
0�B
0�B
0�B
1'B
1�B
2-B
2aB
2�B
3�B
49B
49B
5tB
5B
5�B
6FB
6�B
7�B
8RB
8�B
:*B
9�B
9�B
9�B
:�B
;0B
:�B
:�B
;dB
;�B
<B
<B
<6B
<B
<6B
<jB
<jB
=qB
=<B
=B
=<B
=�B
?}B
?B
?HB
?B
>�B
@B
@�B
A�B
B'B
B�B
B�B
B[B
C-B
D�B
D�B
E9B
E�B
E�B
EmB
FtB
FB
F�B
F�B
G�B
HKB
IRB
IB
IB
IRB
IB
H�B
IRB
J#B
J�B
J�B
K�B
K�B
L0B
LdB
MB
MB
L�B
NpB
OvB
OvB
OvB
O�B
OvB
OBB
O�B
O�B
QNB
P�B
P�B
QB
P�B
P�B
QB
Q�B
P}B
PHB
P�B
P}B
P}B
QNB
Q�B
RTB
R�B
S�B
T�B
VB
V9B
V9B
V9B
VB
V�B
V�B
V�B
V�B
W
B
V�B
XEB
W
B
XEB
W
B
W�B
WsB
Z�B
ZB
Z�B
[�B
\)B
\]B
\�B
\�B
\�B
\�B
]�B
^5B
_pB
_;B
_�B
`vB
_�B
`B
`vB
`B
`BB
`�B
a�B
a�B
a�B
b�B
bNB
b�B
c�B
c�B
c�B
c�B
c�B
d�B
dZB
dZB
d�B
d�B
d�B
e,B
e,B
d�B
d�B
dZB
d�B
d�B
dZB
e�B
f2B
f2B
e�B
e�B
f�B
ffB
d�B
d�B
e`B
e�B
ffB
gmB
g8B
h
B
g�B
hsB
h�B
iB
h�B
h�B
iDB
h>B
g�B
hsB
jB
j�B
k�B
l"B
lWB
k�B
k�B
kB
j�B
j�B
k�B
lWB
l�B
l�B
l"B
jB
jB
kB
k�B
k�B
l"B
l�B
m]B
m�B
m�B
m�B
m�B
ncB
o�B
pB
poB
qvB
rB
rB
r|B
rB
q�B
r|B
r|B
sB
sB
s�B
t�B
t�B
u%B
t�B
uZB
u�B
u�B
u�B
u�B
u�B
v`B
xB
x8B
x�B
xlB
x�B
x�B
x�B
yrB
zB
zDB
{JB
{B
{�B
|B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~]B
~(B
~(B
~]B
~�B
~�B
~�B
~]B
�B
� B
�iB
��B
�GB
��B
��B
�oB
�;B
��B
��B
��B
�{B
��B
��B
�MB
��B
��B
��B
��B
��B
�YB
�%B
�YB
��B
��B
��B
��B
��B
�1B
�1B
�1B
��B
��B
�B
�7B
�lB
��B
��B
�B
�xB
�xB
�DB
�B
�B
��B
��B
��B
�PB
��B
��B
��B
��B
�VB
�VB
�"B
�"B
��B
��B
��B
�VB
��B
�(B
��B
��B
��B
��B
��B
��B
��B
��B
�.B
�.B
�.B
�.B
��B
��B
�\B
��B
�.B
��B
��B
��B
��B
�4B
��B
�:B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�FB
�B
��B
��B
�MB
��B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
�$B
��B
�+B
��B
��B
�eB
�eB
�eB
��B
��B
�kB
�7B
�	B
��B
��B
��B
��B
�=B
�=B
�qB
��B
�xB
��B
��B
��B
�~B
�IB
��B
��B
�B
�OB
�OB
��B
��B
��B
��B
��B
�!B
�VB
�VB
��B
��B
��B
��B
�\B
��B
��B
�-B
�bB
��B
�4B
�B
��B
��B
��B
��B
�@B
�@B
��B
�B
�B
��B
��B
��B
��B
�zB
��B
�LB
��B
��B
�B
�B
�B
��B
��B
�$B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
�eB
��B
�eB
��B
��B
��B
�B
�B
��B
��B
�B
��B
�B
�=B
�B
�B
�B
�B
�qB
�wB
�wB
��B
�B
�OB
�OB
��B
��B
��B
��B
��B
�'B
�'B
�'B
�[B
�[B
��B
��B
��B
�aB
��B
�aB
��B
�hB
��B
�9B
�B
�9B
�9B
�9B
�9B
�nB
�B
�B
��B
�B
�B
�tB
��B
��B
�B
�FB
�FB
�FB
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
��B
��B
��B
�RB
��B
��B
�RB
�RB
��B
��B
��B
��B
�$B
�XB�=B�=B�kB��B��B�kB�6B�B�wB�*B��B�=B��B��B��B�}B��B��B��B��B��B��B��B��B��B��B�9B��B�!B�B��B�=B��B��B��B��B�OB��B��B��B��B��B��B�'B�hB��B��B��B�!B�-B��B�!B��B��B�OB��B�[B��B�'B�-B��B��B��B�UB��B�'B�aB��B��B�-B��B�UB��B��B��B��B��B��B��B��B�UB��B��B��B��B�-B��B�UB��B�aB�UB��B��B�'B�OB��B��B�OB��B��B��B��B�[B��B�OB��B��B�OB��B�[B�OB��B�'B�OB�OB�'B��B��B��B��B��B��B��B�!B�[B�UB�OB��B�[B�B��B�'B�OB�!B��B��B��B��B�[B��B�}B��B��B�B��B�UB��B��B�!B�B��B��B�!B��B��B�!B��B�B�B��B�CB�wB�IB��B�B�B��B��B��B��B�IB��B�CB�B��B��B��B�}B�qB�B��B�B�wB�B�B�B�B��B�wB��B�}B�B��B�B��B��B��B�wB�CB��B��B�B�}B��B��B�IB�IB�CB�}B��B�wB�B��B��B��B�wB��B�B��B�IB�B��B�CB�B�B�B�}B�}B��B��B�B��B��B�CB��B��B�B��B��B��B�CB��B��B�wB�wB�}B�B��B��B�OB��B�CB��B�OB��B�B�B��B�CB��B�B�}B��B��B��B��B��B�qB��B�IB�B�qB�IB�=B�B�IB�B��B��B��B��B�qB�B��B��B�=B��B��B��B�qB��B�B��B��B��B��B�B��B��B�kB�qB��B�B��B�CB��B��B�kB��B��B��B��B��B�B��B��B�$B��B��B�RB�XB��B�XB�:B��B�!B�!B��B�	B�IB�B�FB�FB��B��B�B��B��B�B�B��B�B��B��B{�B�4B��Bz�B{Bv`Bv�BtBp�Bp;BqABv�By>BzDBy�BxBcB��B�1B�	B�B�UB�qB�BB��B|�Bk�B\]BkB_pB`B`BXBQ�BS�Bb�B}�Bn�B��B��B��B�-B�mB�B�wB�TBʌB�B��B�<B�tB��B�6B˒B�XB��B��B��B�B��BޞB��B��B�?B�mB�gBӏB��B��B��B�QBٴB��B�B��B�AB�B��B��B�B�;B��B�B�B�B�5B�iB�+B��B�	B�`B��B�lB��B�B�B��B��BoB  B �B
	B&�B'�B+B�B#�B)*B'�B �B �B �B�BB�B �B!�B�BB�B \B!�B�B 'B!bB$�BB�B�B�B�B~B�BBCB�BxB�BCB�B-CB!bB&B)_B#nB($B�B�BxBBB�B~BxB�B�BCBqBB \B$�B~B!�B�B!�B �B%B-CB,=B+�B$�BxB"�B%BIB#�B�B�BYB�B{B�B�B	�B�B�B	�BxB�B�B�B4BB4B�B\B$BMB�BB�B�BhB�B�B�BB�B(BbB.B�BoB�B�B.B�BhB)�BF?B33B�BB=BkBeBeB�B+B�BSB�BB7BB�B�BkB�B�B$BYB$B�B�B+B=BBoBFB@B4BIBBB.B�B�B�BhBbBhB�B�B.B4BbB@B�BhBhB�B�BuB�B.BB�B�B�B�B�B�B�B�B�B�B�B��B��B�B�VB	7B�B��B�B�B��B��B�B�B�cB��B� B��B��B�B�GB�VB�2B�B�B�?B�B��B��BҽB�BѷB� B�HB�0BΥB͟BϫB��B��B�B�}B��BϫB�BB��B�B��B��B̘B�^B�dB�^B�^B��B�6B˒B�B�[B��B��B�tBԕB� B��B��B�wB��B�^B�tB�nB�B�-B��B��B�[B��B��B�B��B��B�IB�IB��B�OB�$B�tB�FB��B�B�xB�:B��B�B�B�JB��B��B{JBy	Br�BqABp�Bq�BqABl�Bm�Bf�Bg8Bh>Bf2Be�Bc�Bc BkBb�B_�B^jB^B\�BYB\)BV9BS�BR�BP�BOvBHKBN�BNpBJ�BG�BF�BD�B?�B=qBA�B\�BJXB<B@�B@B>wB8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                            G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021111608492720211116084927IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021112603005820211126030058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021112603005820211126030058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365520220126093655IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295720220204232957IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                