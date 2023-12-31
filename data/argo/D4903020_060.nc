CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-21T15:53:15Z creation; 2021-03-26T17:01:01Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20201221155315  20210326170210  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               <   <AA  AOAO7836_008777_060                 7836_008777_060                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�P����@�P����11  @�P�-�@�P�-�@;Ӻ4C�k@;Ӻ4C�k�d���Zp��d���Zp�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@B�\@�G�@��R@�p�@޸RA ��A\)A�RA,��A@  A_\)A~�RA��A��A�  A�Q�A�  A�Q�A�  A��B�B�
B  B   B(  B0(�B8  B@  BH(�BP(�BW�
B`  Bh(�Bp  Bw�B�
B�  B�{B��B�  B�{B�  B��
B��B�{B�  B�  B�  B�  B��B��B��B��B�  B��B��B�  B�  B��B�  B�{B��B�  B�(�B�  B��B�{C 
=C
=C
=C��C  C	��C  C��C��C
=C  C
=C
=C
=C
=C��C��C!��C$  C&{C(
=C*  C,  C.
=C0
=C1�C3�C5��C7��C:  C<
=C=��C?�CA��CD  CE��CG��CJ
=CL{CN{CP
=CR
=CT  CV{CX
=CZ  C\
=C^
=C`  Cb  Cd  Cf  Ch  Ci�Ck�Cm��Co��Cr  Ct  Cv{Cx  Cy��C{�C}��C�C�C�  C���C���C���C�  C�  C�  C�C�C�  C�  C���C�  C�  C�  C���C�C�  C�  C�  C���C�  C�  C���C���C�  C�C���C�  C�C�  C���C���C�  C���C�  C�C�C�  C���C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C�C�  C���C���C���C�C���C���C�  C�  C�  C�C�  C�C�\C�  C���C���C�  C���C�  C�  C�
=C�C���C�C�  C���C�  C�C���C���C���C�C�C���C�C�  C�  C�C���C���C���C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C���C�  C�C���C�  C�  C�  C���C�  C���C���C���C�  C���C���C���C�  C���D }qD  D��D  D}qD��D� D�D�D�D��DD��D  D� D  D�D	D	� D
�D
��D�D}qD��D� DD��D  D� D�D}qD�qD� D�D��D  D}qD�D��D��D� D�D��DD��D  D� D�qD� D�D� D�qD� D  D��DD��D�D� D  D� D�qD}qD   D � D!  D!��D!�qD"}qD#  D#z�D#�RD$z�D%�D%�D&�D&� D'�D'�D(�D(}qD(��D)z�D*  D*� D*�qD+��D,�D,}qD-  D-�D.�D.� D.�qD/}qD0  D0� D0�qD1z�D1�qD2z�D2��D3}qD4  D4�D5�D5��D6  D6z�D7  D7��D7�qD8}qD9  D9��D:�D:� D:�qD;� D<  D<� D=  D=� D>�D>��D?  D?� D@  D@� DA�DA��DB  DBz�DB�qDC� DD�DD}qDD�qDE� DE�qDF}qDF��DGz�DH  DH� DI  DI��DJ  DJ� DK  DK� DL�DL�DM�DM� DN  DN��DO  DO}qDO�qDP}qDP�qDQ}qDR  DR}qDR�qDS}qDS��DT� DU�DU� DV�DV��DW�DW� DX�DX��DY  DY� DZ  DZ}qDZ�RD[z�D[��D\� D]�D]� D^  D^��D_�D_� D_�qD`}qD`�qDa}qDa�qDb� DcDc��Dd�Dd��Dd�qDe}qDfDf� Df��Dgz�Dg�qDh}qDh�qDi� Dj  Dj��Dj�qDkz�Dl  Dl��Dl�qDm� Dn  Dn� Do�Do� Do�qDpz�Dp�qDq��Dq�qDr� Dr�qDsz�Dt  Dt� Dt�qDu��DvDv�Dw  Dw}qDw��DxxRDx�qDy��DzDz��D{  D{�D|�D|��D}  D}� D~  D~� DD��D�  D�AHD��HD�� D���D�=qD�}qD���D�  D�AHD��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D�HD�AHD�� D��HD���D�>�D�~�D���D�  D�AHD�~�D��HD�HD�=qD�~�D�� D��D�B�D���D�� D��qD�>�D��HD�D�  D�@ D�� D�� D�HD�AHD��HD���D���D�AHD�� D�� D�  D�>�D�~�D���D�  D�>�D�� D��HD���D�=qD�� D�D�HD�AHD���D��HD�HD�>�D�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D���D�HD�AHD�}qD���D�  D�AHD�� D�� D���D�>�D�� D�� D���D�AHD��HD�� D�HD�AHD��HD�� D�  D�AHD���D��HD���D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D�HD�@ D�� D�D��D�AHD�� D���D�  D�B�D�� D���D�  D�AHD��HD��HD�  D�>�D��HD�� D��qD�@ D��HD���D���D�@ D�~�D���D�  D�>�D�� D���D���D�=qD�� D��HD�  D�@ D��HD��HD�HD�@ D��HD�� D��qD�>�D��HD�� D���D�=qD���D��HD��qD�=qD�~�D��HD�HD�AHD��HD���D�  D�C�D��HD�� D�  D�@ D�� D���D���D�AHD��HD��HD��D�B�D���D�� D���D�=qD�� D���D���D�>�D�~�D��HD�HD�=qD�}qD��qD��)D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D��HD�D�HD�@ D���D��HD�  D�AHD���D��HD�  D�@ D�� D��HD��D�@ D�|)D���D�  D�@ D�}qD��qD���D�@ D�~�D�� D�HD�@ D��HD�� D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�@ D�~�D��HD�  D�@ DÀ D��HD���D�=qD�}qDĽqD�  D�B�Dł�D��HD��D�@ D�~�Dƾ�D���D�@ Dǀ D�� D�  D�>�D�|)Dȼ)D��qD�>�DɁHD�� D�  D�>�Dʀ D��HD���D�@ DˁHD��HD�  D�AHD̂�D̾�D��qD�>�D̀ D;�D��qD�@ D΀ Dξ�D�  D�@ D�~�DϾ�D�  D�@ DЁHD�� D�  D�AHD�~�D�� D�HD�>�DҀ DҾ�D��qD�=qD�~�D�� D�HD�@ D�~�D�� D�  D�B�DՀ DսqD���D�>�D�~�D�� D�HD�>�D�~�D׾�D���D�>�D؀ D�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ DہHD��HD�  D�=qD܀ Dܾ�D���D�@ D݁HDݾ�D��qD�>�Dނ�D�D�  D�>�D�~�D��HD�HD�>�D�� D�� D�  D�AHD� D�� D�HD�AHD� D�� D�HD�@ D�~�D�qD��qD�>�D�~�D侸D�HD�>�D�}qD徸D��qD�=qD�~�D澸D���D�@ D� D�� D���D�>�D� D辸D�  D�@ D�~�D�� D���D�@ D�HD�� D�HD�AHD�~�D�qD���D�AHD� D�� D��qD�@ D�HD�� D��qD�>�D� D�� D�HD�B�D�HD�� D�  D�AHD�� D�� D�HD�@ D�D�D�HD�>�D�~�D��HD��qD�@ D�D��HD�HD�B�D� D���D��D�B�D�� D�� D�HD�AHD�~�D���D���D�>�D�� D���D�HD�B�D���D�D�  D�AHD��HD���D��qD�@ D�u�>�Q�?��?aG�?�=q?�Q�?�(�@�\@(�@0��@E�@Y��@k�@�  @��@���@���@��
@���@��H@�ff@��@�(�@�ff@�\)@���AG�AA(�A�AQ�A��A!�A&ffA+�A0��A7
=A<��AC33AG�AMp�AQ�AW�A\(�A`��Ae�Aj�HAqG�Aw
=A|��A���A��
A�A���A��\A��A��A��\A�p�A�Q�A��\A��A��A��HA�p�A�\)A���A�(�A��RA���A�33A�{A���A��
A��RA�G�A�(�AƸRAə�A��
A�ffA���AӅA�A�Q�A��HA�p�A��A��
A�RA陚A�(�A�\)A�A�z�A��RA���A��
A�ffB ��B=qB�BG�B�RB  B	p�B
�RB(�Bp�B�RB  B��B�\B(�BG�B�RBQ�BB33B��B=qB�B ��B"ffB#�B$��B&{B'33B(��B*=qB+�
B-�B.�\B/�
B1G�B2�\B4  B5�B6ffB7�B8��B9��B:�\B;�B<��B=p�B>ffB?�B@z�BA��BB�RBC�
BD��BF{BG
=BHQ�BIG�BJffBK�BL��BM�BN�RBO�
BP��BQBR�RBS�BT��BUp�BV�\BW�
BX��BY�B[
=B\(�B]G�B^=qB_33B`z�Ba��Bb�\Bc�Bd��BeBf�RBg�Bh��Bi��Bj�RBk�Bl��Bm�Bn�HBp  Bp��Br{Bs
=Bt(�Bt��Bv{Bv�HBx  Bx��Bz{B{
=B|(�B}G�B~ffB33B�(�B���B��B���B�(�B���B��B�B�Q�B��RB�G�B�B�Q�B���B�\)B�B�Q�B��RB�33B��B�=qB���B��B���B�(�B���B��B��B�{B��\B��B��B�  B�z�B�
=B�p�B��B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB��RB�33B��B�(�B��RB�33B��B�(�B��RB�33B��B�(�B��RB�33B��B�{B���B��B���B�{B��\B���B�p�B��B�ffB���B�\)B��
B�=qB��RB�G�B��
B�Q�B���B�33B���B�{B��\B��B��B�=qB��HB��B�  B�z�B���B�\)B��B��\B��B�B�Q�B��RB�33B��B�=qB��HB���B�  B�ffB���B���B�Q�B���B�G�B��B�(�B��HB���B�{B��\B�
=B�B�ffB�
=B��B�{B��\B�G�B�  B���B�
=B���B�(�B��HBîB�(�Bģ�B�33B��Bƣ�B�G�B�B�(�B���BɅB�(�B��HB�p�B��B�z�B���BͮB�z�B�
=BυB�  B���Bљ�B�=qBҸRB�33B��BԸRB�\)B��B�ffB�
=B��
B؏\B�G�B��
B�ffB�
=B�B܏\B�
=B݅B�Q�B�
=B��
B�Q�B���BᙚB�Q�B�
=B㙚B�{B��HB噚B�=qB�RB�\)B�(�B��HB�\)B�  B��HB�\)B��B���B�p�B��
B��B�p�B��B��\B�\)B��
B�\B�\)B�B�z�B�G�B�B�ffB�G�B�B�Q�B��B��
B�Q�B�
=B��
B�ffB��HB�B��\B�
=B���C 33C ��C �HC(�C�C�C33Cz�C�HC=qCz�C�
CG�C�C��C33C��C�
C33C��C�C(�C�\C��C33C�C��C	=qC	�C	�
C
G�C
��C
�
CG�C��C�HC=qC�C�HC=qC��C��C=qC��C  C=qC��C
=CQ�C�\C  C\)C�\C  CffC��C�C\)C�RC��CG�C�RC{C\)C��C  Cp�C�RC��CffC��C{CQ�C�C�CffC��C
=Cp�C�RC��C\)CC  CQ�CC  C\)CC��CffC�RC
=CffC�C �C Q�C �RC!
=C!\)C!�RC"  C"ffC"��C#
=C#Q�C#�C$
=C$G�C$C$��C%Q�C%�C%�C&\)C&��C&��C'\)C'��C'��C(Q�C(�C(��C)G�C)�\C*  C*33C*�\C*�C+33C+��C+�
C,G�C,z�C,�C-33C-�\C-�HC.33C.�\C.��C/G�C/p�C/�C0�C0��C0C1=qC1p�C1�HC2�C2��C2�
C333C3z�C3�
C433C4z�C4�HC533C5�C5�
C633C6�\C6�HC733C7�\C7�HC8=qC8�\C8�HC9=qC9�\C9�C:=qC:��C:�C;Q�C;�\C<  C<=qC<��C<�C=Q�C=��C=��C>Q�C>��C?  C?G�C?�RC@  C@ffC@��CA{CAQ�CACB  CBp�CB�CC(�CC\)CC��CD
=CDp�CD�RCE(�CEffCE�
CF�CF�CF��CG(�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                        ?��@�\@B�\@�G�@��R@�p�@޸RA ��A\)A�RA,��A@  A_\)A~�RA��A��A�  A�Q�A�  A�Q�A�  A��B�B�
B  B   B(  B0(�B8  B@  BH(�BP(�BW�
B`  Bh(�Bp  Bw�B�
B�  B�{B��B�  B�{B�  B��
B��B�{B�  B�  B�  B�  B��B��B��B��B�  B��B��B�  B�  B��B�  B�{B��B�  B�(�B�  B��B�{C 
=C
=C
=C��C  C	��C  C��C��C
=C  C
=C
=C
=C
=C��C��C!��C$  C&{C(
=C*  C,  C.
=C0
=C1�C3�C5��C7��C:  C<
=C=��C?�CA��CD  CE��CG��CJ
=CL{CN{CP
=CR
=CT  CV{CX
=CZ  C\
=C^
=C`  Cb  Cd  Cf  Ch  Ci�Ck�Cm��Co��Cr  Ct  Cv{Cx  Cy��C{�C}��C�C�C�  C���C���C���C�  C�  C�  C�C�C�  C�  C���C�  C�  C�  C���C�C�  C�  C�  C���C�  C�  C���C���C�  C�C���C�  C�C�  C���C���C�  C���C�  C�C�C�  C���C�  C�  C�  C�  C�C�C�  C���C�  C�  C�  C�C�  C���C���C���C�C���C���C�  C�  C�  C�C�  C�C�\C�  C���C���C�  C���C�  C�  C�
=C�C���C�C�  C���C�  C�C���C���C���C�C�C���C�C�  C�  C�C���C���C���C�  C�C�C���C�  C�  C�  C�  C���C���C���C���C�  C�  C�  C���C�  C�C���C�  C�  C�  C���C�  C���C���C���C�  C���C���C���C�  C���D }qD  D��D  D}qD��D� D�D�D�D��DD��D  D� D  D�D	D	� D
�D
��D�D}qD��D� DD��D  D� D�D}qD�qD� D�D��D  D}qD�D��D��D� D�D��DD��D  D� D�qD� D�D� D�qD� D  D��DD��D�D� D  D� D�qD}qD   D � D!  D!��D!�qD"}qD#  D#z�D#�RD$z�D%�D%�D&�D&� D'�D'�D(�D(}qD(��D)z�D*  D*� D*�qD+��D,�D,}qD-  D-�D.�D.� D.�qD/}qD0  D0� D0�qD1z�D1�qD2z�D2��D3}qD4  D4�D5�D5��D6  D6z�D7  D7��D7�qD8}qD9  D9��D:�D:� D:�qD;� D<  D<� D=  D=� D>�D>��D?  D?� D@  D@� DA�DA��DB  DBz�DB�qDC� DD�DD}qDD�qDE� DE�qDF}qDF��DGz�DH  DH� DI  DI��DJ  DJ� DK  DK� DL�DL�DM�DM� DN  DN��DO  DO}qDO�qDP}qDP�qDQ}qDR  DR}qDR�qDS}qDS��DT� DU�DU� DV�DV��DW�DW� DX�DX��DY  DY� DZ  DZ}qDZ�RD[z�D[��D\� D]�D]� D^  D^��D_�D_� D_�qD`}qD`�qDa}qDa�qDb� DcDc��Dd�Dd��Dd�qDe}qDfDf� Df��Dgz�Dg�qDh}qDh�qDi� Dj  Dj��Dj�qDkz�Dl  Dl��Dl�qDm� Dn  Dn� Do�Do� Do�qDpz�Dp�qDq��Dq�qDr� Dr�qDsz�Dt  Dt� Dt�qDu��DvDv�Dw  Dw}qDw��DxxRDx�qDy��DzDz��D{  D{�D|�D|��D}  D}� D~  D~� DD��D�  D�AHD��HD�� D���D�=qD�}qD���D�  D�AHD��HD��HD�  D�AHD��HD�� D�  D�@ D�~�D���D�HD�AHD�� D��HD���D�>�D�~�D���D�  D�AHD�~�D��HD�HD�=qD�~�D�� D��D�B�D���D�� D��qD�>�D��HD�D�  D�@ D�� D�� D�HD�AHD��HD���D���D�AHD�� D�� D�  D�>�D�~�D���D�  D�>�D�� D��HD���D�=qD�� D�D�HD�AHD���D��HD�HD�>�D�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D���D�HD�AHD�}qD���D�  D�AHD�� D�� D���D�>�D�� D�� D���D�AHD��HD�� D�HD�AHD��HD�� D�  D�AHD���D��HD���D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D��HD��HD�HD�AHD�� D�� D�HD�@ D�� D�D��D�AHD�� D���D�  D�B�D�� D���D�  D�AHD��HD��HD�  D�>�D��HD�� D��qD�@ D��HD���D���D�@ D�~�D���D�  D�>�D�� D���D���D�=qD�� D��HD�  D�@ D��HD��HD�HD�@ D��HD�� D��qD�>�D��HD�� D���D�=qD���D��HD��qD�=qD�~�D��HD�HD�AHD��HD���D�  D�C�D��HD�� D�  D�@ D�� D���D���D�AHD��HD��HD��D�B�D���D�� D���D�=qD�� D���D���D�>�D�~�D��HD�HD�=qD�}qD��qD��)D�AHD��HD��HD�  D�>�D�� D��HD�  D�@ D��HD�D�HD�@ D���D��HD�  D�AHD���D��HD�  D�@ D�� D��HD��D�@ D�|)D���D�  D�@ D�}qD��qD���D�@ D�~�D�� D�HD�@ D��HD�� D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�@ D�~�D��HD�  D�@ DÀ D��HD���D�=qD�}qDĽqD�  D�B�Dł�D��HD��D�@ D�~�Dƾ�D���D�@ Dǀ D�� D�  D�>�D�|)Dȼ)D��qD�>�DɁHD�� D�  D�>�Dʀ D��HD���D�@ DˁHD��HD�  D�AHD̂�D̾�D��qD�>�D̀ D;�D��qD�@ D΀ Dξ�D�  D�@ D�~�DϾ�D�  D�@ DЁHD�� D�  D�AHD�~�D�� D�HD�>�DҀ DҾ�D��qD�=qD�~�D�� D�HD�@ D�~�D�� D�  D�B�DՀ DսqD���D�>�D�~�D�� D�HD�>�D�~�D׾�D���D�>�D؀ D�� D���D�>�D�~�D�� D�  D�@ D�~�D�� D�  D�@ DہHD��HD�  D�=qD܀ Dܾ�D���D�@ D݁HDݾ�D��qD�>�Dނ�D�D�  D�>�D�~�D��HD�HD�>�D�� D�� D�  D�AHD� D�� D�HD�AHD� D�� D�HD�@ D�~�D�qD��qD�>�D�~�D侸D�HD�>�D�}qD徸D��qD�=qD�~�D澸D���D�@ D� D�� D���D�>�D� D辸D�  D�@ D�~�D�� D���D�@ D�HD�� D�HD�AHD�~�D�qD���D�AHD� D�� D��qD�@ D�HD�� D��qD�>�D� D�� D�HD�B�D�HD�� D�  D�AHD�� D�� D�HD�@ D�D�D�HD�>�D�~�D��HD��qD�@ D�D��HD�HD�B�D� D���D��D�B�D�� D�� D�HD�AHD�~�D���D���D�>�D�� D���D�HD�B�D���D�D�  D�AHD��HD���D��qD�@ G�O�>�Q�?��?aG�?�=q?�Q�?�(�@�\@(�@0��@E�@Y��@k�@�  @��@���@���@��
@���@��H@�ff@��@�(�@�ff@�\)@���AG�AA(�A�AQ�A��A!�A&ffA+�A0��A7
=A<��AC33AG�AMp�AQ�AW�A\(�A`��Ae�Aj�HAqG�Aw
=A|��A���A��
A�A���A��\A��A��A��\A�p�A�Q�A��\A��A��A��HA�p�A�\)A���A�(�A��RA���A�33A�{A���A��
A��RA�G�A�(�AƸRAə�A��
A�ffA���AӅA�A�Q�A��HA�p�A��A��
A�RA陚A�(�A�\)A�A�z�A��RA���A��
A�ffB ��B=qB�BG�B�RB  B	p�B
�RB(�Bp�B�RB  B��B�\B(�BG�B�RBQ�BB33B��B=qB�B ��B"ffB#�B$��B&{B'33B(��B*=qB+�
B-�B.�\B/�
B1G�B2�\B4  B5�B6ffB7�B8��B9��B:�\B;�B<��B=p�B>ffB?�B@z�BA��BB�RBC�
BD��BF{BG
=BHQ�BIG�BJffBK�BL��BM�BN�RBO�
BP��BQBR�RBS�BT��BUp�BV�\BW�
BX��BY�B[
=B\(�B]G�B^=qB_33B`z�Ba��Bb�\Bc�Bd��BeBf�RBg�Bh��Bi��Bj�RBk�Bl��Bm�Bn�HBp  Bp��Br{Bs
=Bt(�Bt��Bv{Bv�HBx  Bx��Bz{B{
=B|(�B}G�B~ffB33B�(�B���B��B���B�(�B���B��B�B�Q�B��RB�G�B�B�Q�B���B�\)B�B�Q�B��RB�33B��B�=qB���B��B���B�(�B���B��B��B�{B��\B��B��B�  B�z�B�
=B�p�B��B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB��RB�33B��B�(�B��RB�33B��B�(�B��RB�33B��B�(�B��RB�33B��B�{B���B��B���B�{B��\B���B�p�B��B�ffB���B�\)B��
B�=qB��RB�G�B��
B�Q�B���B�33B���B�{B��\B��B��B�=qB��HB��B�  B�z�B���B�\)B��B��\B��B�B�Q�B��RB�33B��B�=qB��HB���B�  B�ffB���B���B�Q�B���B�G�B��B�(�B��HB���B�{B��\B�
=B�B�ffB�
=B��B�{B��\B�G�B�  B���B�
=B���B�(�B��HBîB�(�Bģ�B�33B��Bƣ�B�G�B�B�(�B���BɅB�(�B��HB�p�B��B�z�B���BͮB�z�B�
=BυB�  B���Bљ�B�=qBҸRB�33B��BԸRB�\)B��B�ffB�
=B��
B؏\B�G�B��
B�ffB�
=B�B܏\B�
=B݅B�Q�B�
=B��
B�Q�B���BᙚB�Q�B�
=B㙚B�{B��HB噚B�=qB�RB�\)B�(�B��HB�\)B�  B��HB�\)B��B���B�p�B��
B��B�p�B��B��\B�\)B��
B�\B�\)B�B�z�B�G�B�B�ffB�G�B�B�Q�B��B��
B�Q�B�
=B��
B�ffB��HB�B��\B�
=B���C 33C ��C �HC(�C�C�C33Cz�C�HC=qCz�C�
CG�C�C��C33C��C�
C33C��C�C(�C�\C��C33C�C��C	=qC	�C	�
C
G�C
��C
�
CG�C��C�HC=qC�C�HC=qC��C��C=qC��C  C=qC��C
=CQ�C�\C  C\)C�\C  CffC��C�C\)C�RC��CG�C�RC{C\)C��C  Cp�C�RC��CffC��C{CQ�C�C�CffC��C
=Cp�C�RC��C\)CC  CQ�CC  C\)CC��CffC�RC
=CffC�C �C Q�C �RC!
=C!\)C!�RC"  C"ffC"��C#
=C#Q�C#�C$
=C$G�C$C$��C%Q�C%�C%�C&\)C&��C&��C'\)C'��C'��C(Q�C(�C(��C)G�C)�\C*  C*33C*�\C*�C+33C+��C+�
C,G�C,z�C,�C-33C-�\C-�HC.33C.�\C.��C/G�C/p�C/�C0�C0��C0C1=qC1p�C1�HC2�C2��C2�
C333C3z�C3�
C433C4z�C4�HC533C5�C5�
C633C6�\C6�HC733C7�\C7�HC8=qC8�\C8�HC9=qC9�\C9�C:=qC:��C:�C;Q�C;�\C<  C<=qC<��C<�C=Q�C=��C=��C>Q�C>��C?  C?G�C?�RC@  C@ffC@��CA{CAQ�CACB  CBp�CB�CC(�CC\)CC��CD
=CDp�CD�RCE(�CEffCE�
CF�CF�CF��CG(�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                        @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��mA��`A��yA��yA��`A��HA��A��A��A��A��A��A��yA��`A��9A��DA�M�A�33A��A�{A�A��A��^A���A�~�A�K�A�1A��A��TA���A�A���A��A���A��DA�l�A�I�A�A�A�=qA�+A��A���A� �A�=qA�-A�z�A��hA�|�A��!A���A�A��;A��A�\)A�%A�n�A��HA�ƨA���A��A���A�ffA�JA��+A���A��uA�XA�p�A�&�A�%A��
A��RA�A�A�{A��RA���A��uA�-A���A�dZA��uA��A�A�|�A��;A�1A�|�A��A���A���A��+A��yA��AC�A|1'Az��Ay�^AxI�Awp�Av�!Au�At�As�Aq`BAp1'An�HAn=qAm�wAl��AkVAj�Aj�Ai�wAiXAfA�AdA�AcG�Aa"�A^$�A\�jA\~�A[dZAZAWXAVjAV{AU|�ATbAS|�AR�yAQ�APQ�AO�7AN��ANM�AM�mAMO�AL-AJ�HAI��AH�\AG��AF�yAF�DAE�AD�jAC��AB�RAB5?AA/A@�A?��A>��A=hsA<��A<�A<r�A:ZA8��A8M�A7�
A7O�A6��A5�A4�RA3�A3�-A3�A1�mA1K�A1+A/��A/|�A/hsA/K�A//A.^5A-��A-`BA-7LA,�/A+��A+"�A*-A)hsA)�A(�A'�
A&��A%�-A%;dA$�A#p�A#G�A#"�A!�#A �`A ĜA ZA�A��AVAI�A�A��A1'A  A�PA��A�A�\AAM�A��AffA=qA�mA`BA�`A��A�A9XAhsA7LA�A
=A
�`A
ȴA
�RA
�+A
VA	�hA&�A��AE�A�AƨA?}A��AS�AjA�A�A��AVA �A ��A -@���@�-@���@�|�@�=q@��-@�1'@�5?@��/@��;@�x�@@�x�@�@�+@�^5@�u@��@�$�@�/@�E�@���@�j@�@�  @ܣ�@�v�@���@ו�@��T@�I�@�  @ӝ�@�@�?}@�1'@�S�@�~�@ͩ�@���@˶F@ʏ\@ɺ^@�bN@��@��T@�Q�@�(�@�33@�J@��-@��@���@��m@�;d@��@���@�5?@��@��`@��u@��P@�n�@���@��P@�;d@�+@��@��
@��/@�\)@��@�{@���@�hs@���@��F@�33@�@���@�=q@��h@��/@�  @�S�@��!@��@���@��w@�;d@��y@��!@�n�@�@�?}@�%@��/@��9@��@��@�33@�n�@���@��@�9X@��m@�dZ@��@�^5@���@��h@�`B@��@���@�Q�@��@�|�@�"�@��@��y@��R@�ff@��@���@�?}@�(�@��@�\)@�o@���@�^5@�{@��#@�&�@�I�@��;@�S�@��!@�^5@�E�@�$�@�@���@���@�hs@��@��@�I�@���@��F@�|�@�C�@�
=@��@��@���@�V@�-@�@��#@���@�O�@��/@�Q�@�(�@�  @��m@�ƨ@�C�@�o@�
=@�@��H@���@�~�@�M�@��@�p�@�7L@��@���@���@��@�1@��;@�ƨ@��P@��@�V@�J@��^@�hs@�&�@�%@��@���@�Ĝ@��j@��9@���@���@���@��u@� �@~��@}��@|Z@|�@{��@{�F@{S�@z�@z~�@y�@y�7@y&�@xĜ@x1'@w�@wl�@w
=@vff@vE�@v@up�@t�/@t��@tj@t1@s��@s�@s�@s�@s33@s"�@r��@r-@q�@qhs@pbN@pA�@pA�@p �@o��@o;d@n��@nV@n$�@n{@m�-@mV@l��@l�/@lZ@l�@l1@l1@k��@kƨ@kS�@ko@j�\@i��@iX@iG�@h��@h�u@hr�@hQ�@h �@h  @g��@g|�@g;d@g�@fȴ@f�+@f5?@f@e�h@e�@d�/@d��@dZ@c�F@c��@cS�@c@b�!@b��@bn�@bM�@bJ@a�@a��@a��@ahs@a&�@a%@`��@`�9@`A�@_
=@^�+@^V@^{@]�@]�h@\z�@\9X@\(�@\�@[S�@[o@Z��@Z=q@ZJ@Y��@YX@Y&�@Y�@X��@X�u@X �@W�;@W��@W|�@W|�@Wl�@W
=@V��@V{@U�@U@U?}@T�j@T�@Sƨ@S��@S�@So@R��@R��@R��@R�\@R^5@R-@R�@Q��@Q�^@QX@P�`@PQ�@O�@O�w@O�P@Ol�@OK�@N�R@NE�@Nff@NV@NV@NV@Nff@Nff@N$�@M�@MV@L��@L�/@L�@L��@L��@L�D@Lj@LZ@L9X@L(�@K�m@KC�@J~�@JJ@I��@Ihs@I%@HĜ@H�@HA�@H �@H  @G�P@F��@F�@F�R@Fv�@F$�@E�@E��@E�-@Ep�@EO�@E/@D��@D(�@C�F@C�@C"�@B�!@Bn�@BM�@A�#@A�#@A��@A�@A�@@�9@@Q�@?��@?�P@?\)@?;d@?
=@>�+@>V@>V@>V@>E�@=�@<��@<�D@<9X@;��@;�
@;ƨ@;��@;��@;�@;�@;�@;t�@;t�@;dZ@;S�@;S�@;33@;"�@:�@:��@:n�@:=q@9��@9x�@9&�@8Ĝ@8�u@81'@7�;@7�w@7��@7��@7�;@7�;@7��@7�P@7\)@65?@5�T@5�@4��@4��@4�@4��@4z�@4I�@4�@2�!@1�@0�`@0�`@0��@0��@0��@0��@0��@0Ĝ@0�9@0r�@0bN@0Q�@0 �@0b@0b@0b@0b@0 �@0b@/��@/\)@/
=@.ȴ@.��@.�+@.��@.V@.5?@.$�@.{@-��@-p�@-O�@-O�@,�/@,��@,I�@+�F@+�@+"�@*��@*^5@*=q@*-@*�@*�@*�@*J@)�@)&�@(�9@(bN@(Q�@(A�@(  @'K�@&��@&�+@&$�@%�T@%��@%�@%`B@$�@$9X@#ƨ@#S�@#"�@#o@"�@"�H@"��@"~�@"n�@"=q@"�@"�@"�@"�@!��@"J@!��@!�@ �`@ Ĝ@ �@ Q�@ A�@  �@ b@�;@��@�P@;d@�y@E�@�T@@��@O�@V@�@/@?}@V@�@�D@j@9X@��@t�@t�@dZ@33@o@�H@��@~�@n�@M�@�@�@J@�^@7L@&�@�`@�u@r�@Q�@1'@��@l�@;d@
=@��@�@�@�R@��@V@5?@{@@�@�@�T@�h@/@�@�D@I�@9X@(�@�m@��@S�@�H@�\@=q@��@��@��@hs@X@7L@��@�9@Ĝ@��@�@r�@Q�@A�@ �@  @��@K�@�@�@
=@�@��@ȴ@�R@��@��@��@v�@$�@�h@O�@/@/@V@�@��@��@�D@�D@j@Z@9X@�@��@�F@t�@C�@@
��@
�!@
�\@
~�@
M�@
-@
�@	�@	�#@	x�@	G�@	G�@	7L@	&�@	�@	�@	�@��@�`@�9@�9@��@�u@�@bN@1'@ �@ �@b@�@��@�w@�@|�@�@��@�@�+@ff@E�@{@�T@�-@�@O�@/@�@�/@��@�@��@z�@Z@9X@1@�m@ƨ@�F@��@t�@C�@"�@�H@��@~�@n�@=q@-@�@�#@�^@�^A��;A��mA��HA��HA��TA��A��`A��HA��HA��`A��yA��A��A��yA��mA��yA��yA��TA��TA��`A��TA��TA��`A��`A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��yA��A��A��yA��`A��HA��TA��yA��A��A��yA��mA��#A��yA��`A��/A��
A���A��!A���A���A��A���A���A���A��hA��A�x�A�r�A�n�A�`BA�G�A�A�A�C�A�C�A�A�A�=qA�9XA�7LA�-A�(�A�"�A��A��A��A�{A�{A��A��A��A��A��A�VA�1A�A�A���A���A���A���A���A���A��A��`A���A�A��wA��RA��FA��-A��A���A���A���A���A���A��uA��PA��+A�~�A�n�A�hsA�^5A�\)A�ZA�VA�-A��A��A�{A�bA�A��A��A��A��A��A��A��A��A��mA��A��A��TA��HA��;A��;A��/A��A��
A��A���A���A�ȴA�ĜA�ƨA�ȴA�ĜA���A�A�ĜA�ĜA�ƨA�A�A�A��wA��jA��jA��jA��jA��RA��FA��-A��A���A���A���A���A���A���A���A���A���A���A���A��\A��\A��PA��PA��PA��PA��PA��+A�|�A�t�A�r�A�p�A�p�A�n�A�l�A�^5A�M�A�K�A�O�A�M�A�I�A�G�A�I�A�I�A�I�A�G�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�;dA�9XA�7LA�7LA�33A�1'A�/A�-A�(�A�$�A��A�A��yA��;A��A���A���A�ƨA�A��RA��FA��9A��A���A���A��hA�v�A�l�A�bNA�\)A�VA�E�A�"�A���A��jA���A���A�`BA�=qA�5?A�+A�&�A�{A��TA��FA���A��PA�bNA�?}A��A��A��hA�|�A�`BA�5?A�1'A��A��FA���A�C�A�1'A��A��HA��A�?}A�5?A�VA��wA��A�dZA�K�A�JA��wA�bNA���A��A��yA�r�A�(�A��-A���A�r�A�{A���A���A�r�A�7LA�A��A���A���A���A�A���A�ffA�A�S�A��
A��A���A�A��RA��A�hsA�K�A�(�A��A��HA���A�S�A�ȴA���A�|�A�l�A�ffA�ZA�K�A�G�A�7LA�"�A�VA�A�  A���A��A���A���A��A�\)A�K�A�=qA�9XA�5?A�(�A��mA��^A���A���A��+A�/A��A���A�p�A�jA�dZA�O�A�VA�%A��A���A���A�n�A�VA�~�A��A��FA��A��!A��!A��A���A���A���A��DA�r�A�hsA�^5A�\)A�S�A�;dA�"�A��A�A���A��A��yA��mA��#A�A��A�  A���A�7LA�{A���A��/A���A���A��A���A���A��7A�t�A�l�A�r�A�r�A�VA�Q�A�C�A�/A��A�A���A�S�A���A��PA�9XA�+A�$�A�"�A� �A��A��A�{A�1A���A���A���A��A��yA��`A��A�ĜA���A��+A�5?A���A�~�A�C�A�~�A�ffA�^5A�G�A�(�A� �A��A��A�oA�{A�oA�bA�oA�
=A���A���A��\A�t�A�;dA�"�A�VA��A��;A���A�ĜA��-A��!A���A��A�hsA�O�A�?}A�33A�1'A�(�A� �A�bA�A��`A�ȴA��RA��A���A��A�v�A�hsA�VA�?}A�33A��A��/A��7A�G�A��A���A��HA���A���A��uA��A�hsA�K�A�33A�A���A��^A���A���A��PA�~�A�r�A�M�A�1'A�+A�&�A��
A��uA�|�A�^5A�G�A�(�A��A���A��!A���A���A��uA�z�A�n�A�\)A�G�A�=qA�-A�bA�
=A���A���A��
A��jA��A���A�p�A�A�A�+A�JA���A��;A���A�ƨA��9A���A��A�t�A�jA�XA�;dA�A��#A���A���A��wA�p�A�{A���A��A���A�p�A�E�A�"�A��A~�HA}��A}K�A|��A|�DA|JA{�#A{�wA{�A{7LAz�`Az�9Az��Az�Az~�AzAy�;Ay��Ay�Ay|�Ay+AyAx��Ax5?Aw�mAw��AwAw��Aw�PAw�PAwhsAw33Aw&�AwVAv�/Av��Av~�AvbNAu��Au��Au��Au�AudZAuAt��At��At�At~�Atr�Atn�At^5AtA�At5?AtJAs�
Asp�Ar�\Aqx�AqS�Aq33Aq�Ap��Ap~�ApE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                        A��TA��mA��`A��yA��yA��`A��HA��A��A��A��A��A��A��yA��`A��9A��DA�M�A�33A��A�{A�A��A��^A���A�~�A�K�A�1A��A��TA���A�A���A��A���A��DA�l�A�I�A�A�A�=qA�+A��A���A� �A�=qA�-A�z�A��hA�|�A��!A���A�A��;A��A�\)A�%A�n�A��HA�ƨA���A��A���A�ffA�JA��+A���A��uA�XA�p�A�&�A�%A��
A��RA�A�A�{A��RA���A��uA�-A���A�dZA��uA��A�A�|�A��;A�1A�|�A��A���A���A��+A��yA��AC�A|1'Az��Ay�^AxI�Awp�Av�!Au�At�As�Aq`BAp1'An�HAn=qAm�wAl��AkVAj�Aj�Ai�wAiXAfA�AdA�AcG�Aa"�A^$�A\�jA\~�A[dZAZAWXAVjAV{AU|�ATbAS|�AR�yAQ�APQ�AO�7AN��ANM�AM�mAMO�AL-AJ�HAI��AH�\AG��AF�yAF�DAE�AD�jAC��AB�RAB5?AA/A@�A?��A>��A=hsA<��A<�A<r�A:ZA8��A8M�A7�
A7O�A6��A5�A4�RA3�A3�-A3�A1�mA1K�A1+A/��A/|�A/hsA/K�A//A.^5A-��A-`BA-7LA,�/A+��A+"�A*-A)hsA)�A(�A'�
A&��A%�-A%;dA$�A#p�A#G�A#"�A!�#A �`A ĜA ZA�A��AVAI�A�A��A1'A  A�PA��A�A�\AAM�A��AffA=qA�mA`BA�`A��A�A9XAhsA7LA�A
=A
�`A
ȴA
�RA
�+A
VA	�hA&�A��AE�A�AƨA?}A��AS�AjA�A�A��AVA �A ��A -@���@�-@���@�|�@�=q@��-@�1'@�5?@��/@��;@�x�@@�x�@�@�+@�^5@�u@��@�$�@�/@�E�@���@�j@�@�  @ܣ�@�v�@���@ו�@��T@�I�@�  @ӝ�@�@�?}@�1'@�S�@�~�@ͩ�@���@˶F@ʏ\@ɺ^@�bN@��@��T@�Q�@�(�@�33@�J@��-@��@���@��m@�;d@��@���@�5?@��@��`@��u@��P@�n�@���@��P@�;d@�+@��@��
@��/@�\)@��@�{@���@�hs@���@��F@�33@�@���@�=q@��h@��/@�  @�S�@��!@��@���@��w@�;d@��y@��!@�n�@�@�?}@�%@��/@��9@��@��@�33@�n�@���@��@�9X@��m@�dZ@��@�^5@���@��h@�`B@��@���@�Q�@��@�|�@�"�@��@��y@��R@�ff@��@���@�?}@�(�@��@�\)@�o@���@�^5@�{@��#@�&�@�I�@��;@�S�@��!@�^5@�E�@�$�@�@���@���@�hs@��@��@�I�@���@��F@�|�@�C�@�
=@��@��@���@�V@�-@�@��#@���@�O�@��/@�Q�@�(�@�  @��m@�ƨ@�C�@�o@�
=@�@��H@���@�~�@�M�@��@�p�@�7L@��@���@���@��@�1@��;@�ƨ@��P@��@�V@�J@��^@�hs@�&�@�%@��@���@�Ĝ@��j@��9@���@���@���@��u@� �@~��@}��@|Z@|�@{��@{�F@{S�@z�@z~�@y�@y�7@y&�@xĜ@x1'@w�@wl�@w
=@vff@vE�@v@up�@t�/@t��@tj@t1@s��@s�@s�@s�@s33@s"�@r��@r-@q�@qhs@pbN@pA�@pA�@p �@o��@o;d@n��@nV@n$�@n{@m�-@mV@l��@l�/@lZ@l�@l1@l1@k��@kƨ@kS�@ko@j�\@i��@iX@iG�@h��@h�u@hr�@hQ�@h �@h  @g��@g|�@g;d@g�@fȴ@f�+@f5?@f@e�h@e�@d�/@d��@dZ@c�F@c��@cS�@c@b�!@b��@bn�@bM�@bJ@a�@a��@a��@ahs@a&�@a%@`��@`�9@`A�@_
=@^�+@^V@^{@]�@]�h@\z�@\9X@\(�@\�@[S�@[o@Z��@Z=q@ZJ@Y��@YX@Y&�@Y�@X��@X�u@X �@W�;@W��@W|�@W|�@Wl�@W
=@V��@V{@U�@U@U?}@T�j@T�@Sƨ@S��@S�@So@R��@R��@R��@R�\@R^5@R-@R�@Q��@Q�^@QX@P�`@PQ�@O�@O�w@O�P@Ol�@OK�@N�R@NE�@Nff@NV@NV@NV@Nff@Nff@N$�@M�@MV@L��@L�/@L�@L��@L��@L�D@Lj@LZ@L9X@L(�@K�m@KC�@J~�@JJ@I��@Ihs@I%@HĜ@H�@HA�@H �@H  @G�P@F��@F�@F�R@Fv�@F$�@E�@E��@E�-@Ep�@EO�@E/@D��@D(�@C�F@C�@C"�@B�!@Bn�@BM�@A�#@A�#@A��@A�@A�@@�9@@Q�@?��@?�P@?\)@?;d@?
=@>�+@>V@>V@>V@>E�@=�@<��@<�D@<9X@;��@;�
@;ƨ@;��@;��@;�@;�@;�@;t�@;t�@;dZ@;S�@;S�@;33@;"�@:�@:��@:n�@:=q@9��@9x�@9&�@8Ĝ@8�u@81'@7�;@7�w@7��@7��@7�;@7�;@7��@7�P@7\)@65?@5�T@5�@4��@4��@4�@4��@4z�@4I�@4�@2�!@1�@0�`@0�`@0��@0��@0��@0��@0��@0Ĝ@0�9@0r�@0bN@0Q�@0 �@0b@0b@0b@0b@0 �@0b@/��@/\)@/
=@.ȴ@.��@.�+@.��@.V@.5?@.$�@.{@-��@-p�@-O�@-O�@,�/@,��@,I�@+�F@+�@+"�@*��@*^5@*=q@*-@*�@*�@*�@*J@)�@)&�@(�9@(bN@(Q�@(A�@(  @'K�@&��@&�+@&$�@%�T@%��@%�@%`B@$�@$9X@#ƨ@#S�@#"�@#o@"�@"�H@"��@"~�@"n�@"=q@"�@"�@"�@"�@!��@"J@!��@!�@ �`@ Ĝ@ �@ Q�@ A�@  �@ b@�;@��@�P@;d@�y@E�@�T@@��@O�@V@�@/@?}@V@�@�D@j@9X@��@t�@t�@dZ@33@o@�H@��@~�@n�@M�@�@�@J@�^@7L@&�@�`@�u@r�@Q�@1'@��@l�@;d@
=@��@�@�@�R@��@V@5?@{@@�@�@�T@�h@/@�@�D@I�@9X@(�@�m@��@S�@�H@�\@=q@��@��@��@hs@X@7L@��@�9@Ĝ@��@�@r�@Q�@A�@ �@  @��@K�@�@�@
=@�@��@ȴ@�R@��@��@��@v�@$�@�h@O�@/@/@V@�@��@��@�D@�D@j@Z@9X@�@��@�F@t�@C�@@
��@
�!@
�\@
~�@
M�@
-@
�@	�@	�#@	x�@	G�@	G�@	7L@	&�@	�@	�@	�@��@�`@�9@�9@��@�u@�@bN@1'@ �@ �@b@�@��@�w@�@|�@�@��@�@�+@ff@E�@{@�T@�-@�@O�@/@�@�/@��@�@��@z�@Z@9X@1@�m@ƨ@�F@��@t�@C�@"�@�H@��@~�@n�@=q@-@�@�#@�^G�O�A��;A��mA��HA��HA��TA��A��`A��HA��HA��`A��yA��A��A��yA��mA��yA��yA��TA��TA��`A��TA��TA��`A��`A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��yA��A��A��yA��`A��HA��TA��yA��A��A��yA��mA��#A��yA��`A��/A��
A���A��!A���A���A��A���A���A���A��hA��A�x�A�r�A�n�A�`BA�G�A�A�A�C�A�C�A�A�A�=qA�9XA�7LA�-A�(�A�"�A��A��A��A�{A�{A��A��A��A��A��A�VA�1A�A�A���A���A���A���A���A���A��A��`A���A�A��wA��RA��FA��-A��A���A���A���A���A���A��uA��PA��+A�~�A�n�A�hsA�^5A�\)A�ZA�VA�-A��A��A�{A�bA�A��A��A��A��A��A��A��A��A��mA��A��A��TA��HA��;A��;A��/A��A��
A��A���A���A�ȴA�ĜA�ƨA�ȴA�ĜA���A�A�ĜA�ĜA�ƨA�A�A�A��wA��jA��jA��jA��jA��RA��FA��-A��A���A���A���A���A���A���A���A���A���A���A���A��\A��\A��PA��PA��PA��PA��PA��+A�|�A�t�A�r�A�p�A�p�A�n�A�l�A�^5A�M�A�K�A�O�A�M�A�I�A�G�A�I�A�I�A�I�A�G�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�C�A�A�A�;dA�9XA�7LA�7LA�33A�1'A�/A�-A�(�A�$�A��A�A��yA��;A��A���A���A�ƨA�A��RA��FA��9A��A���A���A��hA�v�A�l�A�bNA�\)A�VA�E�A�"�A���A��jA���A���A�`BA�=qA�5?A�+A�&�A�{A��TA��FA���A��PA�bNA�?}A��A��A��hA�|�A�`BA�5?A�1'A��A��FA���A�C�A�1'A��A��HA��A�?}A�5?A�VA��wA��A�dZA�K�A�JA��wA�bNA���A��A��yA�r�A�(�A��-A���A�r�A�{A���A���A�r�A�7LA�A��A���A���A���A�A���A�ffA�A�S�A��
A��A���A�A��RA��A�hsA�K�A�(�A��A��HA���A�S�A�ȴA���A�|�A�l�A�ffA�ZA�K�A�G�A�7LA�"�A�VA�A�  A���A��A���A���A��A�\)A�K�A�=qA�9XA�5?A�(�A��mA��^A���A���A��+A�/A��A���A�p�A�jA�dZA�O�A�VA�%A��A���A���A�n�A�VA�~�A��A��FA��A��!A��!A��A���A���A���A��DA�r�A�hsA�^5A�\)A�S�A�;dA�"�A��A�A���A��A��yA��mA��#A�A��A�  A���A�7LA�{A���A��/A���A���A��A���A���A��7A�t�A�l�A�r�A�r�A�VA�Q�A�C�A�/A��A�A���A�S�A���A��PA�9XA�+A�$�A�"�A� �A��A��A�{A�1A���A���A���A��A��yA��`A��A�ĜA���A��+A�5?A���A�~�A�C�A�~�A�ffA�^5A�G�A�(�A� �A��A��A�oA�{A�oA�bA�oA�
=A���A���A��\A�t�A�;dA�"�A�VA��A��;A���A�ĜA��-A��!A���A��A�hsA�O�A�?}A�33A�1'A�(�A� �A�bA�A��`A�ȴA��RA��A���A��A�v�A�hsA�VA�?}A�33A��A��/A��7A�G�A��A���A��HA���A���A��uA��A�hsA�K�A�33A�A���A��^A���A���A��PA�~�A�r�A�M�A�1'A�+A�&�A��
A��uA�|�A�^5A�G�A�(�A��A���A��!A���A���A��uA�z�A�n�A�\)A�G�A�=qA�-A�bA�
=A���A���A��
A��jA��A���A�p�A�A�A�+A�JA���A��;A���A�ƨA��9A���A��A�t�A�jA�XA�;dA�A��#A���A���A��wA�p�A�{A���A��A���A�p�A�E�A�"�A��A~�HA}��A}K�A|��A|�DA|JA{�#A{�wA{�A{7LAz�`Az�9Az��Az�Az~�AzAy�;Ay��Ay�Ay|�Ay+AyAx��Ax5?Aw�mAw��AwAw��Aw�PAw�PAwhsAw33Aw&�AwVAv�/Av��Av~�AvbNAu��Au��Au��Au�AudZAuAt��At��At�At~�Atr�Atn�At^5AtA�At5?AtJAs�
Asp�Ar�\Aqx�AqS�Aq33Aq�Ap��Ap~�ApE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�/B��B��B��B�B��B�B�WB�WB��B�QB�QB�B�B��B�B�sB�/B��B�rB�	B�lB�2B��B��B�ZB��B�B��B��B�B��B�B��B�QB�DB��B�2B��B�B��BیB��B�^B��B�'B|�BHB�(B�&B�B�VB�%Bu�Ba�B]�BZ�BS�BR�BIRB<6B%�B�B�BB�B�PB�B�	B�)B��B�B��B��B��B�B��B��B��B�\B��B��Bt�Bm�Bd�B]�BR BGEB@B:�B/B%FBqB�B�B�sB�]B��B��B�B�dB��B��B��B�7B��B��B~�By	Bx8BjBe�Ba�B\]BV�BUgB9�B6FB.}BxBB�B�B�B
�ZB
�B
�B
��B
�BB
�?B
�B
бB
͟B
ʌB
ȀB
�9B
�'B
�HB
�nB
�$B
��B
��B
�	B
�_B
��B
� B
��B
�%B
�iB
~(B
~(B
w�B
v+B
r�B
k�B
f�B
c�B
`BB
c�B
W�B
VmB
T,B
QNB
M�B
L�B
GB
?HB
=<B
9�B
5tB
/B
.}B
+�B
'RB
&�B
%�B
$B
$tB
�B
�B
qB
�B
SB
:B
�B
	B
�B
_B
�B
oB	��B	��B	�B	�B	�B	�B	��B	�B	�
B	�2B	�B	��B	�]B	�,B	�vB	�B	��B	�XB	ȀB	�3B	� B	��B	��B	�9B	�'B	�B	�B	�6B	�_B	��B	�tB	��B	�tB	��B	�YB	��B	��B	�B	�B	��B	��B	��B	�@B	� B	��B	�lB	�fB	��B	��B	�uB	�SB	�B	|�B	{�B	{�B	z�B	y	B	xlB	v�B	v�B	s�B	r�B	qAB	o�B	m)B	k�B	lWB	g�B	ffB	gB	dZB	bB	a|B	_�B	^B	_�B	\�B	\�B	Z�B	^jB	YB	XyB	W
B	XEB	ZB	XEB	V�B	V�B	W�B	W?B	VmB	U�B	U�B	XEB	VB	VB	V�B	VmB	V�B	YB	YB	YB	ZQB	[�B	\]B	^jB	]dB	`B	_pB	`B	`vB	`�B	bNB	b�B	b�B	b�B	cTB	d�B	c�B	dZB	e`B	h
B	iDB	k�B	jB	iDB	g�B	r�B	x8B	xB	y>B	{B	|B	{�B	|�B	�AB	�B	�;B	�oB	��B	�MB	�{B	��B	�B	��B	�B	��B	�MB	�B	�YB	��B	��B	�kB	�~B	�~B	��B	��B	�OB	��B	�:B	��B	�B	�}B	��B	��B	�3B	��B	��B	��B	�B	��B	�BB	�B	��B	ĜB	ŢB	�zB	ȀB	�KB	ɆB	��B	�dB	�dB	�B	�
B	�KB	ںB	�)B	ݘB	�jB	�pB	�BB	�ZB	�B	�B	�iB	�MB	�ZB	��B	�+B	�2B	�lB	��B	�B	��B	��B
�B
�B
�B
fB

	B
�B
JB
�B
�B
bB
4B
oB
B
FB
SB
�B
IB
B
�B
VB
�B
$@B
%B
%B
%FB
&LB
($B
(�B
)�B
-CB
0UB
1�B
2�B
2�B
49B
6�B
:*B
:�B
;dB
<�B
A�B
F�B
IRB
K�B
N�B
QB
R B
S&B
S�B
T,B
TaB
T�B
UgB
UgB
U2B
T�B
YB
`�B
c�B
jB
kQB
k�B
l�B
m�B
o5B
qAB
s�B
uZB
w2B
y>B
|B
}�B
.B
��B
��B
��B
�MB
��B
�1B
��B
��B
��B
��B
�"B
�"B
�"B
��B
��B
� B
�B
��B
��B
�B
�CB
�CB
��B
��B
��B
�B
�FB
�LB
�B
��B
��B
�B
�B
�-B
��B
��B
��B
��B
�nB
��B
��B
�$B
��B
��B
�^B
�dB
��B
�qB
��B
��B
��B
�OB
�[B
��B
ĜB
��B
�B
�KB
�RB
̘B
��B
�HB
�NB
�TB
��B
�2B
�9B
�?B
��B
��B
�QB
چB
��B
�)B
��B
��B
�5B
�pB
�pB
�BB
�vB
�B
�DB
��B
�B
��B
��B
� B
��B
�B
��B
��B
��B
�+B
�2B
�>B
�rB
�B
��B
��B
��B
��B
�]B
��B
��B
��B iBBAB�BMB�B�B�B	B
rB�B~BJB�BVB�B(B(B\B�BbB�B4B�B�BB�B�B�B�B�B�B�B�B�BB�BVB�B 'B!�B#�B$�B%B%FB%�B&B&B&B&�B&�B&�B&�B'RB(�B*�B+�B,qB-wB.}B/B/�B0!B0�B0�B1�B2aB2�B2�B3hB4B4nB4�B4nB4�B5?B5�B6FB8�B8�B8�B8RB8B7�B7�B8�B8RB9$B9�B9�B:�B;�B<�B=<B=�B=�B>BB?�B?�B?�B@B@�BB�BC�BC-BB�BB[BB�BB�BC-BC-BC�BC�BC�BC�BC�BD3BDgBDgBD�BDgBD�BE�BE�BF?BF�BG�BH�BIBIRBJ#BJ�BK)BK�BK^BK�BK�BM6BMjBNpBO�BO�BPHBQNBP�BQBQBQBQBQNBT�BV�BV�BV�BV�BW
BW
BW?BW
BW
BW
BWsBW�BXBYBYKBY�BY�BY�BZBZ�B[#B[�B[�B]/B^B^B]�B^�B^�B^�B^�B`B`�BaB`�Ba�BbNBb�BcTBcTBc�Bd�Be`Be`Be�Be�Be�Be�Bf2Bf2Bh>Bh�BiBh�Bh�Bh�BjBkBkBk�Bl"BlWBlWBlWBm�BncBo5Bo�BpBp;BpBoiBn/Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm]Bm)Bm)BncBn/BncBo5BoiBoiBo�BoiBoiBoiBoiBo�Bo�BpoBqABqBqBq�Br�Bs�Bs�Bs�BtTBt�Bt�Bt�Bt�Bv�Bv�Bv�BwfBw�BxBx8Bx�Bx�By	By	By�By>By>BzDBzxBz�B{B{B{�B{�B{�B|�B|�B|�B}VB}�B}�B}�B}�B}�B~(B~]B~�B~�B~�B~�B~]B~�B.BcB�iB�iB�iB��B��B�;B�oB�AB�uB�GB�{B�{B��B��B��B�B�B�B�B�SB�SB��B��B��B�%B�YB��B�_B�+B�+B�_B�+B�_B��B��B��B��B�_B��B��B��B�7B�7B�7B�lB��B��B�	B�	B�	B�	B�	B�	B�=B�rB�B�DB�xB�B�B�JB�JB�JB��B�~B��B��B��B��B�PB�PB�PB��B��B�PB�PB��B��B�"B��B�"B��B�"B�"B�VB�VB��B��B�"B�VB��B�VB��B�\B��B�.B�.B�bB�bB� B� B�hB��B��B��B�hB�B��B�B��B�.B�bB��B� B� B��B��B� B�4B�hB�hB�B�:B�oB�oB��B��B�B�uB�uB�@B��B�B��B��B�B�)B�;B�B��B�]B�"B��B�]B��B��B��B�cB��B��B�B�B��B�WB��B�B��B�]B�]B�)B�WB��B�QB��B��B�WB��B�QB�KB�B��B�B��B�B�B�B�QB��B�B��B�
B�>B��B�B�B�yB�B�B��B�
B�B�KB�&B�yB�B�fB�B�B�B�mB��B�yB��B��B��B�B�5B�B�B��B��B��B��B��B�>B�DB��B�DB��B�rB�>B��B��B�B��B�	B��B�lB��B��B�lB�B�+B�`B�+B�2B�B��B��B��B�%B��B��B�%B��B�ZB�`B��B��B�+B�B�+B�+B��B�|B�B�B�B�B��B��B� B� B��B��B�B�)B��B�WB�B�"B�B�B�"B��B�/B�B��B�/B�/B�B��B��B�]B�cB��B��B�B�QB�B�"B��B�B�B�B�QB��B�B�WB� B�B��B��B�B�]B��B��B��B��B�B��B�WB��B�WB��B�WB�QB��B��B�B�B�DB�sB�sB�
B��B�yB��B�B�2B��B�B�B�B�B��B�B�`B��B�`B�2B��B�B�B��B�`B�B�,B��B�ZB��B�B�B��B�B�B�&B� B� B�B��B�TB�B�B�NB�B��B�B�B��B�5B�]B�)BچB�nB��B�EB�yB�B�B�B��B��B҉B��B�B�jB�B�pB�RB�6B��B�BB�B�nB��B��B��B�aB�3B�}B�tB��B�bB�4B��B�OB��B��B��B�YB~]B~�B��B��By>BV�BV�BNpB��B�B�BB�B	�B�B �BMB�cB�.B�B�JB��B�B��B�vB�EB�#B�#B��B�0B�jB�B�-B�IB��B�zB��B�CB�\B�B�hB��B��B��B��B��B�+B{Bz�B{BzB}VBn�Bt�Bv�B�Bj�Bc�BaB_�Bc�Ba�B`vB`BB`vB^jB]dB\�B\)B]/B`�B^�B_pBV�BZQBV9BS�BQ�BR�BffBW�BQ�BL�BJ�Bb�BQ�B\�BMjBL�BMjBPHBN�BE9BGEBC�BEB9XB?�BK�B;�B/B'�B&�B&B%zB'�B&B$�B(�B$�B�B�B�B~B�B�B�B�BMB�B�B�B�BB�B�B:B�BAB�BB��B��B	7B�VB�B�]B��B�lB��B��B��B�fB�B�PB��B�lB�|B��B�B�"B�ZB�pB�B�WBٴB�QB��BרB�mBԕB��B��B��B�TB�HB�NB�pB�dB͟BбB�2B�wB�-B��B�[B�wB��B�aB��B�B��B�B��B��B��B��B��B��B�'B�B�:B�eB��B��B��B��B�	B�CB�=B�$B��B�7B�kB�B�B��B�:B�:B��B�B��B�@B��B��B�B��B�B��B�1B��B��B��B�B��B�YB�B~(By�Bx8By	BtBq�BrBq�BqBn�Bq�Bm�BgmBgmBd&Bc�Bc Bc�Be�B\)B\)B[�Bn�BZQBW
BV9BR�BU�BP�BX�BK^BH�BH�BIRBIBEmBF?BC�BB[BA�BB�B<6B=qB=qB=<B<B9�B7�B7LB9�B3�B5�B.}B-�B)�B)�B+6B)�B%�B!�B!�B �B"�B#�B�B�B{BSB"�BuB�B	B�BB�B 4B�B	B�]B�B��B�B�B��B�B�pBߤB��BیB�B�9B�dB�
B�gB��B��BѷB�TB��BуB�<B��B�3B�'B��B�B�B��B��B��B�BB�<B��B��B��B�XB��B��B��B�aB��B��B�=B��B�$B��B��B�B�LB��B��B�-B�}B�B��B�B��B��B��B�1B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020122115531520201221155315IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164420210325101644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                