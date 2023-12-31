CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-09-15T23:20:29Z creation; 2022-02-04T23:30:04Z DMQC;      
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
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210915232029  20220204223517  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_182                 6810_008521_182                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٓ���0@ٓ���011  @ٓ�C,�@ٓ�C,�@0�Zp��U@0�Zp��U�d�`�V.�d�`�V.11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�?��H@B�\@�  @�  @�G�@�G�@�p�A  A   A,(�A?\)A^�RA\)A�  A��A�  A�Q�A�  A�  A�  A��B�B  B(�B (�B(Q�B0(�B7�
B@  BG�
BPQ�BX(�B`(�Bg�
Bo�
Bx(�B�  B�{B�  B�  B�{B�(�B�  B��B�  B�{B�{B�  B�  B�{B�  B�{B�(�B�  B�{B�  B�  B�(�B�(�B�{B�{B��B��B��
B�B��
B��B��B��C
=C
=C  C{C
{C
=C{C{C��C�C�C�C�C�C  C 
=C"  C$  C&�C(
=C*  C,  C-��C/��C2  C3��C5��C8  C:  C<  C>  C@
=CA��CC��CF  CH{CJ
=CL
=CN  CO��CR  CT  CV
=CX
=CZ
=C\  C^  C_��Cb  Cd  Ce��Cg��Cj  Cl  Cn
=Cp  Cr  Ct  Cv
=Cx{Cz
=C{�C}�C�C���C�  C�
=C�  C���C�  C�C���C���C�  C�  C�  C�  C���C���C�C�
=C���C�  C�C�C���C�  C�
=C�C���C���C�
=C�  C���C�  C���C���C�C���C���C�  C�C���C�C�  C�  C�  C�  C�C�  C���C�  C�  C�
=C�  C�  C���C�  C���C���C���C�C�C�  C���C�  C�
=C�  C���C���C���C�  C�  C�C���C���C���C���C�  C�C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�  C���C���C�  C�C�C�C�  C�  C�  C�
=C�
=C�  C���C�C�C�  C�  C���C���C�  C�C���C���C�  C���C���C�C�  C���C���C���C���C���C�  C�C�  C�  C�  C���D   D � D �qD}qD  D� D�qD}qD�D� D�D��D  D� D  D� D�qD� D	  D	}qD	��D
}qD�D��D  D� D  D��D�D� D  D�D  D� D  D}qD  D��DD� D�D� D�qD}qD  D}qD�qD� D�D� D  D� D�D}qD�D��D�D}qD�qD��D�D��D�D}qD�qD ��D!  D!� D"�D"}qD"�qD#� D$  D$}qD$�qD%� D&D&��D'  D'� D(  D(� D(�qD)� D*  D*��D*�qD+� D,�D,}qD-  D-��D.  D.}qD.�qD/� D0�D0� D1�D1� D2  D2}qD3�D3��D3�qD4��D5�D5� D6  D6��D7  D7� D8  D8}qD9  D9}qD9�qD:}qD:�qD;}qD<  D<� D=�D=��D>  D>� D>��D?z�D@  D@� DA  DA� DB�DB��DC  DCz�DC��DD}qDE  DE�DFDF��DG  DG� DH  DH}qDH��DI}qDJ�DJ��DJ�qDK}qDL  DL��DM  DM��DN�DN��DODO� DP  DP��DQ  DQ}qDR  DR��DS�DS}qDS�qDT� DU�DU�DV  DV� DW�DW� DX  DX��DYDY��DZ�DZ}qDZ�qD[� D[��D\� D]  D]� D]�qD^}qD_  D_� D_�qD`}qDa  Da�Da�qDbz�Db�qDc��Dc�qDdz�De�De��De�qDf� Dg  Dgz�Dg��Dh� Di  Diz�Di�qDj}qDk  Dk��Dk�qDl}qDm  Dm��DnDn��Do  Do�Dp�Dp� Dq  Dq� Dq�qDr��Ds�Ds��Dt�Dt��Du  Du�Dv�Dv�DwDw��Dx  Dx��Dy  Dy}qDy�qDz� D{  D{��D|�D|��D}D}�D~�D~}qD  D� D�qD�>�D�~�D�� D��D�@ D��HD�D��D�AHD�� D�� D�  D�@ D�~�D��qD���D�AHD���D�D���D�@ D��HD��HD�  D�>�D�� D���D���D�@ D��HD��HD�HD�B�D��HD���D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�~�D��qD���D�@ D��HD�D�  D�@ D��HD�� D��qD�>�D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�  D�AHD�~�D�� D���D�>�D�� D�D��D�>�D�~�D�� D�  D�@ D�~�D��HD�  D�@ D��HD�� D���D�@ D��HD���D�  D�AHD��HD�� D���D�@ D���D�� D��qD�@ D��HD�� D�  D�AHD��HD���D���D�>�D�~�D�� D���D�=qD�� D��HD�  D�>�D�� D�� D�HD�B�D��HD���D�HD�AHD�� D�D��D�@ D�~�D��qD��qD�@ D�� D��HD�HD�>�D�� D��HD���D�>�D�� D�� D�  D�B�D���D��HD�  D�>�D�� D�� D�  D�@ D�}qD��qD�  D�@ D�� D���D���D�AHD��HD���D���D�@ D�� D�D�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�B�D��HD���D��qD�@ D��HD��HD���D�>�D��HD��HD��D�@ D�~�D�� D�  D�@ D��HD���D��D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D��HD�  D�>�D�|)D���D�  D�AHD��HD�� D���D�>�D�� D��HD��D�AHD�~�D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�AHD��HD��HD�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D��HD���D�>�D�~�D�� D�  D�@ DÁHD�� D���D�@ DĀ D��HD���D�=qDŀ D�� D�  D�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ DȀ D�� D�  D�AHDɁHD�� D�  D�@ DʁHD�D�HD�>�D�~�D��HD�HD�AHD̀ D�� D���D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�>�Dπ D�� D���D�@ DЀ Dо�D�  D�AHDсHDѾ�D���D�@ DҀ D�� D�  D�AHDӀ D�� D�HD�@ DԀ DԾ�D��qD�@ D�~�D�� D�  D�AHDր D־�D�  D�AHDׁHD�� D�HD�AHD؁HDؾ�D��qD�>�D�~�D�� D��D�@ Dڀ D��HD�HD�AHDۂ�D�D�HD�@ D�~�Dܾ�D���D�>�D�~�D�� D�  D�@ DށHD�� D�  D�>�D�~�D�� D�HD�AHD�� D�qD�  D�B�D�HD�� D�HD�B�D₏D�D��D�@ D�~�D㾸D��qD�>�D� D�� D�  D�@ D�~�D徸D���D�>�D�HD澸D���D�@ D�HD�D�HD�@ D� D辸D�  D�@ D� D龸D���D�=qD�~�D꾸D�  D�@ D�~�D뾸D���D�@ D�HD�� D��qD�@ D�HD��HD���D�@ D�HD��HD�HD�AHDD�D��D�AHD�~�D�D�HD�AHD�HD�� D�HD�AHD�HD��HD�HD�AHD�~�D�D�  D�@ D� D��HD�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�>�D�� D���D�  D�AHD��HD�� D��?#�
?8Q�?k�?���?��?�@�@�R@333@B�\@Q�@fff@z�H@��
@��@�
=@�G�@��@���@��H@��@˅@�33@޸R@���@��@�
=A ��AffA
�HA�RA�A�A��A   A#�
A(��A.�RA1�A6ffA<(�AAG�AE�AHQ�AN�RAS�
AVffAZ�HA`��Adz�AhQ�Amp�Ar�\AvffAz=qA\)A��\A�z�A�{A���A�33A��A�
=A�G�A��
A��RA�Q�A�=qA���A�\)A�G�A�33A�p�A�Q�A��HA�z�A��RA�G�A��A�p�A��A��\A���A��RA�Q�A\A��AǮA���A��HA��AϮA��A�(�A�{A׮Aٙ�AۅA�{A�Q�A�=qA��
A�{A��A��HA�z�A�{A��A�A��A�
=A�G�A�(�A�ffB   B ��B=qB�Bz�BG�B�RB�
B��B	B
=BQ�BG�B{B\)B��B�B�RB�B��BffB\)BQ�Bp�B�HB  B��BB
=B Q�B!��B"ffB#\)B$��B%�B&�HB'�
B)G�B*ffB+33B,(�B-p�B.�RB/�B0z�B1B3
=B3�
B4��B6{B7\)B8Q�B9�B:=qB;\)B<��B=B>�\B?�B@��BB{BB�HBC�BDz�BE��BF�RBG�BH(�BIG�BJ�\BK�
BL��BN{BO33BP��BQ�BS
=BT  BU�BV�\BW�BX��BYB[
=B\��B]p�B^ffB_�Ba�Bb=qBc
=BdQ�Be��Bf�HBg�
Bh��Bj{Bk�Blz�Bmp�Bn�RBp(�Bq�Br{Bs33Bt��Bu�Bw
=Bw�
Bx��BzffB{�B|z�B}B33B�=qB���B�G�B�  B��RB�33B��B�Q�B�
=B��B�(�B��RB�\)B�(�B��RB��B��
B��\B��B���B�(�B��HB���B�{B��\B�G�B�  B��\B��B���B�Q�B�
=B��B�{B���B��B��B�z�B�
=B��
B�ffB���B�p�B�(�B���B�p�B�  B�z�B�
=B�B�ffB���B��B�  B���B�p�B�  B�z�B��B��
B�z�B�
=B��B�=qB���B��B�  B���B�\)B�  B�ffB�
=B�B�ffB���B�\)B�  B��RB�\)B��B�z�B�
=B���B�=qB��HB���B�(�B���B�G�B�  B���B��B���B�=qB���B���B�(�B���B�33B��B���B�33B�B�Q�B��HB���B�=qB��HB�G�B��B\B�33B�B�=qB���BŅB�(�Bƣ�B�33B��Bȣ�B�33BɮB�=qB�
=BˮB�(�Ḅ�B�\)B�(�BΏ\B��B��
BЏ\B�33B�B�=qB���BӮB�Q�B���B�G�B�  BָRB�p�B�{Bأ�B�33B�Bڏ\B�G�B��
B�ffB���Bݙ�B�Q�B�
=Bߙ�B�(�B�RB�B�(�B��B��B��
B�\B�33B�B�Q�B��HB�B�Q�B���B�B�(�B��HB�\)B��B�\B�G�B�{B��B�33B��
B��\B�G�B�  B�RB�\)B��B�\B�G�B�  B���B�p�B��B���B�\)B�(�B���B�\)B��B���B�p�B�  B���B�33B��
C Q�C �C  CG�C�\C�
C=qC��C�
C(�CffC��C(�C�C��C{CffCC(�Cz�C�RC
=CffCC�CffC�C��C	Q�C	�C

=C
\)C
��C
�C33C��C��C=qCz�C��C�Cp�C�
C�CffC��C��C\)C�C��C33Cp�CC�Cp�CC  CG�C��C  CQ�C��C�
C33C��C�C(�Cz�C�
C33C�\C�HC�CffC�RC{Cp�CC
=CQ�C��C�C=qC��C�C(�Cp�CC{Cp�CC
=CG�C��C�HC=qC�\C�
C{CffC�RC {C ffC �C �C!33C!�C!�C"33C"�C"C#
=C#G�C#�C$  C$G�C$�C$��C%{C%p�C%��C&{C&\)C&��C&�
C'33C'�C'��C(  C(=qC(�\C(�C)33C)ffC)�C*
=C*Q�C*�\C*��C+
=C+ffC+�RC+��C,33C,p�C,�RC-{C-\)C-��C-��C.{C.p�C.C/
=C/G�C/�\C/��C0�C0p�C0�C0�C1G�C1��C1�HC2{C2\)C2�RC3
=C3Q�C3�C3�
C4(�C4�C4��C5{C5G�C5�\C5��C6�C6z�C6C7
=C7G�C7��C7��C8G�C8�\C8��C9{C9z�C9��C:�C:Q�C:��C;
=C;\)C;��C;�HC<(�C<z�C<�
C=�C=Q�C=�\C=�C>33C>p�C>��C>�HC?(�C?ffC?�C?�RC?�HC@�C@\)C@z�C@��C@CA  CA�CA=qCAQ�CAz�CA�CA�
CB
=CB(�CBG�CBffCB�CB�CB�HCC
=CC33CC\)CCp�CC�\CC��CD  CD33CD\)CDz�CD�\CD�RCD��CE(�CEG�CE\)CE�CECE��CF
=CF33CF\)CF�\CF��CF�CG
=CG=qCGz�CG�CG�
CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    ?k�?��H@B�\@�  @�  @�G�@�G�@�p�A  A   A,(�A?\)A^�RA\)A�  A��A�  A�Q�A�  A�  A�  A��B�B  B(�B (�B(Q�B0(�B7�
B@  BG�
BPQ�BX(�B`(�Bg�
Bo�
Bx(�B�  B�{B�  B�  B�{B�(�B�  B��B�  B�{B�{B�  B�  B�{B�  B�{B�(�B�  B�{B�  B�  B�(�B�(�B�{B�{B��B��B��
B�B��
B��B��B��C
=C
=C  C{C
{C
=C{C{C��C�C�C�C�C�C  C 
=C"  C$  C&�C(
=C*  C,  C-��C/��C2  C3��C5��C8  C:  C<  C>  C@
=CA��CC��CF  CH{CJ
=CL
=CN  CO��CR  CT  CV
=CX
=CZ
=C\  C^  C_��Cb  Cd  Ce��Cg��Cj  Cl  Cn
=Cp  Cr  Ct  Cv
=Cx{Cz
=C{�C}�C�C���C�  C�
=C�  C���C�  C�C���C���C�  C�  C�  C�  C���C���C�C�
=C���C�  C�C�C���C�  C�
=C�C���C���C�
=C�  C���C�  C���C���C�C���C���C�  C�C���C�C�  C�  C�  C�  C�C�  C���C�  C�  C�
=C�  C�  C���C�  C���C���C���C�C�C�  C���C�  C�
=C�  C���C���C���C�  C�  C�C���C���C���C���C�  C�C�  C�  C�C�C�  C���C���C�  C�  C�C�
=C�  C���C���C�  C�C�C�C�  C�  C�  C�
=C�
=C�  C���C�C�C�  C�  C���C���C�  C�C���C���C�  C���C���C�C�  C���C���C���C���C���C�  C�C�  C�  C�  C���D   D � D �qD}qD  D� D�qD}qD�D� D�D��D  D� D  D� D�qD� D	  D	}qD	��D
}qD�D��D  D� D  D��D�D� D  D�D  D� D  D}qD  D��DD� D�D� D�qD}qD  D}qD�qD� D�D� D  D� D�D}qD�D��D�D}qD�qD��D�D��D�D}qD�qD ��D!  D!� D"�D"}qD"�qD#� D$  D$}qD$�qD%� D&D&��D'  D'� D(  D(� D(�qD)� D*  D*��D*�qD+� D,�D,}qD-  D-��D.  D.}qD.�qD/� D0�D0� D1�D1� D2  D2}qD3�D3��D3�qD4��D5�D5� D6  D6��D7  D7� D8  D8}qD9  D9}qD9�qD:}qD:�qD;}qD<  D<� D=�D=��D>  D>� D>��D?z�D@  D@� DA  DA� DB�DB��DC  DCz�DC��DD}qDE  DE�DFDF��DG  DG� DH  DH}qDH��DI}qDJ�DJ��DJ�qDK}qDL  DL��DM  DM��DN�DN��DODO� DP  DP��DQ  DQ}qDR  DR��DS�DS}qDS�qDT� DU�DU�DV  DV� DW�DW� DX  DX��DYDY��DZ�DZ}qDZ�qD[� D[��D\� D]  D]� D]�qD^}qD_  D_� D_�qD`}qDa  Da�Da�qDbz�Db�qDc��Dc�qDdz�De�De��De�qDf� Dg  Dgz�Dg��Dh� Di  Diz�Di�qDj}qDk  Dk��Dk�qDl}qDm  Dm��DnDn��Do  Do�Dp�Dp� Dq  Dq� Dq�qDr��Ds�Ds��Dt�Dt��Du  Du�Dv�Dv�DwDw��Dx  Dx��Dy  Dy}qDy�qDz� D{  D{��D|�D|��D}D}�D~�D~}qD  D� D�qD�>�D�~�D�� D��D�@ D��HD�D��D�AHD�� D�� D�  D�@ D�~�D��qD���D�AHD���D�D���D�@ D��HD��HD�  D�>�D�� D���D���D�@ D��HD��HD�HD�B�D��HD���D�  D�@ D��HD�D�HD�@ D�� D�� D�  D�@ D��HD��HD�  D�@ D�~�D��qD���D�@ D��HD�D�  D�@ D��HD�� D��qD�>�D�� D�� D�  D�AHD��HD�� D�HD�@ D�� D�� D�  D�AHD�~�D�� D���D�>�D�� D�D��D�>�D�~�D�� D�  D�@ D�~�D��HD�  D�@ D��HD�� D���D�@ D��HD���D�  D�AHD��HD�� D���D�@ D���D�� D��qD�@ D��HD�� D�  D�AHD��HD���D���D�>�D�~�D�� D���D�=qD�� D��HD�  D�>�D�� D�� D�HD�B�D��HD���D�HD�AHD�� D�D��D�@ D�~�D��qD��qD�@ D�� D��HD�HD�>�D�� D��HD���D�>�D�� D�� D�  D�B�D���D��HD�  D�>�D�� D�� D�  D�@ D�}qD��qD�  D�@ D�� D���D���D�AHD��HD���D���D�@ D�� D�D�HD�@ D��HD��HD�  D�@ D�� D�� D�HD�B�D��HD���D��qD�@ D��HD��HD���D�>�D��HD��HD��D�@ D�~�D�� D�  D�@ D��HD���D��D�AHD�� D���D�  D�@ D�� D�� D�  D�@ D��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D��HD�  D�>�D�|)D���D�  D�AHD��HD�� D���D�>�D�� D��HD��D�AHD�~�D���D���D�@ D�� D�� D�  D�AHD��HD�� D�  D�@ D�~�D���D�  D�AHD��HD��HD�HD�@ D�� D��HD�  D�@ D�� D�� D�  D�>�D�� D��HD���D�>�D�~�D�� D�  D�@ DÁHD�� D���D�@ DĀ D��HD���D�=qDŀ D�� D�  D�@ D�~�D�� D�  D�=qD�~�D�� D�  D�@ DȀ D�� D�  D�AHDɁHD�� D�  D�@ DʁHD�D�HD�>�D�~�D��HD�HD�AHD̀ D�� D���D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�>�Dπ D�� D���D�@ DЀ Dо�D�  D�AHDсHDѾ�D���D�@ DҀ D�� D�  D�AHDӀ D�� D�HD�@ DԀ DԾ�D��qD�@ D�~�D�� D�  D�AHDր D־�D�  D�AHDׁHD�� D�HD�AHD؁HDؾ�D��qD�>�D�~�D�� D��D�@ Dڀ D��HD�HD�AHDۂ�D�D�HD�@ D�~�Dܾ�D���D�>�D�~�D�� D�  D�@ DށHD�� D�  D�>�D�~�D�� D�HD�AHD�� D�qD�  D�B�D�HD�� D�HD�B�D₏D�D��D�@ D�~�D㾸D��qD�>�D� D�� D�  D�@ D�~�D徸D���D�>�D�HD澸D���D�@ D�HD�D�HD�@ D� D辸D�  D�@ D� D龸D���D�=qD�~�D꾸D�  D�@ D�~�D뾸D���D�@ D�HD�� D��qD�@ D�HD��HD���D�@ D�HD��HD�HD�AHDD�D��D�AHD�~�D�D�HD�AHD�HD�� D�HD�AHD�HD��HD�HD�AHD�~�D�D�  D�@ D� D��HD�  D�@ D�� D���D�  D�AHD�� D�� D�HD�@ D�~�D�� D�HD�AHD��HD��HD�  D�>�D�� D���D�  D�AHD��HD�� G�O�?#�
?8Q�?k�?���?��?�@�@�R@333@B�\@Q�@fff@z�H@��
@��@�
=@�G�@��@���@��H@��@˅@�33@޸R@���@��@�
=A ��AffA
�HA�RA�A�A��A   A#�
A(��A.�RA1�A6ffA<(�AAG�AE�AHQ�AN�RAS�
AVffAZ�HA`��Adz�AhQ�Amp�Ar�\AvffAz=qA\)A��\A�z�A�{A���A�33A��A�
=A�G�A��
A��RA�Q�A�=qA���A�\)A�G�A�33A�p�A�Q�A��HA�z�A��RA�G�A��A�p�A��A��\A���A��RA�Q�A\A��AǮA���A��HA��AϮA��A�(�A�{A׮Aٙ�AۅA�{A�Q�A�=qA��
A�{A��A��HA�z�A�{A��A�A��A�
=A�G�A�(�A�ffB   B ��B=qB�Bz�BG�B�RB�
B��B	B
=BQ�BG�B{B\)B��B�B�RB�B��BffB\)BQ�Bp�B�HB  B��BB
=B Q�B!��B"ffB#\)B$��B%�B&�HB'�
B)G�B*ffB+33B,(�B-p�B.�RB/�B0z�B1B3
=B3�
B4��B6{B7\)B8Q�B9�B:=qB;\)B<��B=B>�\B?�B@��BB{BB�HBC�BDz�BE��BF�RBG�BH(�BIG�BJ�\BK�
BL��BN{BO33BP��BQ�BS
=BT  BU�BV�\BW�BX��BYB[
=B\��B]p�B^ffB_�Ba�Bb=qBc
=BdQ�Be��Bf�HBg�
Bh��Bj{Bk�Blz�Bmp�Bn�RBp(�Bq�Br{Bs33Bt��Bu�Bw
=Bw�
Bx��BzffB{�B|z�B}B33B�=qB���B�G�B�  B��RB�33B��B�Q�B�
=B��B�(�B��RB�\)B�(�B��RB��B��
B��\B��B���B�(�B��HB���B�{B��\B�G�B�  B��\B��B���B�Q�B�
=B��B�{B���B��B��B�z�B�
=B��
B�ffB���B�p�B�(�B���B�p�B�  B�z�B�
=B�B�ffB���B��B�  B���B�p�B�  B�z�B��B��
B�z�B�
=B��B�=qB���B��B�  B���B�\)B�  B�ffB�
=B�B�ffB���B�\)B�  B��RB�\)B��B�z�B�
=B���B�=qB��HB���B�(�B���B�G�B�  B���B��B���B�=qB���B���B�(�B���B�33B��B���B�33B�B�Q�B��HB���B�=qB��HB�G�B��B\B�33B�B�=qB���BŅB�(�Bƣ�B�33B��Bȣ�B�33BɮB�=qB�
=BˮB�(�Ḅ�B�\)B�(�BΏ\B��B��
BЏ\B�33B�B�=qB���BӮB�Q�B���B�G�B�  BָRB�p�B�{Bأ�B�33B�Bڏ\B�G�B��
B�ffB���Bݙ�B�Q�B�
=Bߙ�B�(�B�RB�B�(�B��B��B��
B�\B�33B�B�Q�B��HB�B�Q�B���B�B�(�B��HB�\)B��B�\B�G�B�{B��B�33B��
B��\B�G�B�  B�RB�\)B��B�\B�G�B�  B���B�p�B��B���B�\)B�(�B���B�\)B��B���B�p�B�  B���B�33B��
C Q�C �C  CG�C�\C�
C=qC��C�
C(�CffC��C(�C�C��C{CffCC(�Cz�C�RC
=CffCC�CffC�C��C	Q�C	�C

=C
\)C
��C
�C33C��C��C=qCz�C��C�Cp�C�
C�CffC��C��C\)C�C��C33Cp�CC�Cp�CC  CG�C��C  CQ�C��C�
C33C��C�C(�Cz�C�
C33C�\C�HC�CffC�RC{Cp�CC
=CQ�C��C�C=qC��C�C(�Cp�CC{Cp�CC
=CG�C��C�HC=qC�\C�
C{CffC�RC {C ffC �C �C!33C!�C!�C"33C"�C"C#
=C#G�C#�C$  C$G�C$�C$��C%{C%p�C%��C&{C&\)C&��C&�
C'33C'�C'��C(  C(=qC(�\C(�C)33C)ffC)�C*
=C*Q�C*�\C*��C+
=C+ffC+�RC+��C,33C,p�C,�RC-{C-\)C-��C-��C.{C.p�C.C/
=C/G�C/�\C/��C0�C0p�C0�C0�C1G�C1��C1�HC2{C2\)C2�RC3
=C3Q�C3�C3�
C4(�C4�C4��C5{C5G�C5�\C5��C6�C6z�C6C7
=C7G�C7��C7��C8G�C8�\C8��C9{C9z�C9��C:�C:Q�C:��C;
=C;\)C;��C;�HC<(�C<z�C<�
C=�C=Q�C=�\C=�C>33C>p�C>��C>�HC?(�C?ffC?�C?�RC?�HC@�C@\)C@z�C@��C@CA  CA�CA=qCAQ�CAz�CA�CA�
CB
=CB(�CBG�CBffCB�CB�CB�HCC
=CC33CC\)CCp�CC�\CC��CD  CD33CD\)CDz�CD�\CD�RCD��CE(�CEG�CE\)CE�CECE��CF
=CF33CF\)CF�\CF��CF�CG
=CG=qCGz�CG�CG�
CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@�XG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�E�A�=qA�;dA�9XA�9XA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�9XA�7LA�5?A�5?A�7LA�7LA�7LA�9XA�7LA�1'A�$�A��A�1A���AڬAڝ�Aڕ�AڍPA�r�A�`BA�\)A�O�A�A� �A���A֡�A֍PA�33A��yA�p�A���A�33A���AӰ!A�9XA�p�AѸRA���AЏ\A�1'A���AϏ\A�VAΕ�A�?}A�\)A̴9A�?}A���A�"�A�VA�  A� �A���Aư!AƶFAƅA��`AŴ9A��A�$�A�ZA¾wA�z�A���A�A�A�33A���A�I�A��A�ƨA���A�"�A��
A�ƨA�
=A�=qA���A�oA�+A��9A�z�A���A�|�A��A��A���A���A�33A�A��/A�E�A�
=A��-A�$�A��RA��9A���A��mA�ffA�ĜA��yA�l�A�VA��`A��A~I�A|�/A{XAw�At~�Aq�PAn-Ah��Ad�9Ac\)Aa�wA`1A]VAZ�9AX�AVz�ASoAQ�AQ�AQ33APVAO�wAN�jAM�hALjAI�PAG?}ABVA@I�A=p�A:��A9C�A6��A5��A4�jA3\)A1�A/��A/A.��A.�jA.�!A.�uA-\)A+7LA)K�A(��A($�A'S�A&��A&jA%��A%��A&{A(=qA(�\A(�A(A'dZA&�HA&I�A&  A%�-A%XA%VA$~�A#��A"�A!�A!VA �A��A
=A�PA
=A�A��Av�A�HAM�A��A�`A�-A{AȴA
�A	��A~�A��A$�A��A�HA;dA��AC�A �A v�A E�A 1'@�o@�C�@�J@� �@���@�l�@��+@�O�@��@���@��@��-@��@�x�@�z�@�33@�dZ@��H@�@���@�Z@�@�V@�G�@�`B@�p�@�V@�Z@���@�;d@��@���@�/@�r�@�  @�F@���@�$�@�X@�|�@�~�@�x�@�(�@���@��T@�j@߾w@ߕ�@�|�@�K�@�M�@�7L@ܬ@�  @��@�^5@��#@���@�1@�1@��
@�\)@�C�@�@���@��H@֟�@�E�@�p�@�G�@�/@�/@ԋD@�(�@�  @��@��
@�\)@��@ҟ�@��@��`@ϥ�@�\)@�@ͩ�@�X@̓u@�1@��;@ˮ@�  @�(�@��;@�|�@�|�@�\)@��@�M�@�@ɩ�@ɑh@�?}@ȼj@��@ǝ�@�C�@���@Ƈ+@�^5@�5?@���@ř�@�O�@�V@���@�I�@öF@�\)@�@¸R@\@�=q@�`B@���@��D@�Z@��@�|�@���@�-@�$�@�@�`B@�G�@��D@��;@���@���@���@�dZ@���@�^5@��@���@�%@�1'@���@���@�dZ@�C�@�
=@��R@�n�@�E�@�-@�J@�@�`B@�7L@��@���@�A�@�1'@���@���@�\)@��@���@��+@�^5@�M�@�5?@��T@�hs@�O�@���@�Q�@�1'@���@��w@���@�K�@��y@��!@�v�@�5?@��#@��@�V@���@��@��;@�|�@�S�@�;d@�\)@�
=@���@�=q@���@���@�1@��@���@�;d@�M�@��^@��@�X@�/@��@�Q�@� �@���@�S�@��@�
=@�@��@�E�@��T@�?}@�%@��@��j@�z�@� �@��;@�dZ@�33@�33@�+@�"�@��!@��@���@��@���@�?}@��@���@��u@�Z@���@�l�@�K�@�@�ȴ@�ff@��@�7L@�%@��/@���@��@�bN@��D@�z�@�I�@�1'@���@��@��@�V@�5?@�J@�@���@�p�@�/@���@���@�r�@��w@�S�@�S�@�o@���@���@��+@�E�@�-@��@��@�x�@��@��j@���@�I�@���@��;@��@���@���@��P@�\)@�o@�V@��#@�x�@�G�@��@��/@��D@�9X@��w@���@���@�dZ@�;d@�o@�@���@��H@��R@��!@���@�~�@�M�@�{@��#@��^@��-@���@��@�hs@���@��u@�r�@�r�@�bN@�Q�@�A�@�9X@� �@��@���@�t�@�
=@�~�@�n�@�5?@�-@�-@�$�@��T@���@���@�x�@�p�@�hs@�X@��@���@���@���@��u@��u@�1'@��m@���@�|�@�\)@�;d@�
=@���@�^5@�=q@�{@��T@�x�@�7L@��`@�b@~�y@~�+@~E�@~$�@}�-@|�@{S�@z��@z��@y��@yG�@xbN@x1'@x  @w��@w��@w��@wl�@v��@v@u��@u?}@uV@t(�@s��@r�@r�!@r^5@rM�@r-@q�@q��@p��@p �@o�w@o�P@ol�@ol�@n��@n{@m��@m�h@m�@mO�@mV@l�j@l�D@lZ@k��@kƨ@k��@kdZ@ko@j�H@j�@kS�@k33@j-@i��@ix�@iX@i�@h�`@h��@h�@h �@g|�@fȴ@fE�@e�T@d��@d�/@d�@d�j@dz�@d9X@c��@c"�@b�!@b^5@bM�@b�@a��@a7L@a%@`�9@`�@`Q�@`b@_�;@_�@^��@^��@^V@]�@]�@]?}@\j@[�m@[��@[S�@[33@Z�H@Z^5@Y�@Xb@W�P@W|�@W+@V�y@V��@V{@U��@U��@T�@TI�@S�F@S33@R�@RM�@Q�^@Q��@Q��@Q��@Q��@QG�@PĜ@P�9@P�9@PQ�@O��@N�@Nff@N$�@N{@M�-@MO�@L�/@L�D@Lj@L�@K�@KS�@KC�@K"�@J~�@I��@I��@H��@H�9@H�u@Hr�@H1'@H  @G�P@F�R@FV@F{@E�@E?}@D�@Dz�@D(�@C��@C�@CC�@C"�@Co@B�@B��@B=q@A��@A�^@@��@@ �@?�;@?|�@?K�@?;d@?+@>�@>ȴ@>��@>$�@=O�@<��@<�D@<Z@;�m@;��@:�H@9�@9�7@9hs@9�@8��@8Q�@8b@7�@7�@7+@6�@6�R@6ff@6V@5�@5p�@5/@4�@4��@4Z@41@3�@3"�@2�@2��@2�@1�@1��@1�^@1��@1x�@0��@0�u@0�@0Q�@0 �@/|�@.��@.�+@.v�@-@-p�@-?}@,z�@+�m@+t�@*��@*=q@)��@(��@(�u@(�@'�@'��@'|�@'K�@'+@&�y@&�R@&V@%��@%O�@$�/@$�@$�D@$j@$I�@$(�@$1@#�@#S�@#33@#@"�!@"=q@"J@!�@!��@!7L@!�@ ��@ r�@ A�@   @�@�w@��@|�@l�@�@ȴ@��@v�@ff@$�@�T@��@p�@?}@�@�j@�@(�@��@dZ@33@o@�@�H@��@�!@�\@n�@=q@��@�@�#@�^@x�@G�@%@Ĝ@Q�@b@�w@��@K�@+@
=@�@�@�@�@��@V@{@@�-@�-@`B@/@�@��@��@��@�D@z�@j@j@j@Z@�@��@dZ@C�@"�@o@�@�H@�H@��@�!@^5@M�@-@J@��@�@�#@��@x�@X@X@&�@�`@�9@bN@A�@  @��@�P@\)@\)@;d@
=@�@ȴ@��@v�@ff@5?@$�@@��@��@�h@?}@V@�@��@�@j@9X@(�@1@�
@��@t�@S�@o@
��@
��@
�\@
n�@
=q@	�@	��@	��@	��@	G�@	7L@	&�@	%@�`@Ĝ@�9A�M�A�K�A�I�A�M�A�E�A�A�A�C�A�G�A�A�A�=qA�9XA�=qA�;dA�7LA�9XA�9XA�9XA�9XA�7LA�;dA�=qA�7LA�7LA�9XA�;dA�9XA�5?A�7LA�;dA�;dA�7LA�5?A�9XA�;dA�7LA�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�7LA�;dA�9XA�5?A�7LA�;dA�9XA�5?A�9XA�=qA�9XA�5?A�9XA�;dA�9XA�7LA�7LA�;dA�;dA�1'A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�7LA�33A�33A�7LA�7LA�33A�33A�7LA�7LA�33A�33A�7LA�9XA�7LA�33A�5?A�7LA�9XA�5?A�5?A�5?A�9XA�9XA�;dA�9XA�7LA�33A�5?A�7LA�;dA�9XA�5?A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�5?A�7LA�7LA�9XA�5?A�33A�7LA�5?A�1'A�/A�1'A�33A�/A�/A�1'A�33A�-A�+A�(�A�&�A��A��A��A��A� �A��A��A��A��A��A�{A�oA�VA�
=A�A�  A���A���A��TA��A���A�ĜA�AڼjAڶFAڶFAڶFAڬAڣ�Aڧ�Aڥ�Aڟ�Aڟ�Aڡ�Aڣ�Aڡ�Aڛ�Aڗ�Aڕ�Aڙ�Aڗ�AړuAړuAڕ�Aڕ�Aڕ�Aڏ\Aڏ\AړuAړuAڏ\AڋDAډ7Aډ7Aډ7AڃA�z�A�v�A�p�A�jA�hsA�bNA�bNA�dZA�dZA�^5A�ZA�^5A�`BA�^5A�\)A�\)A�^5A�\)A�VA�XA�ZA�XA�Q�A�I�A�E�A�;dA�$�A�{A�%A�A���A��A��
A���Aٲ-A�`BA��A���Aغ^A؝�AؓuA�^5A�C�A��A�r�A�1AּjA֬A֣�A֝�A֝�A֟�A֟�A֛�A֕�A֑hA֓uA֍PAցA�x�A�dZA�XA�G�A��A�1A�A���A��A��A��A��yA��HA��A���Aմ9AՋDA�ffA�O�A�33A��A�JA���A��
A���AԮAԛ�A�|�A�I�A�7LA�-A�"�A��A�A��HA���A���A�A�ƨA�ĜA���AӸRAӺ^AӼjAӶFAө�AӓuA�x�A�O�A�K�A�?}A�-A��A��A�ƨAң�A�z�A�O�A�=qA�+A��A�  A��
AѾwAѲ-AёhA�Q�A�+A��A���AмjAа!Aа!Aв-Aд9AЩ�AЛ�AЇ+A�v�A�jA�ZA�C�A�/A�+A�$�A�$�A� �A��A��A��A�A��HA���A���AϺ^AϺ^Aϧ�Aχ+A�ffA�XA�G�A�=qA�+A�VA�A��A��A���A���AήA·+A�n�A�ffA�dZA�VA�K�A�G�A�A�A�7LA�{A��A���A͋DA�M�A�"�A��A��yA��HA���A�ȴA̧�A̅A�hsA�ZA�O�A�G�A�A�A�9XA��A��A��TA��#A���A�ȴA˶FA˙�A�p�A�O�A�%A��A��A���A�p�A�;dA�-A��A��#A�x�A�`BA�+A�
=A���A��/A�ĜAȰ!AȋDA�7LA�A��
AǴ9Aǡ�A�G�A���Aƴ9AƬAƮAư!AƲ-AƲ-AƮAƮAư!Aƴ9Aƴ9AƲ-Aƴ9Aƺ^AƸRAƶFAƲ-AƬAƛ�A�v�A�G�A�$�A�A���A��TA��
A���A���A�ƨA���A���AžwAũ�Aş�A�n�A�dZA�G�A��A��mAĶFAĕ�A�hsA�I�A�(�A�A���AþwAç�AÁA�jA�I�A��A��A��HA���A���A¶FA�A�A�A�A�AA�l�A�S�A�M�A�G�A�C�A�7LA���A���A�p�A�%A��^A�\)A�bA��`A���A���A�n�A�?}A�"�A�A�  A���A��A��TA��A���A��FA���A��A�bNA�M�A�=qA�1'A��A�A���A��A�ȴA���A���A���A���A�ƨA�ĜA�A�ĜA�ĜA�ĜA���A��RA��A���A���A��+A�r�A�\)A�=qA�VA���A��A��A��;A��#A��A��A���A�ȴA�ȴA�ƨA���A���A���A���A��jA��9A���A�=qA��A���A��A���A��+A�x�A�ffA�M�A�7LA��HA��/A�A�|�A�  A�A���A�\)A��;A�bNA��yA�v�A�O�A���A�`BA���A��A��hA�jA�;dA��A�  A���A�33A�JA��A���A���A�t�A�ZA�/A�{A��A���A�K�A�JA��A���A��\A�E�A��PA���A��;A��FA�1'A���A��;A���A�`BA��HA�ĜA���A�;dA���A�JA�"�A��TA��A�A��7A�{A���A���A�Q�A��mA�;dA��A�hsA�+A� �A�{A�A��A��/A�ƨA��^A��^A��!A��A��A���A�z�A�S�A�1'A��A�A���A��A��HA���A���A���A��FA�x�A�\)A�7LA�  A��
A���A��7A�bNA�9XA�+A�
=A�ȴA�ffA�9XA��A���A�;dA��A�A�A�M�A���A�dZA�9XA�(�A��A�  A���A���A�K�A�
=A��A��A���A��A�33A�C�A�l�A���A�l�A�1A���A��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    A�K�A�E�A�=qA�;dA�9XA�9XA�9XA�7LA�9XA�7LA�;dA�9XA�9XA�9XA�7LA�5?A�5?A�7LA�7LA�7LA�9XA�7LA�1'A�$�A��A�1A���AڬAڝ�Aڕ�AڍPA�r�A�`BA�\)A�O�A�A� �A���A֡�A֍PA�33A��yA�p�A���A�33A���AӰ!A�9XA�p�AѸRA���AЏ\A�1'A���AϏ\A�VAΕ�A�?}A�\)A̴9A�?}A���A�"�A�VA�  A� �A���Aư!AƶFAƅA��`AŴ9A��A�$�A�ZA¾wA�z�A���A�A�A�33A���A�I�A��A�ƨA���A�"�A��
A�ƨA�
=A�=qA���A�oA�+A��9A�z�A���A�|�A��A��A���A���A�33A�A��/A�E�A�
=A��-A�$�A��RA��9A���A��mA�ffA�ĜA��yA�l�A�VA��`A��A~I�A|�/A{XAw�At~�Aq�PAn-Ah��Ad�9Ac\)Aa�wA`1A]VAZ�9AX�AVz�ASoAQ�AQ�AQ33APVAO�wAN�jAM�hALjAI�PAG?}ABVA@I�A=p�A:��A9C�A6��A5��A4�jA3\)A1�A/��A/A.��A.�jA.�!A.�uA-\)A+7LA)K�A(��A($�A'S�A&��A&jA%��A%��A&{A(=qA(�\A(�A(A'dZA&�HA&I�A&  A%�-A%XA%VA$~�A#��A"�A!�A!VA �A��A
=A�PA
=A�A��Av�A�HAM�A��A�`A�-A{AȴA
�A	��A~�A��A$�A��A�HA;dA��AC�A �A v�A E�A 1'@�o@�C�@�J@� �@���@�l�@��+@�O�@��@���@��@��-@��@�x�@�z�@�33@�dZ@��H@�@���@�Z@�@�V@�G�@�`B@�p�@�V@�Z@���@�;d@��@���@�/@�r�@�  @�F@���@�$�@�X@�|�@�~�@�x�@�(�@���@��T@�j@߾w@ߕ�@�|�@�K�@�M�@�7L@ܬ@�  @��@�^5@��#@���@�1@�1@��
@�\)@�C�@�@���@��H@֟�@�E�@�p�@�G�@�/@�/@ԋD@�(�@�  @��@��
@�\)@��@ҟ�@��@��`@ϥ�@�\)@�@ͩ�@�X@̓u@�1@��;@ˮ@�  @�(�@��;@�|�@�|�@�\)@��@�M�@�@ɩ�@ɑh@�?}@ȼj@��@ǝ�@�C�@���@Ƈ+@�^5@�5?@���@ř�@�O�@�V@���@�I�@öF@�\)@�@¸R@\@�=q@�`B@���@��D@�Z@��@�|�@���@�-@�$�@�@�`B@�G�@��D@��;@���@���@���@�dZ@���@�^5@��@���@�%@�1'@���@���@�dZ@�C�@�
=@��R@�n�@�E�@�-@�J@�@�`B@�7L@��@���@�A�@�1'@���@���@�\)@��@���@��+@�^5@�M�@�5?@��T@�hs@�O�@���@�Q�@�1'@���@��w@���@�K�@��y@��!@�v�@�5?@��#@��@�V@���@��@��;@�|�@�S�@�;d@�\)@�
=@���@�=q@���@���@�1@��@���@�;d@�M�@��^@��@�X@�/@��@�Q�@� �@���@�S�@��@�
=@�@��@�E�@��T@�?}@�%@��@��j@�z�@� �@��;@�dZ@�33@�33@�+@�"�@��!@��@���@��@���@�?}@��@���@��u@�Z@���@�l�@�K�@�@�ȴ@�ff@��@�7L@�%@��/@���@��@�bN@��D@�z�@�I�@�1'@���@��@��@�V@�5?@�J@�@���@�p�@�/@���@���@�r�@��w@�S�@�S�@�o@���@���@��+@�E�@�-@��@��@�x�@��@��j@���@�I�@���@��;@��@���@���@��P@�\)@�o@�V@��#@�x�@�G�@��@��/@��D@�9X@��w@���@���@�dZ@�;d@�o@�@���@��H@��R@��!@���@�~�@�M�@�{@��#@��^@��-@���@��@�hs@���@��u@�r�@�r�@�bN@�Q�@�A�@�9X@� �@��@���@�t�@�
=@�~�@�n�@�5?@�-@�-@�$�@��T@���@���@�x�@�p�@�hs@�X@��@���@���@���@��u@��u@�1'@��m@���@�|�@�\)@�;d@�
=@���@�^5@�=q@�{@��T@�x�@�7L@��`@�b@~�y@~�+@~E�@~$�@}�-@|�@{S�@z��@z��@y��@yG�@xbN@x1'@x  @w��@w��@w��@wl�@v��@v@u��@u?}@uV@t(�@s��@r�@r�!@r^5@rM�@r-@q�@q��@p��@p �@o�w@o�P@ol�@ol�@n��@n{@m��@m�h@m�@mO�@mV@l�j@l�D@lZ@k��@kƨ@k��@kdZ@ko@j�H@j�@kS�@k33@j-@i��@ix�@iX@i�@h�`@h��@h�@h �@g|�@fȴ@fE�@e�T@d��@d�/@d�@d�j@dz�@d9X@c��@c"�@b�!@b^5@bM�@b�@a��@a7L@a%@`�9@`�@`Q�@`b@_�;@_�@^��@^��@^V@]�@]�@]?}@\j@[�m@[��@[S�@[33@Z�H@Z^5@Y�@Xb@W�P@W|�@W+@V�y@V��@V{@U��@U��@T�@TI�@S�F@S33@R�@RM�@Q�^@Q��@Q��@Q��@Q��@QG�@PĜ@P�9@P�9@PQ�@O��@N�@Nff@N$�@N{@M�-@MO�@L�/@L�D@Lj@L�@K�@KS�@KC�@K"�@J~�@I��@I��@H��@H�9@H�u@Hr�@H1'@H  @G�P@F�R@FV@F{@E�@E?}@D�@Dz�@D(�@C��@C�@CC�@C"�@Co@B�@B��@B=q@A��@A�^@@��@@ �@?�;@?|�@?K�@?;d@?+@>�@>ȴ@>��@>$�@=O�@<��@<�D@<Z@;�m@;��@:�H@9�@9�7@9hs@9�@8��@8Q�@8b@7�@7�@7+@6�@6�R@6ff@6V@5�@5p�@5/@4�@4��@4Z@41@3�@3"�@2�@2��@2�@1�@1��@1�^@1��@1x�@0��@0�u@0�@0Q�@0 �@/|�@.��@.�+@.v�@-@-p�@-?}@,z�@+�m@+t�@*��@*=q@)��@(��@(�u@(�@'�@'��@'|�@'K�@'+@&�y@&�R@&V@%��@%O�@$�/@$�@$�D@$j@$I�@$(�@$1@#�@#S�@#33@#@"�!@"=q@"J@!�@!��@!7L@!�@ ��@ r�@ A�@   @�@�w@��@|�@l�@�@ȴ@��@v�@ff@$�@�T@��@p�@?}@�@�j@�@(�@��@dZ@33@o@�@�H@��@�!@�\@n�@=q@��@�@�#@�^@x�@G�@%@Ĝ@Q�@b@�w@��@K�@+@
=@�@�@�@�@��@V@{@@�-@�-@`B@/@�@��@��@��@�D@z�@j@j@j@Z@�@��@dZ@C�@"�@o@�@�H@�H@��@�!@^5@M�@-@J@��@�@�#@��@x�@X@X@&�@�`@�9@bN@A�@  @��@�P@\)@\)@;d@
=@�@ȴ@��@v�@ff@5?@$�@@��@��@�h@?}@V@�@��@�@j@9X@(�@1@�
@��@t�@S�@o@
��@
��@
�\@
n�@
=q@	�@	��@	��@	��@	G�@	7L@	&�@	%@�`@ĜG�O�A�M�A�K�A�I�A�M�A�E�A�A�A�C�A�G�A�A�A�=qA�9XA�=qA�;dA�7LA�9XA�9XA�9XA�9XA�7LA�;dA�=qA�7LA�7LA�9XA�;dA�9XA�5?A�7LA�;dA�;dA�7LA�5?A�9XA�;dA�7LA�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�7LA�;dA�9XA�5?A�7LA�;dA�9XA�5?A�9XA�=qA�9XA�5?A�9XA�;dA�9XA�7LA�7LA�;dA�;dA�1'A�1'A�5?A�7LA�5?A�1'A�5?A�7LA�7LA�33A�33A�7LA�7LA�33A�33A�7LA�7LA�33A�33A�7LA�9XA�7LA�33A�5?A�7LA�9XA�5?A�5?A�5?A�9XA�9XA�;dA�9XA�7LA�33A�5?A�7LA�;dA�9XA�5?A�5?A�9XA�;dA�7LA�5?A�9XA�;dA�9XA�5?A�7LA�7LA�9XA�5?A�33A�7LA�5?A�1'A�/A�1'A�33A�/A�/A�1'A�33A�-A�+A�(�A�&�A��A��A��A��A� �A��A��A��A��A��A�{A�oA�VA�
=A�A�  A���A���A��TA��A���A�ĜA�AڼjAڶFAڶFAڶFAڬAڣ�Aڧ�Aڥ�Aڟ�Aڟ�Aڡ�Aڣ�Aڡ�Aڛ�Aڗ�Aڕ�Aڙ�Aڗ�AړuAړuAڕ�Aڕ�Aڕ�Aڏ\Aڏ\AړuAړuAڏ\AڋDAډ7Aډ7Aډ7AڃA�z�A�v�A�p�A�jA�hsA�bNA�bNA�dZA�dZA�^5A�ZA�^5A�`BA�^5A�\)A�\)A�^5A�\)A�VA�XA�ZA�XA�Q�A�I�A�E�A�;dA�$�A�{A�%A�A���A��A��
A���Aٲ-A�`BA��A���Aغ^A؝�AؓuA�^5A�C�A��A�r�A�1AּjA֬A֣�A֝�A֝�A֟�A֟�A֛�A֕�A֑hA֓uA֍PAցA�x�A�dZA�XA�G�A��A�1A�A���A��A��A��A��yA��HA��A���Aմ9AՋDA�ffA�O�A�33A��A�JA���A��
A���AԮAԛ�A�|�A�I�A�7LA�-A�"�A��A�A��HA���A���A�A�ƨA�ĜA���AӸRAӺ^AӼjAӶFAө�AӓuA�x�A�O�A�K�A�?}A�-A��A��A�ƨAң�A�z�A�O�A�=qA�+A��A�  A��
AѾwAѲ-AёhA�Q�A�+A��A���AмjAа!Aа!Aв-Aд9AЩ�AЛ�AЇ+A�v�A�jA�ZA�C�A�/A�+A�$�A�$�A� �A��A��A��A�A��HA���A���AϺ^AϺ^Aϧ�Aχ+A�ffA�XA�G�A�=qA�+A�VA�A��A��A���A���AήA·+A�n�A�ffA�dZA�VA�K�A�G�A�A�A�7LA�{A��A���A͋DA�M�A�"�A��A��yA��HA���A�ȴA̧�A̅A�hsA�ZA�O�A�G�A�A�A�9XA��A��A��TA��#A���A�ȴA˶FA˙�A�p�A�O�A�%A��A��A���A�p�A�;dA�-A��A��#A�x�A�`BA�+A�
=A���A��/A�ĜAȰ!AȋDA�7LA�A��
AǴ9Aǡ�A�G�A���Aƴ9AƬAƮAư!AƲ-AƲ-AƮAƮAư!Aƴ9Aƴ9AƲ-Aƴ9Aƺ^AƸRAƶFAƲ-AƬAƛ�A�v�A�G�A�$�A�A���A��TA��
A���A���A�ƨA���A���AžwAũ�Aş�A�n�A�dZA�G�A��A��mAĶFAĕ�A�hsA�I�A�(�A�A���AþwAç�AÁA�jA�I�A��A��A��HA���A���A¶FA�A�A�A�A�AA�l�A�S�A�M�A�G�A�C�A�7LA���A���A�p�A�%A��^A�\)A�bA��`A���A���A�n�A�?}A�"�A�A�  A���A��A��TA��A���A��FA���A��A�bNA�M�A�=qA�1'A��A�A���A��A�ȴA���A���A���A���A�ƨA�ĜA�A�ĜA�ĜA�ĜA���A��RA��A���A���A��+A�r�A�\)A�=qA�VA���A��A��A��;A��#A��A��A���A�ȴA�ȴA�ƨA���A���A���A���A��jA��9A���A�=qA��A���A��A���A��+A�x�A�ffA�M�A�7LA��HA��/A�A�|�A�  A�A���A�\)A��;A�bNA��yA�v�A�O�A���A�`BA���A��A��hA�jA�;dA��A�  A���A�33A�JA��A���A���A�t�A�ZA�/A�{A��A���A�K�A�JA��A���A��\A�E�A��PA���A��;A��FA�1'A���A��;A���A�`BA��HA�ĜA���A�;dA���A�JA�"�A��TA��A�A��7A�{A���A���A�Q�A��mA�;dA��A�hsA�+A� �A�{A�A��A��/A�ƨA��^A��^A��!A��A��A���A�z�A�S�A�1'A��A�A���A��A��HA���A���A���A��FA�x�A�\)A�7LA�  A��
A���A��7A�bNA�9XA�+A�
=A�ȴA�ffA�9XA��A���A�;dA��A�A�A�M�A���A�dZA�9XA�(�A��A�  A���A���A�K�A�
=A��A��A���A��A�33A�C�A�l�A���A�l�A�1A���A��+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�BB
�B
��B
�pB
�;B
�;B
�;B
ߤB
�pB
�pB
��B
�pB
�pB
�;B
�;B
�pB
�pB
�B
�;B
ߤB
�;B
�;B
�pB
�BB
�vB
�B
�B
�QB
�)B
��B
��B
��B
��B
�cB
�cB
�QB
��B
�WB
ɺB
�#B
ΥB
�B
ɺB
�jB
�9B
��B
�B
��B
��B
�	B
��B
��B
�B
�$B
�MB
�B
��B
��B
�B
�B
��B
�uB
�YB
��B
ΥB
��B�B	7BfB.B�B+B"4B!�B$tB*�B+�B1�BMjBVBZ�Bd�Bn�BsBw�B��B��B�xB�B��B��B��B��B�B�>B�%B��BB��B iB�ZB�B�sB�B�1Bu�B`BT�BMB<B�B
��B
�B
˒B
�}B
�\B
w�B
q�B
c B
HKB
P}B
T�B
H�B
7B
'�B
�B

rB	�;B	�B	یB	��B	��B	��B	�*B	�'B	��B	��B	��B	��B	�B	z�B	w�B	p�B	j�B	_pB	U�B	CaB	5B	.�B	!B	qB	B	"B	
�B	%B	�B	;B�]B��B�]B��B�B��B��B��B��B	 4B	�B	 B	+�B	B[B	H�B	XEB	�xB	�wB	�zB	уB	� B	�sB	�B	�|B	�ZB	�B	�B	��B	�JB	��B	�AB	��B	�>B	�pB	˒B	��B	��B	��B	�;B	}VB	qvB	Z�B	R�B	jB	jB	[�B	Q�B	H�B	:�B	1�B	*�B	1�B	7�B	:*B	6�B	(XB	*�B	1�B	6�B	7�B	9�B	;0B	=qB	A�B	B�B	E9B	D�B	EB	C-B	C-B	D�B	F�B	LdB	VB	Z�B	[�B	[#B	_pB	a�B	a|B	_�B	d&B	pB	v�B	{B	}�B	~�B	��B	��B	�B	�=B	�7B	�VB	�:B	��B	��B	��B	��B	�:B	��B	�$B	��B	��B	��B	�B	�IB	�B	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�<B	��B	ȴB	��B	��B	�B	ӏB	��B	ܒB	ݘB	ޞB	�HB	�B	�B	�B	�QB	�cB	��B	�GB	�B	�B	�vB	�TB	�B	��B	��B	�TB	�B	��B	�%B	�ZB	�%B	��B	��B	��B	��B
 �B
MB
�B
�B
�B
+B
�B
	�B

	B
	�B
	lB

=B

�B
B
�B
DB
~B
JB
JB
~B
B
PB
B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
bB
�B
�B
bB
�B
:B
�B
.B
 B
�B
�B
@B
oB
{B
MB
B
�B
�B
�B
�B
+B
+B
$B
SB
�B
�B
�B
YB
�B
1B
�B
�B
kB
�B
�B
qB
CB
B
�B
CB
�B
�B
�B
VB
!B
 'B
 �B
!-B
 �B
 �B
 �B
#B
!�B
!�B
!-B
"�B
#:B
#�B
$@B
$�B
$�B
%B
%zB
%�B
'B
)�B
(�B
)�B
)�B
(�B
(XB
($B
*eB
*eB
+�B
+�B
,=B
,�B
*�B
-CB
.B
/B
.}B
.}B
/B
.�B
.�B
0!B
/�B
0UB
1'B
1�B
1�B
1�B
1�B
1�B
2�B
1�B
2aB
2�B
49B
4�B
5?B
6B
6�B
7�B
7�B
8B
7�B
7�B
8�B
9XB
9$B
8�B
9XB
9�B
9�B
:*B
:*B
:�B
<�B
<�B
<�B
=B
<�B
=B
=�B
=�B
=B
=<B
=�B
=�B
=�B
@OB
A�B
A�B
A�B
CaB
C�B
FtB
FtB
FB
F�B
GzB
GB
G�B
G�B
G�B
GzB
HKB
IB
H�B
J#B
J#B
I�B
J�B
K�B
K�B
K^B
K)B
K^B
LdB
L0B
L�B
L�B
M�B
M6B
M6B
MjB
M6B
MB
L�B
L�B
L�B
NpB
NpB
NpB
NpB
N�B
N�B
OB
N�B
O�B
OBB
OBB
PB
P�B
QB
QNB
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
U�B
U�B
VB
VmB
V�B
W
B
W
B
W
B
W?B
W
B
XB
YKB
X�B
ZB
[�B
[WB
[�B
[�B
\�B
]/B
]�B
^�B
_B
^�B
^�B
^�B
^�B
_pB
_pB
_pB
_�B
`B
`vB
`B
_�B
_�B
`B
_�B
_�B
_�B
_B
^�B
^�B
^�B
]�B
]�B
_B
_B
_;B
^�B
^�B
^jB
^5B
^�B
]�B
]/B
^B
^5B
^5B
^5B
^jB
^�B
^�B
^�B
^jB
_�B
`B
`BB
`vB
`�B
a�B
a�B
b�B
b�B
b�B
b�B
b�B
b�B
c�B
c�B
d�B
e,B
e,B
d�B
d�B
e�B
ffB
ffB
ffB
ffB
ffB
f�B
f�B
f�B
g8B
g�B
h
B
hsB
h�B
jB
j�B
j�B
l�B
n/B
m�B
m)B
m]B
m)B
m�B
m�B
m�B
m�B
m�B
n�B
oiB
o5B
poB
qvB
qvB
rGB
r�B
sMB
s�B
t�B
uZB
u�B
u�B
u�B
u�B
v+B
u�B
v�B
wfB
w�B
w�B
w2B
w�B
v�B
v�B
w�B
xlB
y	B
y	B
y�B
zB
zB
zxB
zxB
zxB
z�B
{B
}"B
|�B
|�B
|B
|PB
|�B
}�B
}�B
}�B
}�B
~]B
}�B
}�B
~(B
}�B
~(B
~�B
~�B
�B
�4B
�B
�iB
�4B
�4B
� B
�4B
��B
� B
� B
�B
�B
�4B
��B
��B
�iB
�B
��B
�;B
��B
�B
��B
�AB
�uB
��B
��B
��B
�B
��B
�MB
�B
��B
�SB
��B
��B
��B
�+B
��B
��B
�fB
��B
�B
�7B
�lB
�lB
�lB
�=B
��B
��B
��B
��B
�JB
�~B
�~B
�JB
�JB
�JB
�~B
�~B
�~B
��B
��B
��B
��B
��B
�"B
�VB
�\B
��B
��B
��B
� B
�hB
��B
�hB
�hB
�hB
��B
��B
��B
�:B
�oB
��B
��B
�B
�B
�B
��B
��B
��B
�@B
�@B
��B
�FB
�FB
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
�YB
��B
��B
�SB
�SB
��B
�YB
��B
��B
�1B
�1B
�1B
�B
�7B
�7B
�kB
�7B
��B
��B
�	B
��B
�B
�xB
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�VB
��B
��B
�'B
�\B
�\B
��B
��B
�-B
��B
�bB
�bB
��B
�4B
�4B
�4B
�4B
��B
��B
�:B
�:B
�nB
��B
��B
�nB
�tB
�B
�B
�FB
�zB
�zB
�zB
��B
��B
��B
��B
�LB
��B
��B
��B
��B
��B
�B
�B
��B
��B
�$B
��B
��B
��B
�*B
�_B
�*B
�*B
�*B
��B
��B
��B
�eB
�eB
�eB
�eB
��B
��B
��B
��B
�6B
�kB
�kB
��B
��B
��B
��B
��B
�=B
�B
�CB
�wB
��B
��B
��B
�B
�B
��B
�B
�}B
�}B
��B
�B
��B
�B
�B
��B
��B
��B
��B
�!B
�UB
�UB
�'B
�'B
��B
��B
��B
�-B
�-B
�aB
��B
��B
��B
��B
�3B
�hB
�hB
�hB
��B
�B
�9B
�9B
��B
��B
��B
�?B
�?B
�tB
��B
��B
��B
�B
�FB
�zB
��B
�B
�LB
�LB
��B
��B
��B
�B
�B
�B
�RB
��B
��B
��B
��B
�$B
�XB
�XB
�pB
�vB
��B
�B
�pB
ޞB
�HB
�B
ߤB
�B
�BB
ޞB
�jB
ߤB
�BB
��B
ޞB
�pB
�BB
��B
��B
�B
�vB
ߤB
�jB
�;B
�vB
�B
�jB
�5B
��B
�vB
ޞB
�jB
�B
�vB
�;B
�5B
ߤB
�vB
ޞB
�5B
�pB
�BB
ޞB
��B
�BB
�B
�jB
��B
��B
�;B
�5B
�;B
��B
��B
��B
ߤB
�vB
ߤB
�/B
�;B
�B
�B
�B
�jB
ߤB
�vB
�pB
�jB
��B
��B
�B
�jB
��B
�vB
�vB
�jB
�B
�BB
�B
��B
�5B
��B
�vB
��B
ޞB
�5B
��B
�B
�B
ޞB
�jB
�5B
ޞB
�pB
�B
�vB
�pB
�5B
ޞB
�B
�B
�jB
�5B
ߤB
�vB
�B
�jB
�B
�BB
��B
ޞB
�B
ߤB
�vB
��B
�B
��B
�vB
�;B
ޞB
ߤB
�vB
�pB
�jB
�BB
�B
�BB
�;B
��B
�HB
�B
�HB
�;B
�B
�|B
�HB
�|B
�B
�|B
��B
�B
��B
�B
��B
��B
��B
�&B
�2B
�8B
�
B
�B
�sB
�KB
�B
�B
�B
�]B
�"B
�B
�B
�)B
��B
�B
�"B
��B
�B
�/B
�B
�)B
�B
��B
�B
��B
�B
� B
�5B
��B
�)B
��B
� B
��B
�B
�WB
�"B
�]B
�]B
�/B
�B
�B
�cB
�/B
�WB
��B
� B
� B
�]B
��B
��B
��B
��B
�)B
��B
��B
�B
�B
��B
�/B
�B
�/B
�/B
�;B
�)B
��B
�
B
��B
��B
��B
�B
��B
��B
��B
�BB
�pB
�/B
ںB
��B
��B
�B
�;B
��B
˒B
�6B
��B
ʌB
�#B
��B
ȀB
��B
��B
��B
�B
��B
�^B
ɺB
�<B
ʌB
��B
��B
�HB
͟B
�<B
��B
��B
�dB
�B
�B
�B
�6B
�B
�B
�B
ƨB
��B
��B
��B
�B
�6B
�XB
��B
��B
��B
�B
�UB
�aB
�aB
�!B
��B
��B
��B
��B
�IB
�kB
�kB
��B
��B
��B
�eB
��B
�=B
��B
�IB
�XB
��B
��B
�LB
�B
��B
��B
�nB
��B
��B
�IB
�~B
�IB
�IB
��B
�YB
�YB
�B
��B
��B
��B
��B
��B
��B
��B
�SB
�+B
�xB
�B
��B
�OB
�CB
��B
�!B
��B
�eB
��B
�SB
��B
��B
�$B
�B
��B
��B
��B
��B
�@B
��B
��B
�YB
��B
�B
�B
��B
��B
�YB
��B
�B
��B
�{B
�B
��B
�qB
��B
�:B
� B
�{B
�hB
�4B
�\B
��B
�FB
��B
�:B
��B
�@B
�B
��B
�B
�B
��B
�4B
��B
��B
��B
��B
��B
�B
�:B
��B
��B
�MB
�hB
�:B
��B
��B
�@B
��B
��B
��B
�'B
�MB
�_B
��B
�tB
�\B
�!B
��B
��B
�'B
�FB
ĜB
��B
҉B
خB
�)B
�;B
�ZB
��B
�|B
�lB
�B
�ZB  B�B:B	�B	B
	BfB�B
	B	lB�B_B1B	B�BfB+B	B
	BBbBBeB�B�B�B7B�B�B�B�BYBSB�B	B�B�B�B"�B"hB&LB$�B"4B$tB 'B!�B"4B"4BVB�B �B!�B#�B)�B,�B,qB)�B'�B+6B+kB*�B*0B)_B*�B)�B.IB-wB,=B)�B(�B)_B5tB5tBC�BF�BH�BVmBN�BMjBOvBRTBW�BV9BW�BV�BV9BV9BYKBZBY�BY�B\�B^5B`�Bc�Bd�Be`Be�BgBiDBi�BpoBp�Bp�Bp;Bp�Br|Bs�BtTBtBr�Br|BrBsMBo�ByrByrBzDB|�B�;B�B�B��B�7B�rB�JB�B�~B��B��B�~B��B�B�B�DB��B��B��B�xB��B��B��B��B��B��B�zB�B�B�B��B��B��B��B��B��B��B��B�wB��B�mBŢB�B�mB�B�?B�#B�B�KBҽBרB�#B�B�#B�B�B��BߤB�B��B�`B�BB�mB�B�ZB�KB�iB�yB��B��B�B��BBuB�B�lB�cB��B�B�B�B��B��B�>B��B�B�BDB�B�B�ZB�(B 4B�(B�fB�B�B
rB�B��B��B�+B��B��B�>B��B��B�B�B�ZB�vB�B�vB�+B��B��B�|B�B�B�B�cB��B��B�B�B�fB�)B�KB�QB��B�B��B�TB��B�QB�B�B�BخBޞBԕBϫB�zB��BߤB��B��B��B��B��B�-B��B��B�B��B�:B��B�=B�xB��B�7B�hB�GB~�By	Br�BjG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021091523202920210915232029IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092600004720210926000047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092600004720210926000047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365320220126093653IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295520220204232955IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                