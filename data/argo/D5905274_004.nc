CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-02-22T11:25:09Z creation; 2023-04-26T19:24:26Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 |  Z�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  bt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 |  �d   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 |  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �<   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 |  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 | ,�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 | Q�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Yp   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` w`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   w�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   }�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �,   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �4   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180222112509  20230426192426  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7315_008643_004                 7315_008643_004                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�NV�ܜN@�NV�ܜN11  @�NWC,�@�NWC,�@0�Y5�;@0�Y5�;�d��IQ��d��IQ�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@}p�@�  @�  @�G�A ��A��A!G�A,��AAG�A`  A�  A�  A��A��A�  A�Q�A߮A�B   BQ�Bz�BQ�B (�B((�B0Q�B8Q�B@  BG�BO�BX  B`(�Bg�Bp  BxQ�B�  B��B��B�  B�{B�  B�  B��B��B�  B�{B�  B��B��B�(�B�{B��B�  B�  B�{B��B�  B�(�B�  B��
B��
B��B�{B�{B��B��
B�B��
C�HC��C{C  C	��C��C
=C
=C{C�C  C  C��C��C
=C 
=C"
=C$  C%��C'��C*  C,
=C.  C0
=C2  C3��C5��C8  C9��C<  C>  C?�CB  CD
=CE��CG�CJ  CL{CN
=CO�CQ�CS��CV  CX  CZ  C[��C]�C_�Cb  Cd
=Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu��Cw��Cz  C|
=C~  C�C�
=C�
=C�  C���C�  C�C���C���C���C��C��C���C���C���C�C���C���C�C�  C�C�C�C�  C���C���C���C�
=C�C�C�
=C�C�  C�C�\C�
=C���C���C�C���C���C�C�  C���C�  C�C�
=C�
=C�
=C�C�C�
=C�
=C�
=C���C���C�C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�C���C���C�  C�C�  C�C�  C���C���C���C���C���C�C���C��C�C�  C�C�\C�C�
=C�  C���C���C�  C�C�  C�  C�C�
=C���C�C���C���C���C�  C�  C�  C�
=C�C�C�  C���C�  C�C�C�C�C�C�C�C�C�  C�C�
=C�C���D   D }qD �qD��D  D}qD�qD}qD�D��D  D� D��D��DD� D�D� D�qD	}qD
  D
}qD
�qDz�DD��D  D� D��D� DD}qD  D}qD  D� D  D� D�D� D  D� D�D� D  D� D�D� D  D��D�qD��D�qD� D  DxRD��D�D  D� D�qD� D�D}qD�qD � D!�D!��D"  D"� D#�D#}qD$�D$� D%  D%��D%��D&� D'�D'}qD(�D(}qD(�qD)� D*  D*� D+  D+� D+�qD,�D-  D-� D.�D.��D/�D/}qD/��D0� D0�qD1}qD2D2�D3  D3z�D3��D4��D5�D5��D6D6� D7  D7��D8  D8z�D8�qD9� D:�D:� D:��D;� D<  D<��D<�qD=z�D=�qD>� D>�qD?z�D@  D@}qDA�DA}qDB  DB��DCDC}qDC�RDD}qDD�qDE��DFDF� DF�qDG�DH�DH� DIDI��DI�qDJ}qDJ�qDK}qDK�qDL� DMDM�DN�DN��DO�DO}qDO��DP}qDP��DQz�DQ�qDRz�DR��DS}qDTDT� DU  DU� DU�qDV��DWDW�DW�qDX� DY�DY��DZ  DZ� DZ�qD[z�D[�qD\}qD]�D]}qD^D^��D_�D_�D`D`��D`�qDa� Da�qDb� Dc�Dc��Dd�Dd� Dd��De��Df  Df� Dg�Dg�Dh  Dh}qDi  Di� DjDj��Dj��Dkz�Dl  Dl��DmDm��Dn  Dn�Do�Do}qDo��Dp��Dq�Dq� Dr  Dr��Ds�Ds� Dt�Dt� Dt��Du}qDv  Dv� Dw  Dw� Dx  Dx}qDx�qDy}qDz  Dz� D{�D{�D|  D|��D}  D}��D~�D~z�D~��D}qD�HD�@ D�~�D�� D�HD�AHD��HD�� D�  D�B�D�~�D���D�  D�@ D�� D�� D���D�AHD�� D�� D���D�>�D��HD�� D�  D�AHD���D���D�HD�=qD��HD��HD���D�AHD��HD�� D��qD�>�D��HD��HD���D�=qD�~�D��HD�HD�@ D�~�D�� D�  D�=qD�~�D��HD�HD�@ D��HD��HD�  D�>�D�}qD���D���D�=qD�~�D�� D��qD�<)D�}qD��HD��D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�~�D��qD���D�AHD��HD���D���D�@ D�� D���D���D�>�D��HD�� D�  D�AHD�� D�� D�HD�>�D��HD��HD���D�>�D�~�D��)D��qD�@ D��HD�D�HD�AHD���D�D�  D�=qD�~�D��HD�HD�>�D�~�D���D���D�AHD��HD���D���D�=qD�� D��HD�HD�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�D��D�AHD���D�D��D�AHD�� D��qD���D�AHD��HD�� D�  D�@ D�~�D���D�HD�AHD�� D��qD�  D�AHD��HD���D�HD�AHD�� D���D���D�@ D�� D�� D�HD�B�D�� D���D���D�>�D�� D�� D�  D�AHD���D�� D�  D�B�D��HD��HD�HD�@ D��HD��HD�HD�@ D�}qD���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D�D���D�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D��HD��D�@ D�|)D�� D���D�>�D�� D���D�  D�@ D�}qD��qD��qD�>�D�� D���D�HD�=qD�� D��HD��fG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?8Q�?u?��
?Ǯ?�ff@�\@��@(��@:�H@L��@^�R@p��@�  @��@�z�@�p�@�ff@�\)@�Q�@��R@Ǯ@��@��H@��@�\)@�Q�A ��AA
�HAG�AffA�A ��A%A*=qA/\)A3�
A8Q�A<��A@  AE�AI��AN�RAR�\AXQ�A]p�Ab�\AhQ�Amp�AqG�AuAz�HA�  A���A��
A�A�  A��\A���A�
=A�G�A��A�{A�G�A�(�A�{A���A��HA�A�  A��\A�z�A��RA���A�33A�p�A�Q�A�33A�p�A��A�=qA���A�
=A���A��HA�p�AϮA�=qA�z�A׮Aڏ\A���A�\)A��A�(�A�{A�Q�A�33A��A�  A�33A�p�A�  A��\A���A�
=B z�B��B�HB(�BG�B�HB(�B	G�B
�RB�
B��BB
=B(�Bp�B�RB  BG�B�RB�
B��BffB\)BQ�Bp�B�RB�
B!�B"�\B#�
B%�B&=qB'�B(��B)p�B*�RB,  B,��B.�\B0  B1�B2ffB3�B4��B5B6�HB8  B9G�B:ffB;�B=p�B>�\B?�
B@��BA�BC
=BDQ�BE��BF�RBH(�BI��BJ�HBL(�BMp�BN�\BO�
BP��BQ�BS33BTQ�BV{BW\)BX��BY�B[
=B[�
B]�B^ffB_�B`��Bb{Bc�
Be�BfffBg\)Bhz�BiBk
=Blz�Bm�Bo\)Bp(�BqG�Br�RBs�
Bu��Bv�HBxQ�By�BzffB{�B|��B~�\B�
B��\B�33B��B�=qB��HB��B�=qB���B���B�=qB��HB�\)B��B��\B�33B�  B��RB�\)B��
B�ffB���B��B�z�B��B��B�{B���B�\)B�=qB���B��B�(�B���B�33B��
B�ffB�G�B��B��\B�33B���B�ffB��HB��B��\B�G�B�B�Q�B�
=B��
B��\B�G�B�B�Q�B���B��B�Q�B��HB��B��
B�(�B��\B�
=B���B�  B�Q�B���B���B��B�p�B��
B�ffB��RB��B�\)B���B�  B�Q�B��RB�G�B���B�  B�Q�B���B���B�33B��B��
B�Q�B���B�33B�p�B��B�  B�Q�B���B�G�B��B�{B�Q�B��\B���B�\)B��B�=qB��\B��HB�33B���B�(�B��\B���B�
=B�p�B��B�Q�B��RB�
=B�33B���B��B�ffB��HB�33B���B��B�{B��\B���B�p�B��
B�=qB�ffB��RB�G�B�B�(�B¸RB��HB�G�BÙ�B�=qBĸRB��B�\)B�B�=qB��HB�G�BǮB��B�ffB���B�p�B��B�=qBʏ\B��HB˅B�{B�ffB̸RB��B�B�(�B�z�B��HB�33B��B�Q�BУ�B���B�p�B�  B�z�BҸRB��BӅB�(�Bԣ�B��HB�G�B��
B�ffB��HB��BׅB�=qBظRB�
=B�p�B��B�z�B���B��BۅB�=qBܣ�B�
=B�G�BݮB�=qB޸RB�
=B�p�B��
B�z�B��HB�p�BᙚB�  B�RB��B�\)B��
B�Q�B���B�p�B噚B�(�B��B�G�B�B��B�Q�B�
=B�B�B�(�B��HB�\)B뙚B�{B�RB�33B�B��B��B�
=B�\)B��B��\B���B�G�B�B�ffB���B��B�B�ffB��HB�33B���B�=qB���B�
=B���B�=qB���B��B��B�{B��RB��B�p�B��B��\B��HB�\)B�{B�z�B���B�p�C   C 33C p�C �RC  C33CffCC
=C(�CffC�RC  C�C\)CC
=C(�Cp�C��C��C=qC��C�HC{Cp�C�RC�C33C�\CC  CffC�RC�
C	�C	�C	�
C
  C
G�C
�C
�
C�C�CC  C\)C�C�
C(�C�\CC{Cz�C�C��C\)C��C�
C33C�C�RC
=CQ�C�\C�C(�CQ�C��C�
C��C(�C\)Cp�C��CCC��C
=C
=C{CG�C\)C\)C�C�C��C��C�C�HC  C(�C�CG�Cp�CffC�C�RC��C��C��C{C{C=qC\)C\)Cp�C��C��C��C�HC�C�C�C(�C33CffCffCz�C�C�C��C��C  C
=C=qC=qCG�C�\C�\C��C��C�
C�HC�C�C=qCz�Cp�C�\C��C��C��C�C�CQ�C\)Cp�C��C�C��C
=C
=C(�CffCffC��C�RCC  C
=C�CffCffC�CC��C  C�C(�C\)CffC��C�RCC��C
=C=qCQ�C\)C��C��CC  C  C33CQ�CQ�C�\C�\C�RC�HC�C �C �C G�C z�C p�C �C �C �HC ��C!  C!=qC!=qC!p�C!��C!�C!C!C!�C"{C"{C"Q�C"G�C"p�C"��C"�\C"��C"��C"��C#
=C#�C#Q�C#G�C#z�C#z�C#��C#�RC#�RC#�C#�C$�C$�C$33C$\)C$\)C$�C$�C$�RC$�RC$�C$�HC%{C%
=C%=qC%Q�C%Q�C%�\C%�C%C%C%�
C&  C&
=C&=qC&(�C&p�C&ffC&��C&��C&��C&��C'  C'  C'33C'33C'ffC'ffC'��C'��C'��C'��C'��C(
=C({C(Q�C(G�C(�C(p�C(�RC(�RC(�C(�HC){C)�C)G�C)Q�C)z�C)�C)�RC)�C)�C)�C*�C*(�C*\)C*\)C*�\C*�\C*C*��C+  C+  C+33C+=qC+ffC+p�C+��C+C+��C,  C,  C,=qC,=qC,ffC,z�C,�C,�RC,��C,��C-33C-(�C-ffC-p�C-�C-�C-��C.  C.=qC.=qC.z�C.�\C.��C.��C/{C/�C/p�C/p�C/�RC/��C0{C0�C0\)C0�\C0��C0�C0��C1G�C1\)C1��C1�C1��C2
=C2\)C2\)C2�C2C3
=C333C3G�C3�\C3��C3��C4  C4=qC4z�C4�\C4�
C4�C5{C5ffC5z�C5�C5�C6  C6=qC6z�C6��C6�HC7
=C7�C7ffC7z�C7�C7��C8  C8Q�C8ffC8�\C8�HC8�C9�C9ffC9�C9�C:  C:
=C:=qC:�C:��C:�
C;�C;(�C;z�C;�C;C<
=C<=qC<Q�C<z�C<��C<��C={C=\)C=z�C=��C=�HC=��C>(�C>p�C>�C>�C?  C?(�C?G�C?�\C?�RC?��C@{C@G�C@\)C@�\C@�
CA
=CA�CA\)CA��CA�CA�HCB(�CB33CBffCB�CB�
CB��CC33CCffCCp�CC��CC��CD
=CD33CDz�CD��CD�RCD�CE(�CE=qCEffCE�RCECE��CF=qCFffCFz�CF�RCF��CG  CG33CGz�CG��CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @B�\@}p�@�  @�  @�G�A ��A��A!G�A,��AAG�A`  A�  A�  A��A��A�  A�Q�A߮A�B   BQ�Bz�BQ�B (�B((�B0Q�B8Q�B@  BG�BO�BX  B`(�Bg�Bp  BxQ�B�  B��B��B�  B�{B�  B�  B��B��B�  B�{B�  B��B��B�(�B�{B��B�  B�  B�{B��B�  B�(�B�  B��
B��
B��B�{B�{B��B��
B�B��
C�HC��C{C  C	��C��C
=C
=C{C�C  C  C��C��C
=C 
=C"
=C$  C%��C'��C*  C,
=C.  C0
=C2  C3��C5��C8  C9��C<  C>  C?�CB  CD
=CE��CG�CJ  CL{CN
=CO�CQ�CS��CV  CX  CZ  C[��C]�C_�Cb  Cd
=Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu��Cw��Cz  C|
=C~  C�C�
=C�
=C�  C���C�  C�C���C���C���C��C��C���C���C���C�C���C���C�C�  C�C�C�C�  C���C���C���C�
=C�C�C�
=C�C�  C�C�\C�
=C���C���C�C���C���C�C�  C���C�  C�C�
=C�
=C�
=C�C�C�
=C�
=C�
=C���C���C�C�  C�  C�  C���C�  C�  C�  C�  C�C�  C���C�C�C�C���C���C�  C�C�  C�C�  C���C���C���C���C���C�C���C��C�C�  C�C�\C�C�
=C�  C���C���C�  C�C�  C�  C�C�
=C���C�C���C���C���C�  C�  C�  C�
=C�C�C�  C���C�  C�C�C�C�C�C�C�C�C�  C�C�
=C�C���D   D }qD �qD��D  D}qD�qD}qD�D��D  D� D��D��DD� D�D� D�qD	}qD
  D
}qD
�qDz�DD��D  D� D��D� DD}qD  D}qD  D� D  D� D�D� D  D� D�D� D  D� D�D� D  D��D�qD��D�qD� D  DxRD��D�D  D� D�qD� D�D}qD�qD � D!�D!��D"  D"� D#�D#}qD$�D$� D%  D%��D%��D&� D'�D'}qD(�D(}qD(�qD)� D*  D*� D+  D+� D+�qD,�D-  D-� D.�D.��D/�D/}qD/��D0� D0�qD1}qD2D2�D3  D3z�D3��D4��D5�D5��D6D6� D7  D7��D8  D8z�D8�qD9� D:�D:� D:��D;� D<  D<��D<�qD=z�D=�qD>� D>�qD?z�D@  D@}qDA�DA}qDB  DB��DCDC}qDC�RDD}qDD�qDE��DFDF� DF�qDG�DH�DH� DIDI��DI�qDJ}qDJ�qDK}qDK�qDL� DMDM�DN�DN��DO�DO}qDO��DP}qDP��DQz�DQ�qDRz�DR��DS}qDTDT� DU  DU� DU�qDV��DWDW�DW�qDX� DY�DY��DZ  DZ� DZ�qD[z�D[�qD\}qD]�D]}qD^D^��D_�D_�D`D`��D`�qDa� Da�qDb� Dc�Dc��Dd�Dd� Dd��De��Df  Df� Dg�Dg�Dh  Dh}qDi  Di� DjDj��Dj��Dkz�Dl  Dl��DmDm��Dn  Dn�Do�Do}qDo��Dp��Dq�Dq� Dr  Dr��Ds�Ds� Dt�Dt� Dt��Du}qDv  Dv� Dw  Dw� Dx  Dx}qDx�qDy}qDz  Dz� D{�D{�D|  D|��D}  D}��D~�D~z�D~��D}qD�HD�@ D�~�D�� D�HD�AHD��HD�� D�  D�B�D�~�D���D�  D�@ D�� D�� D���D�AHD�� D�� D���D�>�D��HD�� D�  D�AHD���D���D�HD�=qD��HD��HD���D�AHD��HD�� D��qD�>�D��HD��HD���D�=qD�~�D��HD�HD�@ D�~�D�� D�  D�=qD�~�D��HD�HD�@ D��HD��HD�  D�>�D�}qD���D���D�=qD�~�D�� D��qD�<)D�}qD��HD��D�>�D�~�D�� D�HD�@ D�~�D���D�  D�@ D�~�D��qD���D�AHD��HD���D���D�@ D�� D���D���D�>�D��HD�� D�  D�AHD�� D�� D�HD�>�D��HD��HD���D�>�D�~�D��)D��qD�@ D��HD�D�HD�AHD���D�D�  D�=qD�~�D��HD�HD�>�D�~�D���D���D�AHD��HD���D���D�=qD�� D��HD�HD�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D�� D��HD�HD�AHD��HD�D��D�AHD���D�D��D�AHD�� D��qD���D�AHD��HD�� D�  D�@ D�~�D���D�HD�AHD�� D��qD�  D�AHD��HD���D�HD�AHD�� D���D���D�@ D�� D�� D�HD�B�D�� D���D���D�>�D�� D�� D�  D�AHD���D�� D�  D�B�D��HD��HD�HD�@ D��HD��HD�HD�@ D�}qD���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�AHD�� D�D���D�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�~�D��HD��D�@ D�|)D�� D���D�>�D�� D���D�  D�@ D�}qD��qD��qD�>�D�� D���D�HD�=qD�� D��HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?8Q�?u?��
?Ǯ?�ff@�\@��@(��@:�H@L��@^�R@p��@�  @��@�z�@�p�@�ff@�\)@�Q�@��R@Ǯ@��@��H@��@�\)@�Q�A ��AA
�HAG�AffA�A ��A%A*=qA/\)A3�
A8Q�A<��A@  AE�AI��AN�RAR�\AXQ�A]p�Ab�\AhQ�Amp�AqG�AuAz�HA�  A���A��
A�A�  A��\A���A�
=A�G�A��A�{A�G�A�(�A�{A���A��HA�A�  A��\A�z�A��RA���A�33A�p�A�Q�A�33A�p�A��A�=qA���A�
=A���A��HA�p�AϮA�=qA�z�A׮Aڏ\A���A�\)A��A�(�A�{A�Q�A�33A��A�  A�33A�p�A�  A��\A���A�
=B z�B��B�HB(�BG�B�HB(�B	G�B
�RB�
B��BB
=B(�Bp�B�RB  BG�B�RB�
B��BffB\)BQ�Bp�B�RB�
B!�B"�\B#�
B%�B&=qB'�B(��B)p�B*�RB,  B,��B.�\B0  B1�B2ffB3�B4��B5B6�HB8  B9G�B:ffB;�B=p�B>�\B?�
B@��BA�BC
=BDQ�BE��BF�RBH(�BI��BJ�HBL(�BMp�BN�\BO�
BP��BQ�BS33BTQ�BV{BW\)BX��BY�B[
=B[�
B]�B^ffB_�B`��Bb{Bc�
Be�BfffBg\)Bhz�BiBk
=Blz�Bm�Bo\)Bp(�BqG�Br�RBs�
Bu��Bv�HBxQ�By�BzffB{�B|��B~�\B�
B��\B�33B��B�=qB��HB��B�=qB���B���B�=qB��HB�\)B��B��\B�33B�  B��RB�\)B��
B�ffB���B��B�z�B��B��B�{B���B�\)B�=qB���B��B�(�B���B�33B��
B�ffB�G�B��B��\B�33B���B�ffB��HB��B��\B�G�B�B�Q�B�
=B��
B��\B�G�B�B�Q�B���B��B�Q�B��HB��B��
B�(�B��\B�
=B���B�  B�Q�B���B���B��B�p�B��
B�ffB��RB��B�\)B���B�  B�Q�B��RB�G�B���B�  B�Q�B���B���B�33B��B��
B�Q�B���B�33B�p�B��B�  B�Q�B���B�G�B��B�{B�Q�B��\B���B�\)B��B�=qB��\B��HB�33B���B�(�B��\B���B�
=B�p�B��B�Q�B��RB�
=B�33B���B��B�ffB��HB�33B���B��B�{B��\B���B�p�B��
B�=qB�ffB��RB�G�B�B�(�B¸RB��HB�G�BÙ�B�=qBĸRB��B�\)B�B�=qB��HB�G�BǮB��B�ffB���B�p�B��B�=qBʏ\B��HB˅B�{B�ffB̸RB��B�B�(�B�z�B��HB�33B��B�Q�BУ�B���B�p�B�  B�z�BҸRB��BӅB�(�Bԣ�B��HB�G�B��
B�ffB��HB��BׅB�=qBظRB�
=B�p�B��B�z�B���B��BۅB�=qBܣ�B�
=B�G�BݮB�=qB޸RB�
=B�p�B��
B�z�B��HB�p�BᙚB�  B�RB��B�\)B��
B�Q�B���B�p�B噚B�(�B��B�G�B�B��B�Q�B�
=B�B�B�(�B��HB�\)B뙚B�{B�RB�33B�B��B��B�
=B�\)B��B��\B���B�G�B�B�ffB���B��B�B�ffB��HB�33B���B�=qB���B�
=B���B�=qB���B��B��B�{B��RB��B�p�B��B��\B��HB�\)B�{B�z�B���B�p�C   C 33C p�C �RC  C33CffCC
=C(�CffC�RC  C�C\)CC
=C(�Cp�C��C��C=qC��C�HC{Cp�C�RC�C33C�\CC  CffC�RC�
C	�C	�C	�
C
  C
G�C
�C
�
C�C�CC  C\)C�C�
C(�C�\CC{Cz�C�C��C\)C��C�
C33C�C�RC
=CQ�C�\C�C(�CQ�C��C�
C��C(�C\)Cp�C��CCC��C
=C
=C{CG�C\)C\)C�C�C��C��C�C�HC  C(�C�CG�Cp�CffC�C�RC��C��C��C{C{C=qC\)C\)Cp�C��C��C��C�HC�C�C�C(�C33CffCffCz�C�C�C��C��C  C
=C=qC=qCG�C�\C�\C��C��C�
C�HC�C�C=qCz�Cp�C�\C��C��C��C�C�CQ�C\)Cp�C��C�C��C
=C
=C(�CffCffC��C�RCC  C
=C�CffCffC�CC��C  C�C(�C\)CffC��C�RCC��C
=C=qCQ�C\)C��C��CC  C  C33CQ�CQ�C�\C�\C�RC�HC�C �C �C G�C z�C p�C �C �C �HC ��C!  C!=qC!=qC!p�C!��C!�C!C!C!�C"{C"{C"Q�C"G�C"p�C"��C"�\C"��C"��C"��C#
=C#�C#Q�C#G�C#z�C#z�C#��C#�RC#�RC#�C#�C$�C$�C$33C$\)C$\)C$�C$�C$�RC$�RC$�C$�HC%{C%
=C%=qC%Q�C%Q�C%�\C%�C%C%C%�
C&  C&
=C&=qC&(�C&p�C&ffC&��C&��C&��C&��C'  C'  C'33C'33C'ffC'ffC'��C'��C'��C'��C'��C(
=C({C(Q�C(G�C(�C(p�C(�RC(�RC(�C(�HC){C)�C)G�C)Q�C)z�C)�C)�RC)�C)�C)�C*�C*(�C*\)C*\)C*�\C*�\C*C*��C+  C+  C+33C+=qC+ffC+p�C+��C+C+��C,  C,  C,=qC,=qC,ffC,z�C,�C,�RC,��C,��C-33C-(�C-ffC-p�C-�C-�C-��C.  C.=qC.=qC.z�C.�\C.��C.��C/{C/�C/p�C/p�C/�RC/��C0{C0�C0\)C0�\C0��C0�C0��C1G�C1\)C1��C1�C1��C2
=C2\)C2\)C2�C2C3
=C333C3G�C3�\C3��C3��C4  C4=qC4z�C4�\C4�
C4�C5{C5ffC5z�C5�C5�C6  C6=qC6z�C6��C6�HC7
=C7�C7ffC7z�C7�C7��C8  C8Q�C8ffC8�\C8�HC8�C9�C9ffC9�C9�C:  C:
=C:=qC:�C:��C:�
C;�C;(�C;z�C;�C;C<
=C<=qC<Q�C<z�C<��C<��C={C=\)C=z�C=��C=�HC=��C>(�C>p�C>�C>�C?  C?(�C?G�C?�\C?�RC?��C@{C@G�C@\)C@�\C@�
CA
=CA�CA\)CA��CA�CA�HCB(�CB33CBffCB�CB�
CB��CC33CCffCCp�CC��CC��CD
=CD33CDz�CD��CD�RCD�CE(�CE=qCEffCE�RCECE��CF=qCFffCFz�CF�RCF��CG  CG33CGz�CG��CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A�ĜA���A�ĜA�ƨA�A�ȴA�ƨA���AмjA�A���A��
A��HA��`A��`A��TA��`A��`A��;A��#A���A�ĜA�ĜA���A�ĜA�ȴA���A���A���A���A���AБhA�33A�VA���A�\)A��A��
A�?}A��;A�hsA�v�A�p�A�?}A�5?A�(�A˺^A�^5A�r�A�^5AƾwA���A�VA�
=A�t�A�{A��A��jA��/A�S�A�VA���A���A��7A�5?A��hA�+A�/A��A�&�A���A�9XA�  A�  A�/A�ZA�ƨA�bNA�|�A�;dA�ĜA���A�^5A�\)A��DA�A���A��`A��A�PA{�Av�HAsp�Aj�Adn�AcVA`�A_/A\bNAW�ATZAP1AKVAEA>�jA:�yA9S�A7�-A6  A4�A1�wA/ƨA.�\A-ƨA-
=A+�
A*��A*�A)�
A)33A(ĜA(ĜA(�RA(M�A'x�A'G�A&�HA%��A%oA$�A$��A%VA%oA$�jA$A�A"�A �!A 1'A��A��A�hA^5A �A��AVA�;A��A��A|�A1A�A �A�TA��A��A�#A~�A��A�A^5AVA�uAM�A-AAA33A�AE�AA�A&�A5?A+AjA;dA��An�A5?A�AM�A�
Ax�A��A�wA
��A	+A(�A�AK�AVA��AĜA�Ar�AdZA�+A��A	&�A	l�A	��A	�A	�A^5A  A�A��A��A�RAffA�A�wA;dA�9A^5A��A�PAO�A��A9XA1A��Al�A�A
=A �yA �jA �A v�@��F@�K�@��y@�@�G�@�Ĝ@���@��u@�r�@�Q�@�A�@�1'@��w@��R@�V@�E�@�@���@��@��D@�(�@��F@�+@�n�@��@�z�@�"�@�E�@�`B@�9@@��y@�$�@�(�@�;d@�^5@��@�p�@�Ĝ@���@�R@�-@�@��#@�hs@�@�I�@�F@��H@�-@���@��@�Z@�  @߶F@�dZ@���@�V@��@���@ݩ�@�x�@�hs@�O�@�G�@�/@���@�Q�@۝�@�33@ڰ!@ڇ+@١�@�Q�@�|�@�o@ְ!@�@Լj@��@�K�@���@Ұ!@�v�@�-@��@Ѳ-@�%@�j@� �@��@�V@�X@̼j@�bN@��@˕�@�33@��y@���@ʧ�@�~�@��@ə�@�hs@�G�@��@�Ĝ@�Q�@�9X@� �@�t�@Ƈ+@�J@��@Ĭ@ēu@�A�@�l�@��y@�@�E�@���@��@��w@��y@��@��-@���@��@�bN@�(�@��P@��H@��#@�`B@�G�@���@��u@�9X@��F@�|�@���@�@�O�@��@�1@���@��F@��@�K�@�33@�"�@���@�n�@�@�O�@���@��9@��D@�Q�@�\)@��@�n�@��@��@��^@�x�@�G�@��@��@��j@�Z@��;@�|�@��R@�E�@���@��@��@�9X@�  @�ƨ@��@��H@���@�M�@��@�J@�@��@���@��@���@��D@�1'@�b@��
@�33@��+@�@���@�@��^@���@�`B@��/@�1@�l�@��@���@��#@�7L@��@�j@�(�@�b@��
@��@�l�@�l�@�dZ@�
=@�5?@��@���@�x�@�O�@�&�@���@���@��D@�9X@��;@���@�|�@�
=@��+@�-@��#@���@�O�@���@��@�Q�@��@�  @��@��;@��w@���@�C�@��H@���@��!@���@�v�@�^5@��@�@�x�@�X@�V@���@��9@��@�t�@�C�@���@���@�V@�J@���@���@�X@��@��/@��@�t�@�ȴ@���@���@��h@��h@��@�X@���@��9@�r�@� �@��
@��P@�+@��R@�n�@�E�@�J@���@��T@��-@�7L@��9@��@�Z@��
@��P@�
=@���@�{@��@���@��7@�7L@���@��D@�r�@�I�@�(�@�1@�w@��@l�@~��@}�T@}�@}�@}p�@}`B@}�@|��@|��@|z�@|1@{"�@z�@z�@z��@z�\@z=q@zJ@y��@y��@yx�@yX@yX@y&�@x��@xĜ@xQ�@w�w@w\)@w�@v��@v{@u@u��@u`B@t�/@tz�@t9X@sƨ@s�F@s�@r��@r-@q��@q�^@q�7@qhs@qX@qX@q7L@p�`@p��@p�9@pbN@p �@o�@n�R@n{@m`B@m�@lj@k�
@kdZ@ko@j��@j^5@jJ@i��@ix�@h��@h��@hQ�@hb@g�@g�@g�P@gK�@f�@f@e�-@e�@ep�@eO�@d�@d�/@d��@d��@dI�@c��@bn�@bJ@a��@aX@`�`@`�9@`r�@`1'@`  @_|�@_
=@^�y@^ȴ@^��@^��@^��@^v�@^ff@^ff@^E�@^$�@]�-@]/@\�@\�j@\z�@\9X@[�m@[��@[33@Z�H@Z�!@Z�\@Z~�@Z=q@Y�@Y�@Y�#@Y��@Y��@Y��@Y�7@Yx�@YX@Y�@X�`@X1'@W\)@V�y@Vv�@V@U`B@U�@T�@T�D@Tz�@T�D@Tz�@Tj@T9X@S��@SdZ@R�H@R��@RJ@Q��@QG�@QG�@Q�@Pr�@P �@O�@O�P@N�y@N�+@NV@NV@N5?@N{@Mp�@M�@L��@L��@L�@LZ@K��@K�
@K��@K33@J��@J�\@J^5@J-@I�#@Ix�@H�`@HbN@Hb@G�w@G\)@G
=@Fȴ@Fv�@F{@E��@Ep�@Ep�@D�/@Dj@DZ@D9X@C�
@CC�@C"�@B�@B��@A�#@Ax�@@��@@�@@1'@?|�@?;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�ƨA�ĜA�A���A���A���A�A�ĜA�ĜA�A�ĜA�ĜA�ĜA�A�A�A���AоwA�ȴA�ƨA�ȴA�ȴA�ȴA�ƨA�ĜA�A�A���AоwAмjAоwAмjAоwA�ȴA���A�ȴA���A�ȴA��A��/A��/A��;A��/A��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��TA��`A��`A��`A��mA��mA��yA��yA��mA��mA��TA��;A��HA��TA��HA��HA��HA��HA��`A��`A��mA��mA��yA��mA��mA��`A��TA��HA��;A��HA��TA��;A��;A��HA��HA��/A��#A��#A��A��#A��A��A���A���A���A���A���A�ƨA�ƨA�ĜA�ĜA�ĜA�A�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA���AоwAоwA���A�A�ĜA�ĜA�A�A�A�A���A���A���A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A�A�ȴA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��#A��/A��A���A���A�ȴA�ƨA�ƨA�ȴA�ƨA�ĜAоwAв-AЩ�AЩ�AЩ�AЧ�AЗ�A�dZA�E�A�A�A�9XA�1'A�-A�+A�-A�-A�(�A��A�oA�VA���A���A��`A���A���A�ĜAϸRAϴ9Aϩ�AϋDA�`BA�K�A�&�A�&�A�1'A�$�A� �A��A��A�oA�{A��A�JA���Aδ9AάA΋DA�x�A�\)A�I�A�/A�"�A�bA��A��A��;A��#A��A���AͲ-A͉7A�~�A�l�A�\)A�M�A�7LA�/A�+A���A�-A��A��#A�AˋDA�p�A�dZA�S�A�VA�XA�XA�M�A�E�A�;dA�/A��A���A���A�%A�33A˃A˾wA��A�/A�O�A�A�A�/A��A��A˴9A˲-Aˣ�A˝�AˮA˴9Aˣ�A�z�A�VA�I�A�O�A�S�A�XA�\)A�G�A�A�A�ZA�VA�I�A�C�A�(�A�JAʸRA�I�Aɧ�A�dZA�9XA�A���AȲ-Aȡ�A�v�A�bNA�XA�M�A�=qA�5?A���Aǥ�A�n�A�Q�A�;dA�-A��A���Aơ�A�jA�1'A���A�ȴAř�A�ffA�33A�bA��A�Aħ�Aę�AđhA�v�A�E�A�-A�(�A�+A�+A�$�A� �A�VA��TA��mA��mA��HA��TA��A��A���AÍPA���A7A�1'A��A�ȴA�bNA� �A��HA���A��A�ffA�S�A�+A��9A�Q�A��mA���A�-A��A���A��`A���A��A��PA�jA�/A�
=A��A��mA��A��/A��jA���A�v�A�VA�7LA�bA��
A�z�A�?}A�1'A��A�A�A�A�A�A���A��A���A��A�v�A���A�"�A��7A�O�A��A���A���A�ƨA�ȴA���A��A�x�A�r�A�`BA�7LA�$�A�VA��A���A���A�hsA�5?A���A�~�A�K�A�=qA�S�A�VA�1'A���A�ƨA���A�`BA�$�A�bA���A��HA���A���A��A�t�A�p�A�ZA�K�A�I�A� �A��;A���A��A�`BA��A���A�{A���A��A�A��-A��7A�^5A�n�A�bNA�K�A�(�A���A��!A�7LA���A���A�?}A�VA�A��uA�p�A�7LA���A��
A��-A���A��7A�z�A�bNA�O�A��A��`A���A��A�K�A��A���A�K�A���A��FA��DA�bNA�%A��9A�n�A�bA���A�`BA�{A��FA�VA�oA���A�\)A��A��yA���A��PA�oA��wA�G�A���A��A���A��A�ZA�G�A�33A��A�  A���A��RA��A���A�ffA�$�A��A��A��A���A���A��PA�~�A�x�A�G�A���A��#A�ĜA��uA�A��A�9XA���A���A���A��DA�p�A�ffA�7LA��A��A��9A�~�A�n�A�ffA�ffA�`BA�^5A�\)A�ZA�Q�A�VA�Q�A�C�A�C�A�G�A�E�A�E�A�=qA�7LA�33A�7LA�-A�-A�+A�"�A� �A��A��A�oA�oA�oA�VA�JA�%A���A��yA��yA��A��wA��!A���A���A��hA��DA��A�v�A�r�A�hsA�bNA�^5A�M�A�A�A�1'A��A�1A��A���A���A��9A���A���A��+A�t�A�S�A�O�A� �A��A�JA�A��A��
A��A��-A��A��uA��+A�n�A�ZA�C�A�{A���A���A�VA�bA��A���A���A�S�A��A�ĜA��A�O�A�"�A�VA�A��A�JA� �A�`BA��A���A��`A�ffA���A��/A��A���A�ZA�9XA��A���A���A���A��PA�^5A�9XA��A���A���A��hA�O�A��A��;A���A�\)A�5?A�A��A���A��^A���A���A�~�A�n�A�S�A�5?A��A��A�  A��A���A��hA��A�;dA���A���A���A�v�A�dZA�`BA�I�A�;dA�(�A� �A�oA�%A���A��yA���A��^A���A���A��hA�z�A�jA�Q�A�5?A�oA�A��HA�A���A��PA�t�A�\)A�K�A�1'A�bA��HA��A�XA�"�A���A��#A�A�"�A��uA�/A���A��#A��wA�r�A��A��!A���A��+A�hsA�^5A�E�A�
=A��A��RA��PA�O�A��A�{A�JA�%A�  A�A�#A��AG�A~��A~��A~�A~~�A~jA~jA~^5A~M�A~-A~bA}�;A}��A}VA|VA{ƨA{�hA{\)A{G�A{�A{�A{
=Az��Az^5AzAy�AyVAx��AxZAx-Ax{Aw��Aw��Aw�FAw��Aw�AwXAw+Av��Av~�Av5?Au�Au��Au\)Au;dAu/AuoAt��At��At��Atv�AtZAt$�As��Ast�As&�Arz�AqƨAqK�Ap�ApZAox�An�An�AmhsAk�Ai��AhffAghsAgAf��Af(�Ae�
Ae�-Ael�Ad�/AdQ�AdAd  Ac��Ac��Ac�TAc�Ac�TAc�
AcƨAc��Ac��Acl�Ac7LAb��AbffAb-Aa��Aa�;AaƨAa�PAaG�Aa�AaA`�A`��A`��A`ZA`-A_�wA_��A_�PA_�hA_�PA_�hA_�hA_l�A^��A^�A^�A]��A]�FA]hsA]+A\�yA\��A\=qA[�#A[hsA[�AZ��AZ$�AY��AX�AX~�AX �AW��AW|�AW&�AV��AVffAVJAU�^AUt�AU?}AU�AT��AT~�ATAS��AS7LAR��ARr�AR$�AQ��AQC�AP��AP�AP�AOhsAN��AM�;AM%ALĜAL�9AL��AL^5AL=qAK��AKO�AJ��AI��AI`BAHz�AG�AG�AG�AG�AG�AE��AD�AD  ABĜAB{AA��AAG�A@�`A@  A>�`A>v�A>ffA>ZA>5?A>(�A>{A=�A=A=VA<{A;p�A;33A:ȴA:�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜA���A�ĜA�ƨA�A�ȴA�ƨA���AмjA�A���A��
A��HA��`A��`A��TA��`A��`A��;A��#A���A�ĜA�ĜA���A�ĜA�ȴA���A���A���A���A���AБhA�33A�VA���A�\)A��A��
A�?}A��;A�hsA�v�A�p�A�?}A�5?A�(�A˺^A�^5A�r�A�^5AƾwA���A�VA�
=A�t�A�{A��A��jA��/A�S�A�VA���A���A��7A�5?A��hA�+A�/A��A�&�A���A�9XA�  A�  A�/A�ZA�ƨA�bNA�|�A�;dA�ĜA���A�^5A�\)A��DA�A���A��`A��A�PA{�Av�HAsp�Aj�Adn�AcVA`�A_/A\bNAW�ATZAP1AKVAEA>�jA:�yA9S�A7�-A6  A4�A1�wA/ƨA.�\A-ƨA-
=A+�
A*��A*�A)�
A)33A(ĜA(ĜA(�RA(M�A'x�A'G�A&�HA%��A%oA$�A$��A%VA%oA$�jA$A�A"�A �!A 1'A��A��A�hA^5A �A��AVA�;A��A��A|�A1A�A �A�TA��A��A�#A~�A��A�A^5AVA�uAM�A-AAA33A�AE�AA�A&�A5?A+AjA;dA��An�A5?A�AM�A�
Ax�A��A�wA
��A	+A(�A�AK�AVA��AĜA�Ar�AdZA�+A��A	&�A	l�A	��A	�A	�A^5A  A�A��A��A�RAffA�A�wA;dA�9A^5A��A�PAO�A��A9XA1A��Al�A�A
=A �yA �jA �A v�@��F@�K�@��y@�@�G�@�Ĝ@���@��u@�r�@�Q�@�A�@�1'@��w@��R@�V@�E�@�@���@��@��D@�(�@��F@�+@�n�@��@�z�@�"�@�E�@�`B@�9@@��y@�$�@�(�@�;d@�^5@��@�p�@�Ĝ@���@�R@�-@�@��#@�hs@�@�I�@�F@��H@�-@���@��@�Z@�  @߶F@�dZ@���@�V@��@���@ݩ�@�x�@�hs@�O�@�G�@�/@���@�Q�@۝�@�33@ڰ!@ڇ+@١�@�Q�@�|�@�o@ְ!@�@Լj@��@�K�@���@Ұ!@�v�@�-@��@Ѳ-@�%@�j@� �@��@�V@�X@̼j@�bN@��@˕�@�33@��y@���@ʧ�@�~�@��@ə�@�hs@�G�@��@�Ĝ@�Q�@�9X@� �@�t�@Ƈ+@�J@��@Ĭ@ēu@�A�@�l�@��y@�@�E�@���@��@��w@��y@��@��-@���@��@�bN@�(�@��P@��H@��#@�`B@�G�@���@��u@�9X@��F@�|�@���@�@�O�@��@�1@���@��F@��@�K�@�33@�"�@���@�n�@�@�O�@���@��9@��D@�Q�@�\)@��@�n�@��@��@��^@�x�@�G�@��@��@��j@�Z@��;@�|�@��R@�E�@���@��@��@�9X@�  @�ƨ@��@��H@���@�M�@��@�J@�@��@���@��@���@��D@�1'@�b@��
@�33@��+@�@���@�@��^@���@�`B@��/@�1@�l�@��@���@��#@�7L@��@�j@�(�@�b@��
@��@�l�@�l�@�dZ@�
=@�5?@��@���@�x�@�O�@�&�@���@���@��D@�9X@��;@���@�|�@�
=@��+@�-@��#@���@�O�@���@��@�Q�@��@�  @��@��;@��w@���@�C�@��H@���@��!@���@�v�@�^5@��@�@�x�@�X@�V@���@��9@��@�t�@�C�@���@���@�V@�J@���@���@�X@��@��/@��@�t�@�ȴ@���@���@��h@��h@��@�X@���@��9@�r�@� �@��
@��P@�+@��R@�n�@�E�@�J@���@��T@��-@�7L@��9@��@�Z@��
@��P@�
=@���@�{@��@���@��7@�7L@���@��D@�r�@�I�@�(�@�1@�w@��@l�@~��@}�T@}�@}�@}p�@}`B@}�@|��@|��@|z�@|1@{"�@z�@z�@z��@z�\@z=q@zJ@y��@y��@yx�@yX@yX@y&�@x��@xĜ@xQ�@w�w@w\)@w�@v��@v{@u@u��@u`B@t�/@tz�@t9X@sƨ@s�F@s�@r��@r-@q��@q�^@q�7@qhs@qX@qX@q7L@p�`@p��@p�9@pbN@p �@o�@n�R@n{@m`B@m�@lj@k�
@kdZ@ko@j��@j^5@jJ@i��@ix�@h��@h��@hQ�@hb@g�@g�@g�P@gK�@f�@f@e�-@e�@ep�@eO�@d�@d�/@d��@d��@dI�@c��@bn�@bJ@a��@aX@`�`@`�9@`r�@`1'@`  @_|�@_
=@^�y@^ȴ@^��@^��@^��@^v�@^ff@^ff@^E�@^$�@]�-@]/@\�@\�j@\z�@\9X@[�m@[��@[33@Z�H@Z�!@Z�\@Z~�@Z=q@Y�@Y�@Y�#@Y��@Y��@Y��@Y�7@Yx�@YX@Y�@X�`@X1'@W\)@V�y@Vv�@V@U`B@U�@T�@T�D@Tz�@T�D@Tz�@Tj@T9X@S��@SdZ@R�H@R��@RJ@Q��@QG�@QG�@Q�@Pr�@P �@O�@O�P@N�y@N�+@NV@NV@N5?@N{@Mp�@M�@L��@L��@L�@LZ@K��@K�
@K��@K33@J��@J�\@J^5@J-@I�#@Ix�@H�`@HbN@Hb@G�w@G\)@G
=@Fȴ@Fv�@F{@E��@Ep�@Ep�@D�/@Dj@DZ@D9X@C�
@CC�@C"�@B�@B��@A�#@Ax�@@��@@�@@1'@?|�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�ƨA�ĜA�A���A���A���A�A�ĜA�ĜA�A�ĜA�ĜA�ĜA�A�A�A���AоwA�ȴA�ƨA�ȴA�ȴA�ȴA�ƨA�ĜA�A�A���AоwAмjAоwAмjAоwA�ȴA���A�ȴA���A�ȴA��A��/A��/A��;A��/A��TA��TA��TA��TA��HA��HA��HA��HA��HA��TA��TA��`A��`A��`A��mA��mA��yA��yA��mA��mA��TA��;A��HA��TA��HA��HA��HA��HA��`A��`A��mA��mA��yA��mA��mA��`A��TA��HA��;A��HA��TA��;A��;A��HA��HA��/A��#A��#A��A��#A��A��A���A���A���A���A���A�ƨA�ƨA�ĜA�ĜA�ĜA�A�ĜA�ĜA�ĜA�ƨA�ƨA�ƨA�ĜA���AоwAоwA���A�A�ĜA�ĜA�A�A�A�A���A���A���A�ĜA�ƨA�ȴA�ȴA�ȴA���A���A���A�A�ȴA�ȴA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A��#A��#A��/A��A���A���A�ȴA�ƨA�ƨA�ȴA�ƨA�ĜAоwAв-AЩ�AЩ�AЩ�AЧ�AЗ�A�dZA�E�A�A�A�9XA�1'A�-A�+A�-A�-A�(�A��A�oA�VA���A���A��`A���A���A�ĜAϸRAϴ9Aϩ�AϋDA�`BA�K�A�&�A�&�A�1'A�$�A� �A��A��A�oA�{A��A�JA���Aδ9AάA΋DA�x�A�\)A�I�A�/A�"�A�bA��A��A��;A��#A��A���AͲ-A͉7A�~�A�l�A�\)A�M�A�7LA�/A�+A���A�-A��A��#A�AˋDA�p�A�dZA�S�A�VA�XA�XA�M�A�E�A�;dA�/A��A���A���A�%A�33A˃A˾wA��A�/A�O�A�A�A�/A��A��A˴9A˲-Aˣ�A˝�AˮA˴9Aˣ�A�z�A�VA�I�A�O�A�S�A�XA�\)A�G�A�A�A�ZA�VA�I�A�C�A�(�A�JAʸRA�I�Aɧ�A�dZA�9XA�A���AȲ-Aȡ�A�v�A�bNA�XA�M�A�=qA�5?A���Aǥ�A�n�A�Q�A�;dA�-A��A���Aơ�A�jA�1'A���A�ȴAř�A�ffA�33A�bA��A�Aħ�Aę�AđhA�v�A�E�A�-A�(�A�+A�+A�$�A� �A�VA��TA��mA��mA��HA��TA��A��A���AÍPA���A7A�1'A��A�ȴA�bNA� �A��HA���A��A�ffA�S�A�+A��9A�Q�A��mA���A�-A��A���A��`A���A��A��PA�jA�/A�
=A��A��mA��A��/A��jA���A�v�A�VA�7LA�bA��
A�z�A�?}A�1'A��A�A�A�A�A�A���A��A���A��A�v�A���A�"�A��7A�O�A��A���A���A�ƨA�ȴA���A��A�x�A�r�A�`BA�7LA�$�A�VA��A���A���A�hsA�5?A���A�~�A�K�A�=qA�S�A�VA�1'A���A�ƨA���A�`BA�$�A�bA���A��HA���A���A��A�t�A�p�A�ZA�K�A�I�A� �A��;A���A��A�`BA��A���A�{A���A��A�A��-A��7A�^5A�n�A�bNA�K�A�(�A���A��!A�7LA���A���A�?}A�VA�A��uA�p�A�7LA���A��
A��-A���A��7A�z�A�bNA�O�A��A��`A���A��A�K�A��A���A�K�A���A��FA��DA�bNA�%A��9A�n�A�bA���A�`BA�{A��FA�VA�oA���A�\)A��A��yA���A��PA�oA��wA�G�A���A��A���A��A�ZA�G�A�33A��A�  A���A��RA��A���A�ffA�$�A��A��A��A���A���A��PA�~�A�x�A�G�A���A��#A�ĜA��uA�A��A�9XA���A���A���A��DA�p�A�ffA�7LA��A��A��9A�~�A�n�A�ffA�ffA�`BA�^5A�\)A�ZA�Q�A�VA�Q�A�C�A�C�A�G�A�E�A�E�A�=qA�7LA�33A�7LA�-A�-A�+A�"�A� �A��A��A�oA�oA�oA�VA�JA�%A���A��yA��yA��A��wA��!A���A���A��hA��DA��A�v�A�r�A�hsA�bNA�^5A�M�A�A�A�1'A��A�1A��A���A���A��9A���A���A��+A�t�A�S�A�O�A� �A��A�JA�A��A��
A��A��-A��A��uA��+A�n�A�ZA�C�A�{A���A���A�VA�bA��A���A���A�S�A��A�ĜA��A�O�A�"�A�VA�A��A�JA� �A�`BA��A���A��`A�ffA���A��/A��A���A�ZA�9XA��A���A���A���A��PA�^5A�9XA��A���A���A��hA�O�A��A��;A���A�\)A�5?A�A��A���A��^A���A���A�~�A�n�A�S�A�5?A��A��A�  A��A���A��hA��A�;dA���A���A���A�v�A�dZA�`BA�I�A�;dA�(�A� �A�oA�%A���A��yA���A��^A���A���A��hA�z�A�jA�Q�A�5?A�oA�A��HA�A���A��PA�t�A�\)A�K�A�1'A�bA��HA��A�XA�"�A���A��#A�A�"�A��uA�/A���A��#A��wA�r�A��A��!A���A��+A�hsA�^5A�E�A�
=A��A��RA��PA�O�A��A�{A�JA�%A�  A�A�#A��AG�A~��A~��A~�A~~�A~jA~jA~^5A~M�A~-A~bA}�;A}��A}VA|VA{ƨA{�hA{\)A{G�A{�A{�A{
=Az��Az^5AzAy�AyVAx��AxZAx-Ax{Aw��Aw��Aw�FAw��Aw�AwXAw+Av��Av~�Av5?Au�Au��Au\)Au;dAu/AuoAt��At��At��Atv�AtZAt$�As��Ast�As&�Arz�AqƨAqK�Ap�ApZAox�An�An�AmhsAk�Ai��AhffAghsAgAf��Af(�Ae�
Ae�-Ael�Ad�/AdQ�AdAd  Ac��Ac��Ac�TAc�Ac�TAc�
AcƨAc��Ac��Acl�Ac7LAb��AbffAb-Aa��Aa�;AaƨAa�PAaG�Aa�AaA`�A`��A`��A`ZA`-A_�wA_��A_�PA_�hA_�PA_�hA_�hA_l�A^��A^�A^�A]��A]�FA]hsA]+A\�yA\��A\=qA[�#A[hsA[�AZ��AZ$�AY��AX�AX~�AX �AW��AW|�AW&�AV��AVffAVJAU�^AUt�AU?}AU�AT��AT~�ATAS��AS7LAR��ARr�AR$�AQ��AQC�AP��AP�AP�AOhsAN��AM�;AM%ALĜAL�9AL��AL^5AL=qAK��AKO�AJ��AI��AI`BAHz�AG�AG�AG�AG�AG�AE��AD�AD  ABĜAB{AA��AAG�A@�`A@  A>�`A>v�A>ffA>ZA>5?A>(�A>{A=�A=A=VA<{A;p�A;33A:ȴA:�u111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B� B��B�B�B��B�B��B�B�HB�B�TB�ZB��B�mB��B�sB�B�B�oB�AB�MB�MB��B��B��B��B��B��B	;B	JB	uB	qB	)_B	*�B	+6B	.�B	7�B	H�B	MB	M�B	G�B	IRB	W�B	m�B	�-B
L�B
��B
�jB{B�B�B9�B6BB'B<BaB��B��B�XB��B��B�!B�B�WB��B��B��B�2B�B�`B��B�0B�@B� B��B�1B��Be�BT�BK^B>�B(�BfB
�B
_�B
#�B	�fB	�vB	��B	��B	j�B	GEB	/B	�B	�B�.B�xB��B�B�&B��B�B�pB��Bl�Bh�Br|B��B��B�bB�tB��B��B�<B�WB��B�B�DB�B��B	 �B	�B	�B	$�B	=�B	F�B	QNB	P�B	K�B	JXB	QNB	\�B	_�B	^5B	]�B	gmB	YKB	Y�B	QB	7�B	$�B	�B��B�8B��B�B�xB	 �B	1B	{B	O�B	h�B	f�B	�GB	n/B	��B	�UB	�B	��B	�B	��B	��B	�XB	B	�9B	��B	�TB	�B	��B	�B	�fB	��B	��B	�BB	�EB	��B	�B	�HB	��B	�B	ܒB	�;B	یB	ӏB	��B	��B	��B	��B	��B	��B	�aB	��B	��B	�zB	��B	�B	��B	��B
�B
xB
�B
�B
B
�B
�B
�B
�B
B
�B
uB
;B
�B
�B
 �B
�B
 4B
  B
;B
 4B	�cB	��B
  B	��B	��B	�]B	��B	�"B	�.B	��B	�B	��B	��B	��B	�>B	�>B	��B	�	B	��B	�B	�lB	�	B	��B	��B	��B	��B	��B	�+B	�`B	�+B	�ZB	�TB	��B	�MB	�|B	�B	�iB	�iB	�B	�B	�B	�vB	�GB	�B	��B	�5B	�5B	�5B	�GB	�B	�B	�B	��B	�B	�B	�oB	��B	�B	�B	�]B	�B	�]B	�/B	�WB	��B	�)B	�WB	�QB	�B	�B	�KB	��B	�B	�B	��B	�B	�B	�mB	�2B	�2B	�B	�B	�`B	� B	�NB	�|B	�B	�TB	�B	� B	�8B	�
B	��B	�B	�QB	�)B	�B	��B	�KB	�B	�B	�B	�B	�WB	�B	��B	�B	�QB	�B	�B	��B	�KB	��B	��B	�B	�]B	�)B	��B	��B	�B	��B	�]B	�iB	�B	�B	�;B	�vB	�B	��B	�B	��B	�vB	�iB	�B	�KB	�
B	�
B	��B	�B	��B	�B	�DB	�B	��B	��B	�B	��B	��B	�iB	�B	� B	��B	�B	�iB	�iB	�iB	�5B	�B	�B	��B	�5B	�B	� B	�B	�B	�;B	�;B	�B	��B	�B	�B	�AB	��B	�B	�B	��B	��B	�vB	�AB	�B	�AB	�vB	�vB	�B	�B	��B	�iB	�oB	�B	�;B	�5B	��B	�B	�AB	�vB	�|B	�B	�B	��B	��B	��B	��B	��B	�lB	�	B	��B	�>B	��B	�B	�VB	��B	��B	�"B	�"B	�VB	�]B	��B
 �B
 4B
�B
�B
�B
;B
;B
;B
;B
�B
AB
uB
�B
B
GB
MB
B
B
�B
�B
�B
�B
%B
�B
�B
+B
�B
_B
�B
	B
	B
	7B
	B

�B

	B

�B
B
DB
DB
DB
DB
xB
DB
~B
B
PB
PB
�B
�B
�B
4B
oB
FB
�B
�B
�B
�B
�B
YB
$B
�B
YB
YB
YB
$B
�B
YB
�B
�B
+B
�B
�B
SB
B
SB
SB
�B
SB
SB
SB
SB
�B
�B
$B
�B
�B
_B
�B
�B
�B
1B
eB
	B
=B
	B
	B
CB
B
IB
OB
�B
�B
VB
!B
 \B
 �B
 �B
 �B
!bB
!bB
!�B
!�B
"4B
!�B
#B
#�B
$@B
#�B
$B
$@B
$tB
$@B
$tB
$�B
%B
&�B
&LB
&LB
&�B
'B
'B
'�B
'�B
(XB
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*eB
*�B
+�B
,=B
,�B
,�B
,�B
-wB
-wB
-�B
.B
.B
.IB
0!B
/�B
0!B
0�B
0�B
0�B
0�B
0�B
1'B
1[B
1[B
1�B
2-B
2�B
3�B
49B
3�B
4B
3�B
33B
3hB
3�B
3�B
49B
5B
5�B
5tB
6FB
6�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
:*B
:*B
:^B
:�B
:�B
;0B
;�B
<6B
<6B
<�B
<�B
=<B
=�B
=<B
=<B
=B
=<B
<�B
=<B
=B
<�B
=�B
?�B
@�B
@�B
@OB
@OB
@OB
@OB
@OB
@B
@B
?�B
@OB
?�B
?�B
?�B
?�B
?�B
?�B
?�B
?�B
@OB
@OB
@B
@B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
B'B
B�B
CaB
C�B
D�B
D�B
EmB
E9B
E9B
E9B
E9B
E9B
EmB
EmB
F?B
F?B
F?B
GEB
F�B
GEB
GEB
GB
G�B
G�B
G�B
HB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
IRB
JXB
JXB
JXB
J�B
J�B
K�B
J�B
K�B
K^B
K�B
K�B
L�B
L�B
MB
MB
MjB
M6B
M�B
NB
N<B
NpB
N�B
N<B
OvB
O�B
OvB
OvB
O�B
P}B
PHB
PHB
P}B
Q�B
Q�B
R�B
R�B
R�B
S�B
TaG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�TB�B��B�TB�NB�B�B�B�B�B�B�B�B�TB�B�&B�TB�B�TB�TB�&B�B��B�B��B�BB�B��B�vB�B�B�HB�B�HB�;B��B��B��B��B� B�,B�`B��B�,B�B�B��B��B�fB�
B�
B�
B�
B�
B�B�mB�B��B�2B��B�mB�B�8B��B�B�2B�>B�)B��B�KB�B�fB��B�B�"B�WB�B�B�WB��B�cB��B�B��B��B�B�oB�iB�B�B��B�vB��B�AB�B��B��B��B�|B�|B�B�B�B�B�TB�TB�B�B�B�B�|B�B�TB��B��B�%B��B�TB��B�MB�MB�MB�B�B��B�B��B�%B�%B�`B�fB��B�+B�ZB�`B�`B�`B��B�8B��B��B��B��B��B��B�B�VB��B	 4B	  B�cB��B��B	 4B	 �B	{B	B	�B	B	(B	(B	(B	�B	4B	B	oB	B	uB	SB	�B	SB	B	B	�B	)_B	&�B	'�B	)_B	+B	*0B	*�B	*0B	)_B	*eB	*�B	)�B	)�B	+B	(�B	,=B	+�B	*�B	,B	*�B	*0B	+6B	0!B	1'B	/B	49B	.�B	,=B	5B	3�B	9XB	9XB	9�B	:^B	;dB	EmB	NB	L0B	K�B	QNB	K^B	OvB	K^B	L0B	K�B	N�B	PB	NpB	P�B	MjB	L0B	K)B	T�B	J�B	GzB	IRB	GEB	@�B	@�B	?}B	E�B	YKB	OBB	@�B	B�B	H�B	S�B	R�B	V�B	]/B	aB	a|B	c�B	j�B	ncB	poB	q�B	w�B	}"B	�B	�1B	��B	�B	�B
�B
@�B
O�B
o�B
i�B
p�B
r�B
u�B
y�B
�eB
��B
�B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
�B
�,B
��B
�AB
�]B�BB�B�BfB	7B�B�B�B(B�BuBhB�B�B4B�B=BBYB{B�B�B�B#B�B$B%�B.IB3hB3hB5�B6�B;0B=�B?HB?HB9�B:�B7�B-�B0�B1�B3�B1�B2�B8�B3�B:*BB'BA B>�BJ�BMBNpBR�BT�BB�B2�B)�B3�B8�B1�B<jB8�B9XB>BB:^BC�BE�BD�BA BB�BH�BI�Be�By>Bw2B�B�B��B�~B��B�7B��B��B�"B�=B��B�rB�fB��B�fB�JB��B�$B�B�CB�-B�B��B��B�LB�B��B�6B��B�IB��B�B��B��B��B��B�?B��B��B�jB��B�B��B�0B�CB��B�eB�B��B��B��B�!B��B�wB�B�?B��B� B�B�&B�B�B�B�]B�5B�)B��B�]B�AB�B�)B�B�)B��B�sB�QB�5B�B�B�B�&BoB� B��B�B�]B�mB�B�ZB��B��B�cB�B��B�B�PB;B��B��B  B��B�JB�`B��B�%B�B�B�]B��B�B��B�DB�QB�B�WB� B�yB��B�>B�B�vB�B��B��B�ZB�#B��B��B�QBƨB�NB��BȴB�$B�aB��B�3B��B��B�$B��B�B�0B��B�B�OB��B��B��B��B�@B� B�uB�JB�	B�7B�B�7B�_B��B��B�rB�_B�+B�fB��B��B��B��B�{B�B�bB�%B}�BqvBo�Bm]Bc�Bh>BbBgB_pB]/Bd�B[�BS�BR�BOvBPHBQ�BM�BN�BN�BMjBL�BMjBK�BK)BL�BL�BK�BJ�BL0BI�BJ�BI�BHKBGBH�BH�BE9BF�BE�BD�BFBD3BC�BD�BB[B?�B@OBCaB>�B<B;�B9$B9$B:*B9�B5?B5�B6zB4B5?B1�B4�B2�B2�B-�B-�B)�B*0B+B'�B%�B&B+kB#�B+�B!�B!bB!�BYB"4B�BxB�B$B_BB(BVB�BDB~B�B
�B
��B
�B
�B
�B
��B
�B
��B
�B
�EB
��B
ȴB
�aB
�&B
ޞB
�OB
�LB
��B
�kB
�=B
��B
{�B
~�B
w2B
{B
s�B
qvB
bNB
c�B
kB
`BB
X�B
TaB
QNB
K�B
QNB
J#B
E�B
F�B
@�B
;�B
2�B
/OB
/�B
$�B
$�B
�B
�B
CB
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
 �B
fB
uB	��B	�fB	�MB	�AB	�cB	� B	�;B	�DB	�KB	�2B	�B	�B	�TB	�5B	ޞB	�5B	��B	��B	��B	�EB	�WB	�gB	��B	��B	҉B	�B	�RB	�#B	�mB	ĜB	�OB	�qB	��B	��B	�B	�B	��B	��B	�tB	�FB	��B	�B	��B	�1B	��B	��B	��B	��B	~�B	{JB	|B	.B	.B	�MB	�B	��B	��B	��B	��B	��B	��B	��B	�GB	�B	�oB	��B	��B	�B	�lB	~�B	~�B	|PB	}�B	zxB	{�B	y>B	|B	xB	y�B	zDB	}VB	y>B	n�B	jKB	jKB	ffB	d�B	`vB	c�B	b�B	c�B	`�B	aHB	[�B	Y�B	VmB	S&B	PB	N�B	M�B	JXB	J�B	K^B	J�B	K�B	H�B	D�B	@OB	=B	?�B	=�B	9�B	7B	8RB	5tB	6FB	4B	1�B	.B	1'B	2�B	.IB	+B	6�B	+B	#nB	�B	'B	(�B	!�B	+B	�B	-CB	5tB	�B	FB	
rB	�B	4B	%B	B	DB	%B	(B	B�cB	oB�cB	 iB��B�.B��B�.B�cB��B��B	  B	�B	 �B��B�PB�DB�B�VB��B�rB�B�rB�fB��B�8B��B��B��B��B�TB�B�B�B�+B��B��B��B�MB��B�MB�vB�B�B�;B�B�vB�B�5B�]B�B�B�mB�B�B��BߤB�B��B��BݘB��B��B��B�QBרB��B��B�gB֡B��BҽBخB��B��BΥB��BרB�sB��B�dB�B�pB�BBуB�pBѷB�#BԕB͟BȴB��B�3B�IB�=B�BɆB�B�+B�$B��B��B�ByrB�B��B|PBg8Be,Bb�Be,BaBaBaHBb�Bk�Bj�Bc�Ba�Bj�Be`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�:B��B�B�B��B�NB�4B�-B�B�B�B�@B��B�B��B�sB�0B��B�B�B�B�hB�B��B��B��B��B��B	oB	�B	�B	~B	*0B	,�B	-�B	0UB	9�B	K�B	OBB	P�B	MB	N�B	Y1B	m�B	�B
N�B
��B
ªBpBB&�B>�B=<BP�BJ	Bh$B��B��B��B�B�ZB��B�)B�9B��B�MB�B�wB��B�B�BǮB��B��B�oB��B�BkBV�BN�BF�B8lB'RB
�qB
q�B
1�B
�B	��B	��B	��B	yrB	T�B	J�B	/ B		�B	?B	 �B	 �B�.B�B�XB�B�4B�`By�Bn�Bx8B��B�B��B�B��BɺB�B�pB��B�B��B��B�B	 �B	.B	�B	'�B	>wB	H�B	UgB	R�B	L�B	J�B	P.B	\�B	aB	`BB	b�B	oB	[�B	^�B	[�B	@OB	0B	�B	uB��B��B��B��B	 �B	�B	
rB	LdB	i*B	hXB	��B	hsB	��B	�B	��B	�6B	�AB	��B	��B	��B	�B	��B	�YB	��B	��B	�dB	��B	��B	�B	�B	�&B	�B	�aB	��B	ϫB	�xB	�B	�B	�NB	��B	רB	�B	�rB	�aB	��B	ĜB	�3B	�B	��B	��B	�B	� B	�mB	��B	�B
�B
�B
"B
NB
�B
�B
	lB
SB
9B
YB
tB
aB
-B
�B
�B
[B
B
;B
�B
GB
 �B
 OB
 iB
 B	�B	�wB	�B	�B	�B
;B	�qB	��B	��B	�B	��B	��B	�XB	�$B	�>B	��B	�8B	�XB	��B	��B	�B	�LB	�fB	��B	�2B	�B	��B	�`B	��B	�zB	�?B	��B	�GB	�'B	�B	��B	�hB	�aB	��B	�B	�-B	�B	�;B	��B	�B	�9B	�B	�B	�B	�B	�aB	�[B	�B	�B	�B	�AB	�B	��B	�B	�B	�B	� B	��B	�B	�B	�eB	��B	�B	�B	�6B	��B	�kB	�B	�eB	�>B	�B	�B	�eB	��B	��B	��B	� B	�:B	�B	��B	��B	�B	�B	�B	�_B	��B	�"B	�cB	��B	�B	�B	�=B	��B	��B	�CB	�)B	�CB	�B	�B	�B	��B	��B	�B	�6B	�WB	�=B	��B	��B	��B	� B	�B	��B	�B	�cB	�'B	��B	�UB	��B	��B	�B	�B	��B	��B	�3B	�B	�]B	�B	��B	�_B	�sB	�*B	�eB	�KB	�B	��B	��B	�$B	�DB	�B	�wB	�UB	�!B	�!B	�B	�[B	��B	�UB	��B	�iB	�oB	�oB	�B	�iB	�OB	�B	�B	�aB	��B	�B	�oB	�oB	��B	�B	�B	�|B	��B	�B	�aB	�GB	��B	�B	�B	��B	�aB	�GB	�B	�vB	�B	��B	�B	�B	�B	�B	�oB	�AB	��B	��B	��B	�B	�9B	��B	��B	�fB	��B	�rB	�$B	�XB	�XB	��B	�6B	�B	��B	��B	�B	�VB	��B	�]B	��B
 B
UB
 B
B
�B
�B
�B
�B
oB
�B
AB
uB
uB
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
YB
�B
zB
�B
�B
1B
�B
	�B
	�B
	lB
	�B
DB

�B
^B
xB
xB
xB
xB
�B
�B
�B
6B
jB
jB
�B
<B
�B
HB
�B
B
�B
2B
�B
B
$B
yB
�B
�B
EB
�B
�B
�B
sB
�B
�B
YB
EB
1B
EB
�B

B
9B
SB
�B
SB
$B
�B
�B
�B
$B
�B
�B
�B
KB
�B
1B
1B
1B
�B
QB
�B
�B
qB
B
�B
B
5B
;B
�B
;B
�B
�B
!-B
!HB
!-B
!HB
!�B
!�B
"B
"4B
"�B
"�B
#�B
$@B
$@B
#�B
$&B
$�B
$�B
$tB
$�B
%`B
%�B
&�B
&LB
&�B
'B
'mB
'RB
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)B
*0B
*eB
*�B
+B
,qB
,�B
,�B
-)B
-]B
-�B
-�B
.IB
./B
.cB
/5B
0�B
0!B
0oB
0�B
0�B
0�B
0�B
0�B
1vB
1vB
1�B
1�B
2|B
3B
4�B
4�B
4�B
4TB
4TB
3�B
3�B
3�B
4B
4�B
5ZB
5�B
5�B
6�B
7B
8B
8B
8B
8�B
8�B
9>B
:B
:�B
:xB
:�B
:�B
:�B
;�B
;�B
<PB
<jB
=B
=VB
>]B
>B
=�B
=�B
=qB
=qB
<�B
=�B
=<B
=VB
>B
?�B
@�B
@�B
@OB
@iB
@iB
@iB
@OB
@4B
@OB
@OB
@�B
@4B
@B
@4B
@4B
@4B
@4B
@4B
@4B
@�B
@iB
@4B
@iB
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
AoB
B[B
B�B
CB
C�B
D�B
D�B
EB
E�B
ESB
E9B
ESB
ESB
EmB
E�B
FB
F�B
FtB
F�B
G�B
G+B
GEB
GzB
G�B
H1B
HB
HKB
H�B
IB
H�B
H�B
H�B
H�B
I�B
J	B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K)B
K�B
K�B
K�B
L�B
MB
L�B
MPB
MjB
M�B
M�B
N"B
NpB
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
P�B
P}B
P�B
Q4B
R B
RB
SB
R�B
SuB
TFG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�TB�B��B�TB�NB�B�B�B�B�B�B�B�B�TB�B�&B�TB�B�TB�TB�&B�B��B�B��B�BB�B��B�vB�B�B�HB�B�HB�;B��B��B��B��B� B�,B�`B��B�,B�B�B��B��B�fB�
B�
B�
B�
B�
B�B�mB�B��B�2B��B�mB�B�8B��B�B�2B�>B�)B��B�KB�B�fB��B�B�"B�WB�B�B�WB��B�cB��B�B��B��B�B�oB�iB�B�B��B�vB��B�AB�B��B��B��B�|B�|B�B�B�B�B�TB�TB�B�B�B�B�|B�B�TB��B��B�%B��B�TB��B�MB�MB�MB�B�B��B�B��B�%B�%B�`B�fB��B�+B�ZB�`B�`B�`B��B�8B��B��B��B��B��B��B�B�VB��B	 4B	  B�cB��B��B	 4B	 �B	{B	B	�B	B	(B	(B	(B	�B	4B	B	oB	B	uB	SB	�B	SB	B	B	�B	)_B	&�B	'�B	)_B	+B	*0B	*�B	*0B	)_B	*eB	*�B	)�B	)�B	+B	(�B	,=B	+�B	*�B	,B	*�B	*0B	+6B	0!B	1'B	/B	49B	.�B	,=B	5B	3�B	9XB	9XB	9�B	:^B	;dB	EmB	NB	L0B	K�B	QNB	K^B	OvB	K^B	L0B	K�B	N�B	PB	NpB	P�B	MjB	L0B	K)B	T�B	J�B	GzB	IRB	GEB	@�B	@�B	?}B	E�B	YKB	OBB	@�B	B�B	H�B	S�B	R�B	V�B	]/B	aB	a|B	c�B	j�B	ncB	poB	q�B	w�B	}"B	�B	�1B	��B	�B	�B
�B
@�B
O�B
o�B
i�B
p�B
r�B
u�B
y�B
�eB
��B
�B
�aB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�)B
�B
�,B
��B
�AB
�]B�BB�B�BfB	7B�B�B�B(B�BuBhB�B�B4B�B=BBYB{B�B�B�B#B�B$B%�B.IB3hB3hB5�B6�B;0B=�B?HB?HB9�B:�B7�B-�B0�B1�B3�B1�B2�B8�B3�B:*BB'BA B>�BJ�BMBNpBR�BT�BB�B2�B)�B3�B8�B1�B<jB8�B9XB>BB:^BC�BE�BD�BA BB�BH�BI�Be�By>Bw2B�B�B��B�~B��B�7B��B��B�"B�=B��B�rB�fB��B�fB�JB��B�$B�B�CB�-B�B��B��B�LB�B��B�6B��B�IB��B�B��B��B��B��B�?B��B��B�jB��B�B��B�0B�CB��B�eB�B��B��B��B�!B��B�wB�B�?B��B� B�B�&B�B�B�B�]B�5B�)B��B�]B�AB�B�)B�B�)B��B�sB�QB�5B�B�B�B�&BoB� B��B�B�]B�mB�B�ZB��B��B�cB�B��B�B�PB;B��B��B  B��B�JB�`B��B�%B�B�B�]B��B�B��B�DB�QB�B�WB� B�yB��B�>B�B�vB�B��B��B�ZB�#B��B��B�QBƨB�NB��BȴB�$B�aB��B�3B��B��B�$B��B�B�0B��B�B�OB��B��B��B��B�@B� B�uB�JB�	B�7B�B�7B�_B��B��B�rB�_B�+B�fB��B��B��B��B�{B�B�bB�%B}�BqvBo�Bm]Bc�Bh>BbBgB_pB]/Bd�B[�BS�BR�BOvBPHBQ�BM�BN�BN�BMjBL�BMjBK�BK)BL�BL�BK�BJ�BL0BI�BJ�BI�BHKBGBH�BH�BE9BF�BE�BD�BFBD3BC�BD�BB[B?�B@OBCaB>�B<B;�B9$B9$B:*B9�B5?B5�B6zB4B5?B1�B4�B2�B2�B-�B-�B)�B*0B+B'�B%�B&B+kB#�B+�B!�B!bB!�BYB"4B�BxB�B$B_BB(BVB�BDB~B�B
�B
��B
�B
�B
�B
��B
�B
��B
�B
�EB
��B
ȴB
�aB
�&B
ޞB
�OB
�LB
��B
�kB
�=B
��B
{�B
~�B
w2B
{B
s�B
qvB
bNB
c�B
kB
`BB
X�B
TaB
QNB
K�B
QNB
J#B
E�B
F�B
@�B
;�B
2�B
/OB
/�B
$�B
$�B
�B
�B
CB
�B
�B
+B
�B
�B
�B
�B
�B
�B
�B
 �B
fB
uB	��B	�fB	�MB	�AB	�cB	� B	�;B	�DB	�KB	�2B	�B	�B	�TB	�5B	ޞB	�5B	��B	��B	��B	�EB	�WB	�gB	��B	��B	҉B	�B	�RB	�#B	�mB	ĜB	�OB	�qB	��B	��B	�B	�B	��B	��B	�tB	�FB	��B	�B	��B	�1B	��B	��B	��B	��B	~�B	{JB	|B	.B	.B	�MB	�B	��B	��B	��B	��B	��B	��B	��B	�GB	�B	�oB	��B	��B	�B	�lB	~�B	~�B	|PB	}�B	zxB	{�B	y>B	|B	xB	y�B	zDB	}VB	y>B	n�B	jKB	jKB	ffB	d�B	`vB	c�B	b�B	c�B	`�B	aHB	[�B	Y�B	VmB	S&B	PB	N�B	M�B	JXB	J�B	K^B	J�B	K�B	H�B	D�B	@OB	=B	?�B	=�B	9�B	7B	8RB	5tB	6FB	4B	1�B	.B	1'B	2�B	.IB	+B	6�B	+B	#nB	�B	'B	(�B	!�B	+B	�B	-CB	5tB	�B	FB	
rB	�B	4B	%B	B	DB	%B	(B	B�cB	oB�cB	 iB��B�.B��B�.B�cB��B��B	  B	�B	 �B��B�PB�DB�B�VB��B�rB�B�rB�fB��B�8B��B��B��B��B�TB�B�B�B�+B��B��B��B�MB��B�MB�vB�B�B�;B�B�vB�B�5B�]B�B�B�mB�B�B��BߤB�B��B��BݘB��B��B��B�QBרB��B��B�gB֡B��BҽBخB��B��BΥB��BרB�sB��B�dB�B�pB�BBуB�pBѷB�#BԕB͟BȴB��B�3B�IB�=B�BɆB�B�+B�$B��B��B�ByrB�B��B|PBg8Be,Bb�Be,BaBaBaHBb�Bk�Bj�Bc�Ba�Bj�Be`111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                    1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<H�r<3�.<=gQ<#�
<#�
<|�=<uA�<#�
<#�
<#�
<l9�<#�
<)�<%�N<#�
<#�
<9+<3�.<T|�<#�
<WlR<~Y<�t�<^��<#�
<#�
<#�
<2/�<#�
<#�
<#�
<#�
<#�
<�F�<��e<�)<�$w<s�Y<k�(<�OG<�wM<h��<|7�<k�(<�#<��T<#�
<#�
<#�
<9�F<|7�<^H<��X<�ud<��<�A�<g<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<BN�<*h�<J��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<;*M<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2018022211250920180222112509IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018030418042020180304180420QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018030418042020180304180420QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544220190521075442IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619241920230426192419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619241920230426192419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619241920230426192419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                