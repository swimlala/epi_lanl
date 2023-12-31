CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-01T20:28:23Z creation; 2021-03-26T17:01:00Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201201202823  20210326170210  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               :   :AA  AOAO7836_008777_058                 7836_008777_058                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�K��s��@�K��s��11  @�K����@�K����@<X��@<X���d����K�d����K11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@B�\@��\@�G�@�  @�G�A ��A\)A   A,(�A@  A_\)A�  A��A��A�  A�  AУ�A�Q�A�\)A�\)B�
B  B�B   B((�B0  B8  B@(�BG�
BO�BX  B`(�Bh(�Bp(�Bx  B�B��B�{B�  B��B�  B�  B�  B�  B��B�  B��B�B��B�  B�  B�{B�{B�  B��B�  B�  B��B�  B�  B��B�  B�{B�{B�{B��
B��C 
=C{C
=C��C  C	��C��C
=C{C{C��C��C��C  C  C�C�C!��C#��C&  C'�C*  C,{C-��C/��C1��C3�C5�HC7�C9��C;�C>  C@{CA��CC�CF  CH  CJ  CK��CM��CO�CQ�CS��CU��CW��CZ  C[��C]��C_��Ca��Cd  Cf
=Ch  Ci�Ck��Cm��Co�Cr  Ct  Cv  Cx
=Cz  C{��C}�C�C���C�  C�C�C���C���C�  C�C�  C���C�  C�  C���C�C�C�C�  C�  C���C���C�  C���C���C�  C�C�\C�  C���C�C�
=C�C�C���C���C���C���C�  C�C�C�C�C�C�C���C���C�C�C�  C�  C���C���C���C���C���C�  C�
=C�  C�C�C�C�C���C���C���C��C�
=C�C���C���C���C���C���C�C�  C�C�C�
=C���C���C�C�C�
=C���C���C�  C�  C���C�  C�  C�C�  C�  C�  C�  C���C�  C�
=C�  C�  C�C�C�
=C�C�C�C���C�  C�  C�C���C���C�  C���C���C���C���C�  C�C�
=C�C�  C�  C�  C�  C���C���C�D �D ��D  D��D�D}qD�D�D  D� D�qD��D�D}qD�qD}qD�qD}qD	�D	� D
�D
}qD  D� D�qD}qD�qDxRD  D��D  D��D  D��D�D}qD  D��DD� D  D�D�D��D�qD� D�D}qD  D� D�qD� D�D� D�D��D  D� D�qDz�D�qD� D�qD��D �D ��D!�D!� D"  D"� D#�D#}qD#�qD$� D$�qD%z�D%�RD&z�D&�qD'��D(D(� D(�qD)��D*  D*}qD+  D+}qD+�qD,}qD-  D-��D.�D.� D.�qD/� D0  D0� D1  D1� D1��D2}qD3�D3� D3�qD4� D5�D5� D6�D6��D7  D7� D8  D8��D9D9� D9��D:z�D:��D;� D<�D<� D<�qD=� D>�D>� D>�qD?}qD@  D@� DADA��DB  DB��DC�DC� DC�qDD}qDD�qDE� DF  DF� DG  DG��DHDH�DI�DI��DJ�DJ� DK�DK��DL  DL� DM�DM� DN�DN� DO  DO}qDP  DP� DQ�DQ��DR�DR�DS�DS� DT�DT� DT�qDU� DV  DV}qDW  DW� DXDX��DY�DY� DY�qDZ}qDZ�qD[��D\�D\z�D\�qD]� D^  D^}qD^�RD_xRD_�qD`� Da�Da��DbDb��Dc  Dc}qDc��Dd� De�De}qDe�qDf��Dg  Dg}qDh  Dh� Di�Di��Dj  Dj� Dj��Dk}qDl�Dl��Dm�Dm� Dm��DnxRDo  Do��Do�qDp� Dq�Dq��Dr�Dr�Ds  Ds�Dt�Dt�DuDu��Du�qDv}qDv��Dwz�Dx�Dx��Dx�qDy� Dz  Dz��D{  D{}qD|�D|� D}�D}� D}�qD~��D  D}qD�HD�@ D�~�D�� D�HD�B�D���D��HD�HD�AHD�� D��HD���D�@ D���D�� D��)D�>�D��HD��HD�HD�@ D�� D��HD��D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D���D�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D���D�D�  D�=qD�~�D��HD���D�>�D�� D�� D���D�=qD�� D�� D�  D�@ D�}qD��qD���D�@ D���D��HD���D�>�D�� D��HD�  D�@ D��HD��HD�HD�>�D�~�D��HD�HD�@ D�~�D��qD���D�>�D�� D�� D�  D�=qD�}qD���D���D�AHD���D��HD��qD�>�D��HD���D���D�@ D��HD���D�  D�@ D��HD��HD�  D�>�D�~�D��qD��qD�@ D��HD��HD���D�>�D��HD��HD�  D�AHD�� D��HD���D�=qD�~�D��HD�  D�>�D�� D��qD���D�AHD�~�D���D�HD�AHD���D��HD�HD�AHD��HD�D�HD�AHD���D�D��D�B�D��HD���D��)D�=qD�~�D���D�  D�@ D�� D��HD��D�@ D�� D��HD�  D�@ D�~�D��qD�  D�@ D�� D���D���D�@ D��HD�� D���D�=qD�� D��HD���D�>�D��HD��HD���D�=qD�}qD���D�HD�AHD��HD��HD�  D�@ D��HD�� D���D�@ D���D���D���D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD��HD�HD�@ D��HD���D���D�>�D�� D�� D���D�AHD�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�=qD�~�D�� D�  D�@ D�~�D��qD��qD�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�>�D�~�D��HD��D�@ D�~�D��HD�  D�=qD�}qD�� D�HD�@ D�~�Dľ�D�  D�>�D�~�D��HD��D�B�DƂ�D�D�  D�>�Dǀ DǾ�D��qD�@ DȂ�DȾ�D���D�@ D�~�D�� D��D�@ Dʀ Dʾ�D���D�>�D�~�D˽qD��qD�@ D́HD̾�D���D�>�D�~�DͽqD��qD�>�D�~�D�� D�  D�=qD�}qDϽqD��qD�<)DЀ D�� D�  D�AHDр D�� D�  D�>�D�~�D�� D�  D�AHDӁHDӾ�D���D�@ D�~�DԼ)D�  D�AHDՀ D��HD�  D�>�D�}qD�� D�HD�@ D׀ D�� D���D�<)D�~�D��HD�HD�AHDـ Dپ�D�  D�AHDڀ Dھ�D���D�@ DہHD۾�D�  D�AHD܁HD��HD�  D�@ D݀ D��HD�  D�>�Dހ D��HD�  D�=qD�}qD�� D�HD�@ D�}qDྸD���D�@ DႏD�� D�  D�>�D�HD�D�HD�@ D� D�� D�  D�B�D�HD侸D��qD�@ D�HD徸D��D�C�D悏D�� D���D�AHD� D羸D�  D�@ D� D��HD�HD�@ D�~�D�� D�  D�AHD�HD��HD�  D�>�D�~�D�D��D�AHD�HD쾸D�  D�B�D킏D��HD�HD�AHD� D�� D�  D�AHD�HD�� D���D�@ D�� D�D�  D�AHD� D��HD��D�AHD�HD�� D���D�=qD�|)D�qD��qD�<)D�}qD��HD�  D�>�D�� D��HD�HD�B�D�� D�� D�  D�>�D�}qD���D�  D�AHD���D��HD�  D�>�D�~�D�� D�  D�>�D�� D��HD�HD�#�>�G�?.{?k�?�z�?Ǯ?��H@�@�R@333@E�@W
=@n{@��
@�{@�
=@�  @��@���@��R@�=q@�33@�(�@�ff@�{@���A33A	��A\)Az�A��A{A"�\A(��A/\)A6ffA;�A@  AC�
AJ�HAP��AW
=A\��A`��Ae�Aj�HAp��AvffA{�A�  A�=qA�z�A��RA��A���A��A���A��
A�{A���A�z�A�\)A�=qA���A�\)A��A�(�A�\)A��\A�{A���A��
A��RA���A��
A�
=A�=qA�p�AУ�A�33A�p�A�  A��HA�{A�G�A�z�A�
=A陚A�(�A�\)A�=qA�A�Q�A��A�{B Q�BB
=Bz�B{B�
B	�B
ffB�B��B=qB�BG�B�RBQ�B��B�HB  BG�BffB  B��B
=B z�B!p�B"�\B#�B$��B&=qB'�B(��B*{B+33B,  B,��B-��B.ffB/\)B0Q�B1p�B2�\B3�B4z�B5G�B6{B7
=B7�
B8��B:=qB;\)B<z�B=��B>ffB?33B@(�BA�BB{BC�BD��BE�BF�RBG�BHz�BIp�BJ�RBLQ�BMp�BNffBO\)BP  BQp�BR�RBT(�BU�BU�BW
=BX  BYG�BZ�\B[�B\��B]��B^�\B_\)B`(�Ba�Bb=qBc\)Bd��Be��Bf�\Bg\)BhQ�Bi�Bj{Bj�HBk�Bl��BmBn�HBp  Bpz�BqG�Br{Br�RBs�
Bt��Bu�Bv�HBw�Bx(�Bx��Bz{B{
=B|  B}�B~{B~�HB�B�{B�z�B���B�p�B�  B�z�B��HB�33B���B��B�Q�B��RB��B��B�{B��\B�
=B�p�B��B�=qB��\B���B�G�B��B�  B�Q�B���B�
=B�\)B��
B�Q�B��RB�33B�B�=qB���B�
=B�\)B��
B�Q�B��RB�
=B��B��B�ffB��HB�\)B��
B�Q�B��HB�\)B��B�z�B�
=B���B�  B��\B�
=B���B�{B��\B���B�p�B�  B�z�B���B�p�B�  B�z�B��B���B�(�B��RB�G�B��
B�z�B��HB��B�{B��\B�
=B��B�(�B��RB�\)B��B�z�B�
=B��B�=qB��RB�G�B��
B�ffB���B��B�{B��RB�G�B��
B�Q�B��HB�p�B�{B���B�33B��
B�z�B�
=B��B�=qB��HB���B�=qB��RB�G�B��
B�ffB���B�p�B�  B��\B��B�B�ffB��B�B�ffB��B��B�ffB���BÅB�{Bģ�B�33B��
B�z�B��B�B�ffB�
=BɮB�ffB�
=BˮB�z�B�
=B͙�B�(�BθRB�G�B��
B�z�B�
=BѮB�Q�B���BӮB�Q�B��B�B�z�B�33B��B؏\B��BٮB�Q�B��HBۙ�B�Q�B���BݮB�ffB��B�B�Q�B���BᙚB�=qB��HB㙚B�Q�B��B��
B��B�G�B��B�\B�
=B�B�\B�G�B�(�B�RB�G�B�B��B�\)B�  B��\B��B��
B��B�p�B�  B�\B��B��B��RB�33B��B��\B�G�B�  B��\B��B�B���B�\)B��
B�ffB�33C 
=C Q�C �\C �CQ�C�RC  CG�C��C  CffC�C��CQ�CC�CffC��C��CffCC(�CffC��C{Cp�C�HC	�C	ffC	�RC
�C
�\C
�
C�C�C�CG�C�\C�
C=qC��C  CG�C�C�CQ�C�RC{CQ�C��C��CffC��C
=C\)CC(�Cp�C�RC�C�\C�
C{Cz�C��C=qCz�C�CQ�C��C�C=qC��C{C\)C��C
=Cz�CC
=Cz�C�HC33Cz�C��C=qC��C�HC33C��C  C\)C��C��C Q�C C!
=C!Q�C!C"�C"�\C"��C#�C#�C#�C$33C$�C$�C%\)C%�C%��C&Q�C&�RC'{C'ffC'�C(�C(z�C(C)�C)�C)�C*33C*p�C*�HC+=qC+�C+�C,G�C,��C,�HC-=qC-�RC-��C.=qC.�C/
=C/G�C/�C0{C0Q�C0�RC1(�C1p�C1�RC2(�C2�\C2�
C333C3��C3��C433C4��C5{C5Q�C5��C6
=C6z�C6�RC7{C7�\C7�
C8�C8�\C8��C9=qC9�\C:  C:Q�C:��C;
=C;z�C;�RC<�C<�\C<C=(�C=��C=�HC>G�C>��C>��C?p�C?�RC@
=C@p�C@�
CA{CA�CA�CB33CB�\CC  CCG�CC��CD{CD\)CD�RCE33CEp�CE��CF=qCF��CF�
CGG�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                 ?��@�\@B�\@��\@�G�@�  @�G�A ��A\)A   A,(�A@  A_\)A�  A��A��A�  A�  AУ�A�Q�A�\)A�\)B�
B  B�B   B((�B0  B8  B@(�BG�
BO�BX  B`(�Bh(�Bp(�Bx  B�B��B�{B�  B��B�  B�  B�  B�  B��B�  B��B�B��B�  B�  B�{B�{B�  B��B�  B�  B��B�  B�  B��B�  B�{B�{B�{B��
B��C 
=C{C
=C��C  C	��C��C
=C{C{C��C��C��C  C  C�C�C!��C#��C&  C'�C*  C,{C-��C/��C1��C3�C5�HC7�C9��C;�C>  C@{CA��CC�CF  CH  CJ  CK��CM��CO�CQ�CS��CU��CW��CZ  C[��C]��C_��Ca��Cd  Cf
=Ch  Ci�Ck��Cm��Co�Cr  Ct  Cv  Cx
=Cz  C{��C}�C�C���C�  C�C�C���C���C�  C�C�  C���C�  C�  C���C�C�C�C�  C�  C���C���C�  C���C���C�  C�C�\C�  C���C�C�
=C�C�C���C���C���C���C�  C�C�C�C�C�C�C���C���C�C�C�  C�  C���C���C���C���C���C�  C�
=C�  C�C�C�C�C���C���C���C��C�
=C�C���C���C���C���C���C�C�  C�C�C�
=C���C���C�C�C�
=C���C���C�  C�  C���C�  C�  C�C�  C�  C�  C�  C���C�  C�
=C�  C�  C�C�C�
=C�C�C�C���C�  C�  C�C���C���C�  C���C���C���C���C�  C�C�
=C�C�  C�  C�  C�  C���C���C�D �D ��D  D��D�D}qD�D�D  D� D�qD��D�D}qD�qD}qD�qD}qD	�D	� D
�D
}qD  D� D�qD}qD�qDxRD  D��D  D��D  D��D�D}qD  D��DD� D  D�D�D��D�qD� D�D}qD  D� D�qD� D�D� D�D��D  D� D�qDz�D�qD� D�qD��D �D ��D!�D!� D"  D"� D#�D#}qD#�qD$� D$�qD%z�D%�RD&z�D&�qD'��D(D(� D(�qD)��D*  D*}qD+  D+}qD+�qD,}qD-  D-��D.�D.� D.�qD/� D0  D0� D1  D1� D1��D2}qD3�D3� D3�qD4� D5�D5� D6�D6��D7  D7� D8  D8��D9D9� D9��D:z�D:��D;� D<�D<� D<�qD=� D>�D>� D>�qD?}qD@  D@� DADA��DB  DB��DC�DC� DC�qDD}qDD�qDE� DF  DF� DG  DG��DHDH�DI�DI��DJ�DJ� DK�DK��DL  DL� DM�DM� DN�DN� DO  DO}qDP  DP� DQ�DQ��DR�DR�DS�DS� DT�DT� DT�qDU� DV  DV}qDW  DW� DXDX��DY�DY� DY�qDZ}qDZ�qD[��D\�D\z�D\�qD]� D^  D^}qD^�RD_xRD_�qD`� Da�Da��DbDb��Dc  Dc}qDc��Dd� De�De}qDe�qDf��Dg  Dg}qDh  Dh� Di�Di��Dj  Dj� Dj��Dk}qDl�Dl��Dm�Dm� Dm��DnxRDo  Do��Do�qDp� Dq�Dq��Dr�Dr�Ds  Ds�Dt�Dt�DuDu��Du�qDv}qDv��Dwz�Dx�Dx��Dx�qDy� Dz  Dz��D{  D{}qD|�D|� D}�D}� D}�qD~��D  D}qD�HD�@ D�~�D�� D�HD�B�D���D��HD�HD�AHD�� D��HD���D�@ D���D�� D��)D�>�D��HD��HD�HD�@ D�� D��HD��D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D���D���D�>�D�� D��HD�HD�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D���D�D�  D�=qD�~�D��HD���D�>�D�� D�� D���D�=qD�� D�� D�  D�@ D�}qD��qD���D�@ D���D��HD���D�>�D�� D��HD�  D�@ D��HD��HD�HD�>�D�~�D��HD�HD�@ D�~�D��qD���D�>�D�� D�� D�  D�=qD�}qD���D���D�AHD���D��HD��qD�>�D��HD���D���D�@ D��HD���D�  D�@ D��HD��HD�  D�>�D�~�D��qD��qD�@ D��HD��HD���D�>�D��HD��HD�  D�AHD�� D��HD���D�=qD�~�D��HD�  D�>�D�� D��qD���D�AHD�~�D���D�HD�AHD���D��HD�HD�AHD��HD�D�HD�AHD���D�D��D�B�D��HD���D��)D�=qD�~�D���D�  D�@ D�� D��HD��D�@ D�� D��HD�  D�@ D�~�D��qD�  D�@ D�� D���D���D�@ D��HD�� D���D�=qD�� D��HD���D�>�D��HD��HD���D�=qD�}qD���D�HD�AHD��HD��HD�  D�@ D��HD�� D���D�@ D���D���D���D�@ D�� D�� D�  D�>�D�~�D���D���D�AHD��HD��HD�HD�@ D��HD���D���D�>�D�� D�� D���D�AHD�~�D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�  D�=qD�~�D�� D�  D�@ D�~�D��qD��qD�@ D�� D�� D�HD�AHD��HD��HD�HD�AHD�� D��qD���D�>�D�~�D��HD��D�@ D�~�D��HD�  D�=qD�}qD�� D�HD�@ D�~�Dľ�D�  D�>�D�~�D��HD��D�B�DƂ�D�D�  D�>�Dǀ DǾ�D��qD�@ DȂ�DȾ�D���D�@ D�~�D�� D��D�@ Dʀ Dʾ�D���D�>�D�~�D˽qD��qD�@ D́HD̾�D���D�>�D�~�DͽqD��qD�>�D�~�D�� D�  D�=qD�}qDϽqD��qD�<)DЀ D�� D�  D�AHDр D�� D�  D�>�D�~�D�� D�  D�AHDӁHDӾ�D���D�@ D�~�DԼ)D�  D�AHDՀ D��HD�  D�>�D�}qD�� D�HD�@ D׀ D�� D���D�<)D�~�D��HD�HD�AHDـ Dپ�D�  D�AHDڀ Dھ�D���D�@ DہHD۾�D�  D�AHD܁HD��HD�  D�@ D݀ D��HD�  D�>�Dހ D��HD�  D�=qD�}qD�� D�HD�@ D�}qDྸD���D�@ DႏD�� D�  D�>�D�HD�D�HD�@ D� D�� D�  D�B�D�HD侸D��qD�@ D�HD徸D��D�C�D悏D�� D���D�AHD� D羸D�  D�@ D� D��HD�HD�@ D�~�D�� D�  D�AHD�HD��HD�  D�>�D�~�D�D��D�AHD�HD쾸D�  D�B�D킏D��HD�HD�AHD� D�� D�  D�AHD�HD�� D���D�@ D�� D�D�  D�AHD� D��HD��D�AHD�HD�� D���D�=qD�|)D�qD��qD�<)D�}qD��HD�  D�>�D�� D��HD�HD�B�D�� D�� D�  D�>�D�}qD���D�  D�AHD���D��HD�  D�>�D�~�D�� D�  D�>�D�� D��HD�HG�O�>�G�?.{?k�?�z�?Ǯ?��H@�@�R@333@E�@W
=@n{@��
@�{@�
=@�  @��@���@��R@�=q@�33@�(�@�ff@�{@���A33A	��A\)Az�A��A{A"�\A(��A/\)A6ffA;�A@  AC�
AJ�HAP��AW
=A\��A`��Ae�Aj�HAp��AvffA{�A�  A�=qA�z�A��RA��A���A��A���A��
A�{A���A�z�A�\)A�=qA���A�\)A��A�(�A�\)A��\A�{A���A��
A��RA���A��
A�
=A�=qA�p�AУ�A�33A�p�A�  A��HA�{A�G�A�z�A�
=A陚A�(�A�\)A�=qA�A�Q�A��A�{B Q�BB
=Bz�B{B�
B	�B
ffB�B��B=qB�BG�B�RBQ�B��B�HB  BG�BffB  B��B
=B z�B!p�B"�\B#�B$��B&=qB'�B(��B*{B+33B,  B,��B-��B.ffB/\)B0Q�B1p�B2�\B3�B4z�B5G�B6{B7
=B7�
B8��B:=qB;\)B<z�B=��B>ffB?33B@(�BA�BB{BC�BD��BE�BF�RBG�BHz�BIp�BJ�RBLQ�BMp�BNffBO\)BP  BQp�BR�RBT(�BU�BU�BW
=BX  BYG�BZ�\B[�B\��B]��B^�\B_\)B`(�Ba�Bb=qBc\)Bd��Be��Bf�\Bg\)BhQ�Bi�Bj{Bj�HBk�Bl��BmBn�HBp  Bpz�BqG�Br{Br�RBs�
Bt��Bu�Bv�HBw�Bx(�Bx��Bz{B{
=B|  B}�B~{B~�HB�B�{B�z�B���B�p�B�  B�z�B��HB�33B���B��B�Q�B��RB��B��B�{B��\B�
=B�p�B��B�=qB��\B���B�G�B��B�  B�Q�B���B�
=B�\)B��
B�Q�B��RB�33B�B�=qB���B�
=B�\)B��
B�Q�B��RB�
=B��B��B�ffB��HB�\)B��
B�Q�B��HB�\)B��B�z�B�
=B���B�  B��\B�
=B���B�{B��\B���B�p�B�  B�z�B���B�p�B�  B�z�B��B���B�(�B��RB�G�B��
B�z�B��HB��B�{B��\B�
=B��B�(�B��RB�\)B��B�z�B�
=B��B�=qB��RB�G�B��
B�ffB���B��B�{B��RB�G�B��
B�Q�B��HB�p�B�{B���B�33B��
B�z�B�
=B��B�=qB��HB���B�=qB��RB�G�B��
B�ffB���B�p�B�  B��\B��B�B�ffB��B�B�ffB��B��B�ffB���BÅB�{Bģ�B�33B��
B�z�B��B�B�ffB�
=BɮB�ffB�
=BˮB�z�B�
=B͙�B�(�BθRB�G�B��
B�z�B�
=BѮB�Q�B���BӮB�Q�B��B�B�z�B�33B��B؏\B��BٮB�Q�B��HBۙ�B�Q�B���BݮB�ffB��B�B�Q�B���BᙚB�=qB��HB㙚B�Q�B��B��
B��B�G�B��B�\B�
=B�B�\B�G�B�(�B�RB�G�B�B��B�\)B�  B��\B��B��
B��B�p�B�  B�\B��B��B��RB�33B��B��\B�G�B�  B��\B��B�B���B�\)B��
B�ffB�33C 
=C Q�C �\C �CQ�C�RC  CG�C��C  CffC�C��CQ�CC�CffC��C��CffCC(�CffC��C{Cp�C�HC	�C	ffC	�RC
�C
�\C
�
C�C�C�CG�C�\C�
C=qC��C  CG�C�C�CQ�C�RC{CQ�C��C��CffC��C
=C\)CC(�Cp�C�RC�C�\C�
C{Cz�C��C=qCz�C�CQ�C��C�C=qC��C{C\)C��C
=Cz�CC
=Cz�C�HC33Cz�C��C=qC��C�HC33C��C  C\)C��C��C Q�C C!
=C!Q�C!C"�C"�\C"��C#�C#�C#�C$33C$�C$�C%\)C%�C%��C&Q�C&�RC'{C'ffC'�C(�C(z�C(C)�C)�C)�C*33C*p�C*�HC+=qC+�C+�C,G�C,��C,�HC-=qC-�RC-��C.=qC.�C/
=C/G�C/�C0{C0Q�C0�RC1(�C1p�C1�RC2(�C2�\C2�
C333C3��C3��C433C4��C5{C5Q�C5��C6
=C6z�C6�RC7{C7�\C7�
C8�C8�\C8��C9=qC9�\C:  C:Q�C:��C;
=C;z�C;�RC<�C<�\C<C=(�C=��C=�HC>G�C>��C>��C?p�C?�RC@
=C@p�C@�
CA{CA�CA�CB33CB�\CC  CCG�CC��CD{CD\)CD�RCE33CEp�CE��CF=qCF��CF�
CGG�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@Ȟ@�aG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��
A��A��#A��#A��/A��/A��;A��/A��/A��HA��;A��TA��TA��HA��;A��#A��#A��A��;A��;A��HA��/A���A�x�A���A���A�1'A�;dA�^5A���A��A��A�A�I�A�XA��A�ĜA��A� �A��/A�dZA�|�A�VA��#A��!A�\)A��/A�`BA�A�=qA��A��\A�dZA��yA���A�  A�r�A���A��A�v�A��A�bA�`BA�/A�l�A��A�S�A�ƨA�l�A�"�A��A���A�1'A���A�I�A���A~-A}�A}��A}x�A}K�A}&�A|��A|z�A{��A{?}AzĜAz^5Ay�hAxbAv��AvjAt��Aq`BAo�FAmAlĜAk��Aj�AihsAh�AhI�Ag��Ae�;Ac��Ac33Ab��Ab��Ab5?A`�HA`jA`1A^��A^�A^�RA^ �A]K�A\v�A[�hA[&�AZ��AZE�AZ(�AY��AY�AY��AYG�AW�
AV�\AS��ARVAQ�
AO�;AM"�AKt�AJ��AI��AH(�AGK�AE�;AD�RADv�AD-AC�FAC`BABjAA;dA@�A@9XA?�^A?/A>�A=&�A;��A:ZA9�A8��A7��A6�jA5�
A5A4JA3�7A2��A2�A1C�A0(�A.ȴA-�
A-S�A,��A,��A,z�A+�;A*��A)�A)��A)l�A(��A'��A&��A%�wA%A$�/A$�9A$�+A#�
A"�yA!�PA!%A �\A�wA�hAt�A+AoAĜA=qA~�A�A-AdZAC�AVA�\A{AAK�A�!A��A9XA\)A7LAƨA�A�A��A�#A
~�A	�A	��A	�wA	p�A	
=A�AI�A��A�AI�A�Ar�A{A��AhsAn�@�ƨ@�Z@�|�@�o@�^5@�Z@��\@�{@��@�@�
=@�-@��@��D@�F@�O�@�r�@�  @�n�@��/@��;@�-@��m@�`B@�C�@��y@ް!@�n�@�/@���@�l�@�@�$�@��T@ٲ-@١�@�G�@�ƨ@��@��@ԃ@�Q�@�I�@�9X@�b@�\)@У�@�b@���@�t�@�=q@�G�@�r�@��@�;d@�V@�%@�z�@Ǖ�@��H@Ɨ�@�V@�5?@�7L@���@�A�@�;d@���@�$�@���@�9X@��@��T@��@�I�@�1@���@�;d@�M�@�hs@��@�z�@�1'@��@�+@��@�1@�;d@�n�@�G�@��@���@��@��R@�-@���@��u@�  @���@�\)@�K�@���@���@��@�%@��P@��@��D@�1@�33@�{@��/@� �@���@�S�@���@�J@���@��@���@��j@���@��@�r�@�(�@�+@���@��\@�v�@��7@��@��u@�j@�bN@�1'@��w@�S�@�+@�$�@�7L@���@���@���@��@�z�@�bN@�Q�@�9X@��@��@�5?@��/@��@�(�@��@�l�@�\)@�;d@��y@��+@�-@���@���@��@���@��@�Z@�A�@�1'@�1'@��@��H@�~�@�J@��#@���@��@���@��D@�I�@�1'@���@��
@���@�C�@���@�@��-@�X@���@�r�@��;@��@���@��P@�l�@�K�@��@���@�@��@�O�@��@�V@���@��/@���@�1'@��@l�@~5?@}�@|9X@{�
@{t�@z��@zJ@y��@y�^@yG�@x��@x �@wl�@v��@vV@u�@u@u?}@u/@u�@t�@t�D@s�F@st�@so@r�\@q��@qx�@q7L@q&�@qhs@qx�@q7L@q&�@q&�@p�`@o�;@o;d@n5?@m��@m�h@m�@m`B@mO�@m?}@m?}@m?}@m/@m/@m/@l�@lZ@l9X@kƨ@kt�@kS�@kC�@k@j�H@j��@i�^@i�@i7L@i7L@h�u@hr�@h �@g��@g�@g|�@g;d@f�+@f5?@f$�@f{@e�T@e�-@d�j@d9X@cƨ@c"�@b��@b^5@b-@b-@bJ@a�^@ax�@a7L@a%@`�`@`�@`Q�@`  @_+@_
=@]�h@]/@]V@\�/@\�@\Z@\(�@\1@[t�@[S�@["�@Z�@Z��@Z=q@Y�@Y��@Y&�@X�@XQ�@X1'@X1'@Xb@W�@W�w@WK�@V�@V��@Vv�@Vff@VV@V$�@U�T@U��@Up�@U?}@U�@T��@T�/@Tz�@T�@T1@S�m@S�
@S�F@St�@St�@SdZ@SS�@So@R��@R�\@R^5@Q��@Qx�@QG�@Q�@P�`@P�9@P�u@O��@Ol�@OK�@O+@N�@N��@M��@M�h@MO�@M/@MV@L��@L��@L�/@L9X@K��@K�m@Kƨ@K��@K�@K�@Kt�@KdZ@KS�@J��@JM�@JJ@IG�@H�u@G��@G�P@Gl�@GK�@GK�@G+@G
=@F�+@F{@E�T@E��@Ep�@E`B@E`B@E`B@EV@D(�@CdZ@B��@Bn�@BJ@A�#@A�^@A�^@Ax�@AX@AG�@A�@A%@@��@@Ĝ@@r�@@bN@@Q�@@1'@?�P@?;d@?�@?
=@>�+@>{@=��@=?}@<�j@<1@;S�@:�H@:��@:��@:�\@:n�@:M�@:=q@:�@9��@9X@8�@8 �@8  @7�@7�;@7��@7��@7�@7��@7l�@7\)@7\)@7
=@6ȴ@6��@6V@5�T@5��@5�T@5�@4��@4�j@4�@4�@4��@4�D@4j@4Z@4I�@49X@3��@3��@3t�@333@3"�@3o@2�H@2��@2n�@2M�@2M�@2=q@2M�@2M�@2=q@2=q@2-@2J@1��@1�#@1�7@1hs@0��@/K�@.{@-`B@-/@-/@-V@,��@,�@,��@,��@,�j@,�D@,z�@,�@+dZ@+o@*�@*��@*�\@*J@)�#@)�^@)��@)��@)��@)�7@)7L@(�u@(bN@(Q�@(Q�@'�;@'�w@';d@&ȴ@&�R@&�+@&v�@&ff@&5?@%@%p�@$z�@#t�@#dZ@#dZ@#dZ@#dZ@#dZ@#C�@#C�@#C�@#C�@#S�@#S�@#S�@#dZ@#dZ@#C�@#"�@"�@"�!@"�@!��@!X@!7L@!&�@!�@!%@ �`@ �9@ ��@ �u@ ��@ ��@ ��@ �u@ �@ A�@  �@�@+@�-@�@��@�@��@��@9X@(�@��@��@�m@�m@�
@�F@��@��@��@��@��@��@��@��@��@��@��@��@�@t�@"�@�!@�^@%@�`@��@��@A�@��@l�@l�@K�@+@
=@��@�@��@ff@5?@$�@$�@$�@$�@$�@{@{@@@�@�T@�T@�T@�T@�T@�T@�T@@��@�@`B@�@�/@��@�j@�D@z�@j@Z@I�@(�@1@��@C�@@�!@n�@=q@=q@-@��@��@G�@�`@Q�@A�@��@��@�@ff@V@$�@$�@$�@{@@�@�T@@�@p�@O�@O�@O�@�@�@V@V@��@��@�@�j@j@��@��@C�@"�@@@@
��@
��@
�\@
�@	��@	��@	x�@	x�@	x�@	x�@	x�@	hs@	X@	7L@�`@Ĝ@�9@�u@r�@Q�@�@�@;d@
=@�y@ȴ@��@�+@E�@E�@5?@$�@{@{@��@`B@�@��@I�@�@�@1@1@��@��@�m@�m@�mA��FA��wA��wA�A�A�ƨA���A�ƨA���A���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A��
A��A��
A���A���A���A��A��A��A��A��
A���A��
A��
A��#A��#A��#A��A���A���A���A��
A��#A��#A��A��A��
A��
A��A��#A��#A��/A��/A��#A��
A��A��A��#A��/A��#A��#A��#A��#A��/A��/A��;A��;A��/A��/A��/A��#A��/A��;A��HA��TA��HA��/A��;A��/A��#A��#A��/A��;A��;A��/A��/A��;A��;A��;A��HA��TA��TA��TA��HA��/A��#A��/A��;A��HA��TA��`A��`A��`A��TA��HA��HA��HA��TA��`A��`A��`A��TA��TA��TA��TA��HA��TA��HA��TA��TA��TA��`A��/A��#A��#A��;A��HA��TA��HA��/A��#A��A��#A��#A��/A��/A��A��
A��#A��/A��#A��/A��/A��#A���A��A��A��;A��/A��
A���A��
A��A��A��A��#A��/A��;A��;A��;A��HA��HA��HA��;A��/A��;A��;A��HA��TA��TA��HA��/A��/A��;A��HA��TA��HA��HA��;A��#A��#A��/A��/A��;A��;A��;A��/A��#A��/A��/A��;A��HA��HA��#A��#A���A�ĜA��wA��-A���A��uA���A���A���A��A��A�~�A�p�A�ZA�S�A�5?A�"�A���A�ĜA���A�5?A��A��A�jA��#A�Q�A�jA��hA��wA�K�A�K�A��A�%A��mA��A��A�v�A��A��9A���A�p�A�-A��A��wA�\)A�C�A�-A��A�A�A�  A���A���A��`A��!A�M�A��wA�?}A��A�
=A�/A��A��!A�bNA�K�A�E�A�1'A�bA���A��`A���A��wA���A���A���A��hA��PA�x�A�ffA�M�A�C�A�+A��A��A�  A�ȴA��hA�n�A�S�A�I�A�C�A�?}A�-A�$�A�bA���A���A��+A�VA��A�A�A���A���A��\A�~�A�dZA�dZA�M�A�&�A��A��A��/A���A�Q�A��`A�l�A�"�A�  A��7A�r�A�\)A�G�A�S�A��A�oA�bA�JA�%A���A��yA��
A��
A�ȴA��RA��!A���A��7A�jA�/A���A��#A�A��A���A�l�A�-A�ƨA���A�I�A�
=A��A���A��A�ffA�oA��A�ȴA�dZA�;dA�VA��A��A��hA�v�A�G�A��A��uA�dZA�5?A��A�%A���A��A��A��/A��^A���A��uA�t�A�dZA�O�A�A�A�(�A�A��;A�A���A���A��uA��hA�|�A�=qA� �A���A��
A�ƨA��9A��A���A���A���A��uA��PA��PA��\A��\A��7A��A�v�A�l�A�ffA�7LA��A�bA���A��A��/A�ȴA���A�jA�K�A�A��+A�VA��/A�M�A�oA��A��
A���A��^A���A��+A�x�A�K�A�+A�
=A���A��A���A��A�A�ZA��!A�p�A�VA�C�A�&�A���A��^A�r�A�O�A�
=A���A��yA��;A���A���A���A���A�x�A�ZA�-A�
=A��FA�5?A��TA��A�S�A� �A��A���A�bNA�A�A��A���A�ȴA���A��DA�t�A�dZA�33A��A�JA���A��A��;A�A���A��hA�hsA�E�A�9XA�JA�  A��yA���A��RA���A��PA�r�A�p�A�l�A�l�A�hsA�VA�G�A�9XA�(�A�oA��HA��FA��DA�hsA��A���A�7LA���A���A���A��A�p�A�ffA�S�A�;dA�-A�"�A�{A�1A���A��`A���A��A��PA�v�A�ffA�O�A�7LA�1'A�(�A�JA��/A�5?AXAA~ȴA~Q�A~JA~  A~(�A~5?A~$�A}��A}�A}�mA}�mA}�-A}��A}��A}��A}�hA}��A}��A}�7A}�A}|�A}t�A}`BA}p�A}G�A}S�A}\)A}/A}&�A}+A}&�A}"�A}+A}+A}&�A}�A}
=A|��A|�A|�+A|�A|��A|z�A|z�A|^5A|ffA|I�A| �A|1A{�A{ƨA{��A{�A{dZA{+A{"�A{�A{%Az�yAz�Az�!Az�Azz�AzjAzbNAzZAzM�AzQ�AzI�Az1AyAyp�Ay�Ax�RAxz�Ax^5Ax-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                 A���A�ƨA���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��
A��A��#A��#A��/A��/A��;A��/A��/A��HA��;A��TA��TA��HA��;A��#A��#A��A��;A��;A��HA��/A���A�x�A���A���A�1'A�;dA�^5A���A��A��A�A�I�A�XA��A�ĜA��A� �A��/A�dZA�|�A�VA��#A��!A�\)A��/A�`BA�A�=qA��A��\A�dZA��yA���A�  A�r�A���A��A�v�A��A�bA�`BA�/A�l�A��A�S�A�ƨA�l�A�"�A��A���A�1'A���A�I�A���A~-A}�A}��A}x�A}K�A}&�A|��A|z�A{��A{?}AzĜAz^5Ay�hAxbAv��AvjAt��Aq`BAo�FAmAlĜAk��Aj�AihsAh�AhI�Ag��Ae�;Ac��Ac33Ab��Ab��Ab5?A`�HA`jA`1A^��A^�A^�RA^ �A]K�A\v�A[�hA[&�AZ��AZE�AZ(�AY��AY�AY��AYG�AW�
AV�\AS��ARVAQ�
AO�;AM"�AKt�AJ��AI��AH(�AGK�AE�;AD�RADv�AD-AC�FAC`BABjAA;dA@�A@9XA?�^A?/A>�A=&�A;��A:ZA9�A8��A7��A6�jA5�
A5A4JA3�7A2��A2�A1C�A0(�A.ȴA-�
A-S�A,��A,��A,z�A+�;A*��A)�A)��A)l�A(��A'��A&��A%�wA%A$�/A$�9A$�+A#�
A"�yA!�PA!%A �\A�wA�hAt�A+AoAĜA=qA~�A�A-AdZAC�AVA�\A{AAK�A�!A��A9XA\)A7LAƨA�A�A��A�#A
~�A	�A	��A	�wA	p�A	
=A�AI�A��A�AI�A�Ar�A{A��AhsAn�@�ƨ@�Z@�|�@�o@�^5@�Z@��\@�{@��@�@�
=@�-@��@��D@�F@�O�@�r�@�  @�n�@��/@��;@�-@��m@�`B@�C�@��y@ް!@�n�@�/@���@�l�@�@�$�@��T@ٲ-@١�@�G�@�ƨ@��@��@ԃ@�Q�@�I�@�9X@�b@�\)@У�@�b@���@�t�@�=q@�G�@�r�@��@�;d@�V@�%@�z�@Ǖ�@��H@Ɨ�@�V@�5?@�7L@���@�A�@�;d@���@�$�@���@�9X@��@��T@��@�I�@�1@���@�;d@�M�@�hs@��@�z�@�1'@��@�+@��@�1@�;d@�n�@�G�@��@���@��@��R@�-@���@��u@�  @���@�\)@�K�@���@���@��@�%@��P@��@��D@�1@�33@�{@��/@� �@���@�S�@���@�J@���@��@���@��j@���@��@�r�@�(�@�+@���@��\@�v�@��7@��@��u@�j@�bN@�1'@��w@�S�@�+@�$�@�7L@���@���@���@��@�z�@�bN@�Q�@�9X@��@��@�5?@��/@��@�(�@��@�l�@�\)@�;d@��y@��+@�-@���@���@��@���@��@�Z@�A�@�1'@�1'@��@��H@�~�@�J@��#@���@��@���@��D@�I�@�1'@���@��
@���@�C�@���@�@��-@�X@���@�r�@��;@��@���@��P@�l�@�K�@��@���@�@��@�O�@��@�V@���@��/@���@�1'@��@l�@~5?@}�@|9X@{�
@{t�@z��@zJ@y��@y�^@yG�@x��@x �@wl�@v��@vV@u�@u@u?}@u/@u�@t�@t�D@s�F@st�@so@r�\@q��@qx�@q7L@q&�@qhs@qx�@q7L@q&�@q&�@p�`@o�;@o;d@n5?@m��@m�h@m�@m`B@mO�@m?}@m?}@m?}@m/@m/@m/@l�@lZ@l9X@kƨ@kt�@kS�@kC�@k@j�H@j��@i�^@i�@i7L@i7L@h�u@hr�@h �@g��@g�@g|�@g;d@f�+@f5?@f$�@f{@e�T@e�-@d�j@d9X@cƨ@c"�@b��@b^5@b-@b-@bJ@a�^@ax�@a7L@a%@`�`@`�@`Q�@`  @_+@_
=@]�h@]/@]V@\�/@\�@\Z@\(�@\1@[t�@[S�@["�@Z�@Z��@Z=q@Y�@Y��@Y&�@X�@XQ�@X1'@X1'@Xb@W�@W�w@WK�@V�@V��@Vv�@Vff@VV@V$�@U�T@U��@Up�@U?}@U�@T��@T�/@Tz�@T�@T1@S�m@S�
@S�F@St�@St�@SdZ@SS�@So@R��@R�\@R^5@Q��@Qx�@QG�@Q�@P�`@P�9@P�u@O��@Ol�@OK�@O+@N�@N��@M��@M�h@MO�@M/@MV@L��@L��@L�/@L9X@K��@K�m@Kƨ@K��@K�@K�@Kt�@KdZ@KS�@J��@JM�@JJ@IG�@H�u@G��@G�P@Gl�@GK�@GK�@G+@G
=@F�+@F{@E�T@E��@Ep�@E`B@E`B@E`B@EV@D(�@CdZ@B��@Bn�@BJ@A�#@A�^@A�^@Ax�@AX@AG�@A�@A%@@��@@Ĝ@@r�@@bN@@Q�@@1'@?�P@?;d@?�@?
=@>�+@>{@=��@=?}@<�j@<1@;S�@:�H@:��@:��@:�\@:n�@:M�@:=q@:�@9��@9X@8�@8 �@8  @7�@7�;@7��@7��@7�@7��@7l�@7\)@7\)@7
=@6ȴ@6��@6V@5�T@5��@5�T@5�@4��@4�j@4�@4�@4��@4�D@4j@4Z@4I�@49X@3��@3��@3t�@333@3"�@3o@2�H@2��@2n�@2M�@2M�@2=q@2M�@2M�@2=q@2=q@2-@2J@1��@1�#@1�7@1hs@0��@/K�@.{@-`B@-/@-/@-V@,��@,�@,��@,��@,�j@,�D@,z�@,�@+dZ@+o@*�@*��@*�\@*J@)�#@)�^@)��@)��@)��@)�7@)7L@(�u@(bN@(Q�@(Q�@'�;@'�w@';d@&ȴ@&�R@&�+@&v�@&ff@&5?@%@%p�@$z�@#t�@#dZ@#dZ@#dZ@#dZ@#dZ@#C�@#C�@#C�@#C�@#S�@#S�@#S�@#dZ@#dZ@#C�@#"�@"�@"�!@"�@!��@!X@!7L@!&�@!�@!%@ �`@ �9@ ��@ �u@ ��@ ��@ ��@ �u@ �@ A�@  �@�@+@�-@�@��@�@��@��@9X@(�@��@��@�m@�m@�
@�F@��@��@��@��@��@��@��@��@��@��@��@��@�@t�@"�@�!@�^@%@�`@��@��@A�@��@l�@l�@K�@+@
=@��@�@��@ff@5?@$�@$�@$�@$�@$�@{@{@@@�@�T@�T@�T@�T@�T@�T@�T@@��@�@`B@�@�/@��@�j@�D@z�@j@Z@I�@(�@1@��@C�@@�!@n�@=q@=q@-@��@��@G�@�`@Q�@A�@��@��@�@ff@V@$�@$�@$�@{@@�@�T@@�@p�@O�@O�@O�@�@�@V@V@��@��@�@�j@j@��@��@C�@"�@@@@
��@
��@
�\@
�@	��@	��@	x�@	x�@	x�@	x�@	x�@	hs@	X@	7L@�`@Ĝ@�9@�u@r�@Q�@�@�@;d@
=@�y@ȴ@��@�+@E�@E�@5?@$�@{@{@��@`B@�@��@I�@�@�@1@1@��@��@�m@�mG�O�A��FA��wA��wA�A�A�ƨA���A�ƨA���A���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A��
A��A��
A���A���A���A��A��A��A��A��
A���A��
A��
A��#A��#A��#A��A���A���A���A��
A��#A��#A��A��A��
A��
A��A��#A��#A��/A��/A��#A��
A��A��A��#A��/A��#A��#A��#A��#A��/A��/A��;A��;A��/A��/A��/A��#A��/A��;A��HA��TA��HA��/A��;A��/A��#A��#A��/A��;A��;A��/A��/A��;A��;A��;A��HA��TA��TA��TA��HA��/A��#A��/A��;A��HA��TA��`A��`A��`A��TA��HA��HA��HA��TA��`A��`A��`A��TA��TA��TA��TA��HA��TA��HA��TA��TA��TA��`A��/A��#A��#A��;A��HA��TA��HA��/A��#A��A��#A��#A��/A��/A��A��
A��#A��/A��#A��/A��/A��#A���A��A��A��;A��/A��
A���A��
A��A��A��A��#A��/A��;A��;A��;A��HA��HA��HA��;A��/A��;A��;A��HA��TA��TA��HA��/A��/A��;A��HA��TA��HA��HA��;A��#A��#A��/A��/A��;A��;A��;A��/A��#A��/A��/A��;A��HA��HA��#A��#A���A�ĜA��wA��-A���A��uA���A���A���A��A��A�~�A�p�A�ZA�S�A�5?A�"�A���A�ĜA���A�5?A��A��A�jA��#A�Q�A�jA��hA��wA�K�A�K�A��A�%A��mA��A��A�v�A��A��9A���A�p�A�-A��A��wA�\)A�C�A�-A��A�A�A�  A���A���A��`A��!A�M�A��wA�?}A��A�
=A�/A��A��!A�bNA�K�A�E�A�1'A�bA���A��`A���A��wA���A���A���A��hA��PA�x�A�ffA�M�A�C�A�+A��A��A�  A�ȴA��hA�n�A�S�A�I�A�C�A�?}A�-A�$�A�bA���A���A��+A�VA��A�A�A���A���A��\A�~�A�dZA�dZA�M�A�&�A��A��A��/A���A�Q�A��`A�l�A�"�A�  A��7A�r�A�\)A�G�A�S�A��A�oA�bA�JA�%A���A��yA��
A��
A�ȴA��RA��!A���A��7A�jA�/A���A��#A�A��A���A�l�A�-A�ƨA���A�I�A�
=A��A���A��A�ffA�oA��A�ȴA�dZA�;dA�VA��A��A��hA�v�A�G�A��A��uA�dZA�5?A��A�%A���A��A��A��/A��^A���A��uA�t�A�dZA�O�A�A�A�(�A�A��;A�A���A���A��uA��hA�|�A�=qA� �A���A��
A�ƨA��9A��A���A���A���A��uA��PA��PA��\A��\A��7A��A�v�A�l�A�ffA�7LA��A�bA���A��A��/A�ȴA���A�jA�K�A�A��+A�VA��/A�M�A�oA��A��
A���A��^A���A��+A�x�A�K�A�+A�
=A���A��A���A��A�A�ZA��!A�p�A�VA�C�A�&�A���A��^A�r�A�O�A�
=A���A��yA��;A���A���A���A���A�x�A�ZA�-A�
=A��FA�5?A��TA��A�S�A� �A��A���A�bNA�A�A��A���A�ȴA���A��DA�t�A�dZA�33A��A�JA���A��A��;A�A���A��hA�hsA�E�A�9XA�JA�  A��yA���A��RA���A��PA�r�A�p�A�l�A�l�A�hsA�VA�G�A�9XA�(�A�oA��HA��FA��DA�hsA��A���A�7LA���A���A���A��A�p�A�ffA�S�A�;dA�-A�"�A�{A�1A���A��`A���A��A��PA�v�A�ffA�O�A�7LA�1'A�(�A�JA��/A�5?AXAA~ȴA~Q�A~JA~  A~(�A~5?A~$�A}��A}�A}�mA}�mA}�-A}��A}��A}��A}�hA}��A}��A}�7A}�A}|�A}t�A}`BA}p�A}G�A}S�A}\)A}/A}&�A}+A}&�A}"�A}+A}+A}&�A}�A}
=A|��A|�A|�+A|�A|��A|z�A|z�A|^5A|ffA|I�A| �A|1A{�A{ƨA{��A{�A{dZA{+A{"�A{�A{%Az�yAz�Az�!Az�Azz�AzjAzbNAzZAzM�AzQ�AzI�Az1AyAyp�Ay�Ax�RAxz�Ax^5Ax-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BoBuB�B�B�B�B�B�B�BoB;BB;BB �B iB iB  B��B�cB�.B��B�]B�]B��B��B�VB��B��B��B�PB�JB�xB��B�>B�lB��B�TB�5B�B�XB�+BZQB>�B9XB,�BBBB
�B
�B�fB��B��B�QB�gBѷBɆB�jB�OB�'B��B�YB.Bv�Bp�Be`B`vB]�BQ�BL�B1�B$@BeB��B�B�B�mB�}B�0B�xB��B�B��By	Bs�Bo�BW�BM6BGzB;�B8BuBBPB�B
rBfB1BB  B��B��B�B��B��BخBбB�6B��B��B�4B��B��B�B�B}�By>Bt�Bl"B^BZ�BYBW
BVBOBBJ�BI�BD3BA�BA B>BB8�B6B0�B-�B,�B($B'�B&�B$@B"�B!bB�B�B�B
��B
�AB
�"B
�jB
��B
�pB
�0B
ĜB
�wB
�^B
�hB
��B
��B
�qB
��B
��B
��B
��B
�B
��B
�{B
�B
�(B
��B
�B
xlB
v�B
uZB
o B
k�B
ffB
b�B
]dB
YB
U�B
Q�B
OBB
HKB
B�B
?}B
=<B
<B
9�B
8�B
4�B
0�B
-B
,B
)�B
$tB
"�B
�B
7B
�B
�B
�B
�B
�B
�B
fB
�B
B
�B
�B
 4B	��B	��B	�>B	�	B	��B	�B	�KB	�B	�
B	�
B	�,B	�B	��B	ݘB	��B	�mB	�B	�B	��B	ŢB	B	� B	�HB	�jB	�$B	�B	��B	��B	�B	��B	�hB	��B	�OB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�@B	��B	��B	��B	��B	��B	��B	�=B	�DB	��B	��B	��B	��B	��B	�AB	��B	�B	��B	�B	~]B	}�B	}"B	~�B	}�B	|�B	}�B	|�B	|B	{B	z�B	zB	~]B	{�B	z�B	{B	zxB	z�B	zB	x�B	{�B	|B	y�B	yrB	{B	{�B	zxB	z�B	zxB	z�B	{B	|B	{B	}VB	|�B	}VB	|�B	|PB	~�B	|PB	cB	}�B	.B	.B	.B	��B	�B	�SB	�B	�YB	��B	�YB	��B	�fB	�fB	��B	��B	��B	��B	�B	�PB	�\B	��B	�oB	��B	��B	��B	�B	�~B	�B	�!B	�:B	��B	�B	�zB	�B	��B	�LB	��B	�0B	��B	�aB	��B	��B	�0B	�B	�-B	ŢB	�EB	�B	��B	�B	ΥB	�NB	� B	�TB	҉B	ҽB	ҽB	ӏB	�KB	چB	�QB	ںB	ߤB	��B	�TB	�B	�TB	�B	�B	�B	�sB	�oB	�B	��B	�`B	��B	�lB	�8B	��B	�	B	�	B	�B
  B
MB
�B
~B
�B
:B
�B
{B
�B
�B
�B
	B
B
B
VB
$B
&LB
'�B
'�B
($B
'�B
-B
1'B
3�B
7LB
8B
:*B
>�B
@OB
B�B
D3B
EB
FtB
F�B
HB
IB
NB
R B
T,B
V�B
YKB
]dB
a�B
c B
c�B
c�B
d�B
e`B
h
B
j�B
o�B
tTB
u�B
wfB
w�B
xlB
y	B
{B
~]B
��B
�uB
��B
��B
�\B
� B
�oB
��B
��B
��B
��B
�B
�~B
�\B
�tB
��B
�B
�wB
�IB
��B
�B
��B
�OB
��B
��B
�nB
�zB
��B
�0B
�wB
�}B
� B
�aB
�mB
ŢB
�EB
�#B
�XB
��B
��B
�dB
�B
�<B
�pB
��B
�BB
ϫB
��B
ϫB
��B
�B
�B
�&B
��B
�?B
�EB
�KB
�KB
�B
�B
�B
��B
�B
�B
�B
�B
�B
��B
�B
��B
��B
�vB
�MB
�2B
�8B
�8B
�lB
��B
�B
��B �B�BB�B�B�B�B+B�B	7B
=B
�BB~BB"B�B.B@B�BFB�B�B$B+BYB�B�B�BIB�B�B�B 'B!�B$B%B%zB%zB&B&B'B(XB)�B+B+B+6B+�B-B/B1'B1�B2-B2aB2�B33B4�B6�B7�B8�B8�B8�B8�B8�B8�B9$B:*B;0B;dB;�B=<B>�B>�B>�B?B?HB?�BA�BA�BB'BB[BB�BCaBEBF�BHBHKBH�BH�BHKBH�BJ�BK^BK^BK�BL0BLdBL�BLdBL�BL�BNBNpBNpBO�BQBR�BR�BS�BS�BS�BS�BS�BU�BV9BV�BX�BY�BZBZBZB[#B\�B^�B_pB`Ba|Ba�BbNBbNBcTBcTBc�Bc�Bd&Bd�Bd�Be�Be�Be�Be�Bg8BgmBg�Bg�BiDBi�BjKBk�Bl�Bn/Bo�BpBp;BpoBpoBp�Bp�Bp�Bp�Bp�Bq�BsBs�Bs�BsMBsMBsMBsMBs�Bs�BtBs�Bs�Bt�Bt�BuZBv`Bw�Bw�BwfBy�BzBzDBzDBzDBz�Bz�Bz�Bz�B{B{B|B|�B}�B}�B~(B~(B~�BcB�B�B�B�B�B�B� B�B� B�iB�4B�iB�iB�iB��B�oB�{B�SB��B��B��B�%B��B��B�+B��B��B��B��B��B��B�DB�xB��B��B�PB�PB�PB�B��B�B��B��B��B��B�(B�bB�.B��B�oB�oB��B�B�B�@B��B��B��B�MB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�SB��B��B�+B��B��B��B��B��B��B��B�1B�eB�eB�eB�eB��B�eB��B�eB��B�	B�VB�VB�!B��B�!B�!B��B��B��B��B��B�\B�'B��B��B�-B�bB�bB�bB��B��B��B�bB��B��B��B��B��B��B�hB�@B�zB�zB�zB��B��B��B��B��B��B��B�$B�XB��B��B�*B�_B��B�_B�_B�_B�_B�_B��B��B��B��B�0B��B��B��B��B��B��B��B��B��B��B�eB��B�B�6B�kB��B��B��B��B��B��B�=B��B��B��B�B�IB��B��B�}B�OB��B��B��B��B��B��B�aB��B��B�hB�hB�3B��B��B��B��B�nB��B��B��B��B�nB��B��B��B��B��B�nB�nB�nB��B�tB�B�zB��B��B��B��B��B��B�LB�B��B�$B�$B�XB�XB�XB�$B�$B�XB��B��B��B��B�*B��B�*B��B�0B��B�B�B�6B�jB��B��B��B�B�B�B��B�<B��B�B�wB�HB�HB�HB��B�}B��B��B��B�B�B;BGB�B�BGB �BoB�B��BABB�BuB�B��B�BB�B�BBB �B �BABABuBoBBB �B�BB�B iBB iB iB�BB�B�B 4B 4BoBAB�B;B  B��BB;B�B�B iB 4B  B 4B;BoB;B �B��B��B��B 4BB;B;B iB�.B��B��B  B �B �B 4B��B�cB��B��B��B �B iB��B��B�(B�cB�cB 4B��B��B��B�(B��B�(B�(B��B��B�cB�(B��B��B��B��B��B�cB��B��B��B�"B��B�VB��B�VB��B�]B�"B�VB��B��B�(B��B��B��B��B��B��B�PB�PB�PB�"B��B��B��B��B�B�B��B��B��B��B��B�VB��B�B��B�JB��B��B�B�VB��B��B��B�B�PB�PB��B��B��B��B��B�xB��B�B�B�xB��B�rB�DB��B�PB�B�JB�	B�lB�B��B��B��B�B�	B��B��B�B��B�lB��B��B�	B�B�B�rB�	B�lB�fB�fB��B��B�lB�B��B�+B�`B��B��B�8B�fB�+B��B�B�B�B�B�TB�B�B��B�B�B�;B�B�B�cB��B��B��B��B��B��B�B��B��B��B�B��B�EBԕB�}BҽB�B��B��B��B�9B��B�=B�B�5B�:B��B}�Bf�Bb�B\�Bb�B^5Bc�B�7BW�BLdBK�BM�BHKBNpBA�BJ�BB[B=qB;�B9XB7B4�B5?B3hB5?B<BC-B6�B9�BI�B2�B*�B($B#�B'�BBCB!BCB�B	BkB	BqB$B�BSB�B1B�B�B{B$B4BbBMB�B�BhB"B
�B	�B	7B
=B+B	�B�B�B+B�B�BB�B�8B�+B��B�rB�B��B�B�B��B�/B�+B�B�B�AB��B��B��B�B�>B��B��B��B�EB��B��B�
B�EB��B֡B�&B��B�[BуB�NB�B�NB҉B��BʌB��BǮB�?B�pB�B�BB��B��B��B��B��B��B��B�LB�B��B��B��B��B��B��B��B�B�qB�-B��B�.B�4B�DB�xB�_B�_B�%B��B�+B��B�oB�iB� B}�B|�B|PB� BxlBw�BuZBr�Bp;Bn�Br�Bs�Bo�Bo�BkQBg�BdZBd�Be�Bd�Ba|Ba|Ba�B`�B_�B^5B^B^�B_�B]/BZ�BaB]/BW�BQ�BQ�BTaBPHBGzBffBI�BI�BMB;dBEmBB'B/OB3�B)�B'B%B'�B%zB �B&B�B	B�B�B�B�BIB�BDB;B�MB�GB�|B�AB�2B��B��B�oB��B�5B�QB�QB�B�?B�BܒB�&B�gB͟B�9BߤB��B�dB�0B�6B�BB�!B�!B��B�$B��B��B��B��B�7B��B��B�B�B��B�hB��B��B�PB�B�oB�SB��B��B�AB�B�GB��B� B}�B{�Bw�BxlBwfBv�Bt�Bw�BtTBsBq�Bu�BtBpBiDBs�Bg�Bo B`�B_BZBU2BQNBO�BP}BN<BM�BK)BI�BHKBIBHKBGBE�BF�B?�B=qB=<B>BB7B5?B6FB7�BU�BF�B�BB�B�B�B4B�B�B�B�B(B�B�BVBPB�B�B
	BVB�B�B�B�B�B{B�B�B
=B�B
=B�B�B	B1B1B�BfB�B�BhBMB�B;B �B�B�B �BSB�B��B�B�(B�MB��B��BMB�vB��B�B�B�`B�B�|B�vB�GB�;B�B�5B�]B�]B�B��B��B��B�B�B��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020120120282320201201202823IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164420210325101644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                