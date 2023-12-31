CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-10-18T13:28:18Z creation; 2023-04-26T19:24:27Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181018132818  20230426192427  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7315_008643_028                 7315_008643_028                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @؉�҈�p@؉�҈�p11  @؉�S&@؉�S&@1��8�K@1��8�K�d�KƧ��d�KƧ�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?�@:�H@}p�@�G�@\@�G�A ��A\)A\)A*�HA>{A_\)A�Q�A���A�Q�A���A���A�Q�A��A�Q�B   B  B�B�B   B((�B0(�B8  B?�
BH(�BP  BW�
B`  Bh(�Bp  Bx  B�  B�  B�{B�{B��B��
B��
B��
B�  B�  B�{B�{B��B��B�  B�  B��B�(�B�(�B�  B�  B�  B�  B��B��B�  B�{B�{B��B��B�  B�  B��C��C  C  C  C
  C��C��C  C  C��C�C��C  C��C
=C 
=C!��C#�C%��C(
=C*  C,  C.
=C0
=C2
=C3��C5��C7��C:  C;��C=��C@  CB
=CD
=CF�CH{CJ  CK��CM��CP  CR  CS��CV
=CX{CZ
=C[�C]�C_��Ca��Cc�Cf  Ch
=Cj
=Cl
=Cn  Cp
=Cr  Ct  Cv  Cw��Cy��C|  C~
=C��C���C�  C���C��C���C�C�C���C�  C�
=C�
=C�
=C���C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�  C���C���C���C�C�C�C�  C���C���C�  C�
=C�C���C���C�  C�C�  C�C�  C���C���C�  C�  C�  C���C���C���C���C�  C�  C�C�  C�C�C�
=C�C�  C�  C���C���C�C�  C�C�
=C�  C���C�C�C�  C���C���C���C���C�  C�  C�C���C���C�  C�  C�  C���C���C�  C�C�  C���C�C�C���C���C���C�  C�
=C�C�  C���C���C���C���C�  C���C���C�C�
=C�C�C�
=C�  C�  C�C���C�  C���C���C�C�  C��D }qD  Dz�D�D��D  D� D��D}qD�qD� D  D� D  D��D�qD� D	  D	� D
D
� D
�qD��DD�DD��D  D}qD  D��D  Dz�D�D��D�D��D�D� D  D� D�qD}qD  D��D  D��D  D� D  D� D�D��DD� D�D�D�qD� DD� D�D��D   D ��D �qD!}qD"D"��D"�qD#��D$�D$�D%  D%}qD%�qD&� D'D'�D(�D(��D)�D)��D*  D*� D+  D+� D+��D,}qD,�qD-xRD-��D.}qD/  D/��D/�qD0}qD0�qD1z�D1�qD2� D3  D3� D4�D4��D4��D5}qD5��D6z�D6��D7��D8D8� D9  D9��D:D:��D:�qD;� D<�D<� D<�qD=z�D=�qD>��D?D?�D@�D@}qD@��DA� DB�DBz�DC  DC�DC�qDD� DE  DE� DF�DFz�DG  DG��DH�DH��DH�qDIz�DJ  DJ}qDK�DK� DK�qDL� DM�DM�DNDN��DO  DO��DPDP� DP�qDQ� DQ��DRz�DS  DS� DT  DT�DU  DU� DU�qDV� DWDW��DX  DX� DX�qDY}qDZ  DZ}qD[  D[� D[�qD\� D]�D]}qD^  D^��D_  D_� D`  D`}qD`�qDa}qDb  Db��Dc�Dc��Dd  Dd� De�De}qDe��Df� DgDg�Dh�Dh��DiDi��DjDj�DkDk� Dk��Dlz�Dm  Dm�DnDn�DoDo� Do��Dpz�Dp�qDq}qDr�Dr��Dr�qDs}qDs�qDtz�Du  Du��Dv  Dv� Dw  Dw� Dw��Dx}qDy  Dy}qDzDz�D{  D{� D|�D|� D|�RD}}qD}�qD~}qD�D� D�qD�@ D��HD�D���D�>�D�� D��qD�HD�C�D�� D���D�HD�@ D�~�D���D��qD�@ D�� D��HD�HD�AHD�~�D�� D�HD�@ D��HD�� D��qD�@ D���D��HD�  D�AHD�~�D�� D��D�@ D�� D��HD���D�>�D�}qD��HD�  D�AHD��HD�D�HD�@ D�� D���D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�B�D��HD���D�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�D��D�@ D�� D�� D�  D�>�D�~�D��HD��D�@ D�~�D���D��qD�>�D�� D���D�  D�@ D��HD�D��D�@ D�~�D���D��qD�>�D�~�D���D�HD�B�D���D���D�HD�AHD�� D�� D�HD�B�D�� D���D�HD�B�D��HD���D�  D�AHD��HD�� D��)D�@ D�~�D��qD�  D�AHD�}qD�D��D�@ D��HD���D�  D�AHD�� D�� D�HD�@ D��HD��HD��qD�@ D�� D��qD�HD�@ D�|)D�� D�HD�=qD���D���D���D�B�D�~�D�� D��D�B�D�� D�� D�HD�>�D�� D���D�  D�AHD��HD�D���D�B�D�~�D�� D�  D�@ D���D���D�  D�>�D�~�D��qD�  D�=qD�� D���D�  D�>�D�� D�D���D�AHD�� D���D�  D�>�D�� D�D�  D�>�D��HD���D��qD�=qD�~�D��HD�HD�>�D��HD�D���D�>�D�~�D�� D���D�>�D�~�D���D�  D�@ D�~�D��HD�HD�AHD���D�� D���D�>�D�}qD���D�HD�>�D�� D���D�HD�>�D��HD�� D���D�AHD���D�� D���D�@ D�}qD��HD���D�AHD�� D���D���D�@ D�� D���D�  D�>�D�~�D�� D�HD�@ D D���D�HD�AHD�}qD�� D�HD�AHDāHDľ�D�  D�>�DŁHD��HD�  D�AHDƁHD�� D���D�@ DǁHD��HD��D�AHD�~�D�� D�HD�@ Dɀ Dɾ�D�  D�@ DʁHD�D�  D�>�DˁHD˽qD���D�@ D́HD��HD�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ D�~�DϾ�D�  D�@ DЀ D�� D���D�>�Dр D�� D�  D�=qDҀ DҾ�D�  D�@ DӀ D�� D���D�>�DԁHD��HD�HD�@ D�~�Dվ�D�  D�>�Dր D��HD�  D�AHD׀ D׾�D�  D�AHD؂�D�� D���D�AHD�~�D��HD�  D�>�Dڀ D�� D��qD�AHDۀ D�� D���D�@ D܁HD�� D�HD�@ D݀ Dݾ�D���D�AHDހ D�� D���D�@ D߀ D�� D�HD�<)D�~�D�qD�  D�AHD� D�� D�HD�@ D₏D�� D��)D�@ D�HD�D���D�@ D䂏D侸D��)D�=qD� D��HD���D�@ D悏D��HD���D�>�D�}qD�qD�  D�=qD�}qD�� D��D�C�D�~�D��D��qD�>�D� D�D�HD�>�D�~�D��HD�HD�AHD�HD�� D�  D�@ D킏D���D�HD�>�D�~�D�qD���D�>�D� D�� D���D�@ D��HD�D��D�AHD� D�D�HD�>�D�~�D�� D��qD�=qD�}qD�)D�  D�@ D� D�� D�  D�B�D���D�� D�  D�@ D�� D��HD�HD�B�D�� D��qD���D�>�D�}qD���D�HD�B�D��HD��qD�  D�>�>�Q�?��?u?���?��?�@\)@&ff@:�H@W
=@n{@�ff@�33@��R@���@�@�  @���@ٙ�@�ff@�AG�A�A(�A�AQ�A{A%�A+�A1�A8Q�A>�RAE�AL(�AS�
AZ�HA`��AhQ�Ao\)AuA|��A�G�A�(�A�\)A��\A�A�G�A�(�A��A��\A��RA���A��A��A��\A�p�A��A�=qA��A�  A�=qA���A�
=A���A�z�AƸRAȣ�Aʏ\A���A�{AϮAљ�A�(�A�A�  A��A�(�A�ffA���A�\A�z�A�ffA�  A�G�A�A�p�A�\)A�G�A��HA��A�  A�=qA�(�A�{B (�B ��B�B
=B  BG�B�RB�
B	�B
{B33BQ�B��B{B33BQ�Bp�B
=B  B�B=qB\)BQ�BG�B{B33BQ�Bp�B�RB   B ��B"{B#
=B#�
B$��B%B&�HB'�
B(��B)B*�HB,  B,��B-��B.ffB/33B0(�B0��B1G�B2=qB2�HB3�B4��B5��B6�\B7�B8��B9��B:ffB;33B<(�B<��B=��B>{B?
=B?�
B@��BA��BB�RBC�
BD��BE��BF�\BG�BH��BI�BJ{BK
=BL  BL��BM�BO33BPQ�BQ�BR=qBS
=BS�
BTz�BUG�BVffBW33BX(�BX��BZ=qB[
=B\Q�B]G�B^=qB_33B`(�Ba�Bb{Bc
=Bc�Bd��Be��Bf�RBg�Bh��Bi��Bj�RBk�
Bl��BmBn�RBo�
Bp��Br{Bs33Bt��Bu��Bv�RBw�
Bx��By�Bz�HB|  B|��B~=qB33B�(�B���B��B��B�=qB���B�G�B�B�Q�B��HB�33B�B�=qB���B�G�B��
B�=qB��HB�\)B��B�ffB���B��B�  B�z�B�
=B��B�=qB���B�p�B��B��\B�
=B��B�(�B��RB�33B��
B�ffB���B�p�B�{B���B�33B�B�=qB���B��B�{B���B�33B��B�ffB�
=B��B�(�B���B�G�B��
B��\B��B�B�Q�B��HB�\)B�{B��\B��B��
B�ffB�
=B���B�(�B���B�p�B�  B���B�G�B��
B�ffB���B��B�{B��RB�G�B��
B�ffB�
=B���B�=qB���B�\)B�  B��\B��B�B�=qB���B���B�Q�B���B�G�B��B�z�B�33B�B�=qB���B�\)B�{B��\B�33B�B�ffB�
=B���B�(�B¸RB�G�B�B�Q�B���B�G�B�B�=qBƏ\B��HB��B�G�BǅBǙ�B�  B�Q�B�z�BȸRB��HB��B�G�B�p�Bə�B�B�  B�=qBʏ\B��HB���B�G�B˅B�B��
B�{B�(�B�ffB���B��HB�
=B�\)BͅB�B��B�=qB�ffBΣ�B��HB��B�G�BυB�B�  B�(�B�z�BиRB��B�\)BѮB��
B�=qB�ffBң�B���B�33BӅBӮB��B�  B�=qB�z�BԸRB���B�G�BՅB��
B�  B�=qB֏\B���B�
=B�\)B��
B�{B�ffBأ�B��HB�33BمBٮB��
B�{B�ffBڣ�B���B�33BۅB�B�{B܏\B���B�G�BݮB�  B�=qB�ffB���B��B�\)B�B�Q�B�RB�
=B�\)B�B�{B�z�B�RB���B�G�B�B��B�Q�B��B�
=B�B��B�Q�B��B���B�\)B�B��
B�=qB�\B��HB�33B陚B�  B�z�B���B�33B뙚B��B�=qB�\B�RB�33B�B��
B�z�B��HB�33BB��
B�(�B�z�B���B�33B�B�(�B�\B���B�G�B�p�B��
B�=qB�\B��B��B��B�Q�B�z�B���B��B��B��
B�z�B���B�33B��B��B�{B�z�B���B�G�B��
B�(�B��\B���B�
=B�p�B��B�Q�B���B�
=B�\)B�B��
C {C =qC p�C �RC �HC{C33CQ�C�\C�C�C(�CffC��C�RC�
C
=C33Cp�C�RC�HC{C33C\)C�C��C  C33CQ�Cz�C��C�
C�C\)C�CC�HC  C=qCz�CC��C�C=qCp�C�C��C	(�C	ffC	�C	�C	�
C
(�C
ffC
�C
�C
�
C{C\)C�\CC�HC{C=qC�\CC��C
=C33C�C�RC�HC
=C33C�C�RC�C{C33CffC�C�HC{C(�CffC�\C�HC{CG�CffC�\C�
C�C=qCp�C��C�C�CG�Cz�C��C��C33C\)Cz�C�C  C=qCffC�C�RC
=CG�Cz�C��CC  CQ�C�C�C�
C{CffC�\C�RC�C=qCz�C��C�
C(�C\)Cz�C�C
=CG�CffC��C�C33Cp�C�CC�C\)C�C�RC{CQ�C��C�C�CQ�C�\C�C�C 33C z�C �C �HC!(�C!z�C!��C!�
C"{C"ffC"��C"��C#
=C#ffC#��C#C$  C$ffC$�C$��C%{C%p�C%�RC%�
C&{C&z�C&C&�HC'(�C'�\C'�
C(  C(=qC(��C(�C){C)Q�C)�RC)��C*(�C*ffC*��C+{C+33C+z�C+�
C,�C,G�C,��C,�C-{C-\)C-�RC-�C.33C.�\C.�
C/  C/Q�C/�C/�C0(�C0z�C0��C0��C133C1��C1�HC2{C2\)C2�C2�C3(�C3p�C3��C4  C4=qC4��C4�HC5�C5p�C5C6  C633C6p�C6��C7�C7Q�C7��C7��C8�C8ffC8��C9
=C9G�C9�C9�HC:�C:�\C:�
C;
=C;Q�C;�C;�C<(�C<��C<��C={C=z�C=��C>  C>Q�C>�C>�HC?(�C?�\C?��C@
=C@ffC@�RC@�HCAG�CA�\CACB�CBp�CB��CB�CC=qCCp�CC�CD{CDQ�CD�CD�CE�CE\)CECE��CF33CF�\CF�
CF��CG\)CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        ?u?�@:�H@}p�@�G�@\@�G�A ��A\)A\)A*�HA>{A_\)A�Q�A���A�Q�A���A���A�Q�A��A�Q�B   B  B�B�B   B((�B0(�B8  B?�
BH(�BP  BW�
B`  Bh(�Bp  Bx  B�  B�  B�{B�{B��B��
B��
B��
B�  B�  B�{B�{B��B��B�  B�  B��B�(�B�(�B�  B�  B�  B�  B��B��B�  B�{B�{B��B��B�  B�  B��C��C  C  C  C
  C��C��C  C  C��C�C��C  C��C
=C 
=C!��C#�C%��C(
=C*  C,  C.
=C0
=C2
=C3��C5��C7��C:  C;��C=��C@  CB
=CD
=CF�CH{CJ  CK��CM��CP  CR  CS��CV
=CX{CZ
=C[�C]�C_��Ca��Cc�Cf  Ch
=Cj
=Cl
=Cn  Cp
=Cr  Ct  Cv  Cw��Cy��C|  C~
=C��C���C�  C���C��C���C�C�C���C�  C�
=C�
=C�
=C���C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�  C���C���C���C�C�C�C�  C���C���C�  C�
=C�C���C���C�  C�C�  C�C�  C���C���C�  C�  C�  C���C���C���C���C�  C�  C�C�  C�C�C�
=C�C�  C�  C���C���C�C�  C�C�
=C�  C���C�C�C�  C���C���C���C���C�  C�  C�C���C���C�  C�  C�  C���C���C�  C�C�  C���C�C�C���C���C���C�  C�
=C�C�  C���C���C���C���C�  C���C���C�C�
=C�C�C�
=C�  C�  C�C���C�  C���C���C�C�  C��D }qD  Dz�D�D��D  D� D��D}qD�qD� D  D� D  D��D�qD� D	  D	� D
D
� D
�qD��DD�DD��D  D}qD  D��D  Dz�D�D��D�D��D�D� D  D� D�qD}qD  D��D  D��D  D� D  D� D�D��DD� D�D�D�qD� DD� D�D��D   D ��D �qD!}qD"D"��D"�qD#��D$�D$�D%  D%}qD%�qD&� D'D'�D(�D(��D)�D)��D*  D*� D+  D+� D+��D,}qD,�qD-xRD-��D.}qD/  D/��D/�qD0}qD0�qD1z�D1�qD2� D3  D3� D4�D4��D4��D5}qD5��D6z�D6��D7��D8D8� D9  D9��D:D:��D:�qD;� D<�D<� D<�qD=z�D=�qD>��D?D?�D@�D@}qD@��DA� DB�DBz�DC  DC�DC�qDD� DE  DE� DF�DFz�DG  DG��DH�DH��DH�qDIz�DJ  DJ}qDK�DK� DK�qDL� DM�DM�DNDN��DO  DO��DPDP� DP�qDQ� DQ��DRz�DS  DS� DT  DT�DU  DU� DU�qDV� DWDW��DX  DX� DX�qDY}qDZ  DZ}qD[  D[� D[�qD\� D]�D]}qD^  D^��D_  D_� D`  D`}qD`�qDa}qDb  Db��Dc�Dc��Dd  Dd� De�De}qDe��Df� DgDg�Dh�Dh��DiDi��DjDj�DkDk� Dk��Dlz�Dm  Dm�DnDn�DoDo� Do��Dpz�Dp�qDq}qDr�Dr��Dr�qDs}qDs�qDtz�Du  Du��Dv  Dv� Dw  Dw� Dw��Dx}qDy  Dy}qDzDz�D{  D{� D|�D|� D|�RD}}qD}�qD~}qD�D� D�qD�@ D��HD�D���D�>�D�� D��qD�HD�C�D�� D���D�HD�@ D�~�D���D��qD�@ D�� D��HD�HD�AHD�~�D�� D�HD�@ D��HD�� D��qD�@ D���D��HD�  D�AHD�~�D�� D��D�@ D�� D��HD���D�>�D�}qD��HD�  D�AHD��HD�D�HD�@ D�� D���D���D�AHD��HD��HD�  D�@ D�� D��HD�  D�>�D�� D�� D�  D�B�D��HD���D�  D�AHD�� D�� D�  D�@ D��HD��HD�  D�@ D��HD�D��D�@ D�� D�� D�  D�>�D�~�D��HD��D�@ D�~�D���D��qD�>�D�� D���D�  D�@ D��HD�D��D�@ D�~�D���D��qD�>�D�~�D���D�HD�B�D���D���D�HD�AHD�� D�� D�HD�B�D�� D���D�HD�B�D��HD���D�  D�AHD��HD�� D��)D�@ D�~�D��qD�  D�AHD�}qD�D��D�@ D��HD���D�  D�AHD�� D�� D�HD�@ D��HD��HD��qD�@ D�� D��qD�HD�@ D�|)D�� D�HD�=qD���D���D���D�B�D�~�D�� D��D�B�D�� D�� D�HD�>�D�� D���D�  D�AHD��HD�D���D�B�D�~�D�� D�  D�@ D���D���D�  D�>�D�~�D��qD�  D�=qD�� D���D�  D�>�D�� D�D���D�AHD�� D���D�  D�>�D�� D�D�  D�>�D��HD���D��qD�=qD�~�D��HD�HD�>�D��HD�D���D�>�D�~�D�� D���D�>�D�~�D���D�  D�@ D�~�D��HD�HD�AHD���D�� D���D�>�D�}qD���D�HD�>�D�� D���D�HD�>�D��HD�� D���D�AHD���D�� D���D�@ D�}qD��HD���D�AHD�� D���D���D�@ D�� D���D�  D�>�D�~�D�� D�HD�@ D D���D�HD�AHD�}qD�� D�HD�AHDāHDľ�D�  D�>�DŁHD��HD�  D�AHDƁHD�� D���D�@ DǁHD��HD��D�AHD�~�D�� D�HD�@ Dɀ Dɾ�D�  D�@ DʁHD�D�  D�>�DˁHD˽qD���D�@ D́HD��HD�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ D�~�DϾ�D�  D�@ DЀ D�� D���D�>�Dр D�� D�  D�=qDҀ DҾ�D�  D�@ DӀ D�� D���D�>�DԁHD��HD�HD�@ D�~�Dվ�D�  D�>�Dր D��HD�  D�AHD׀ D׾�D�  D�AHD؂�D�� D���D�AHD�~�D��HD�  D�>�Dڀ D�� D��qD�AHDۀ D�� D���D�@ D܁HD�� D�HD�@ D݀ Dݾ�D���D�AHDހ D�� D���D�@ D߀ D�� D�HD�<)D�~�D�qD�  D�AHD� D�� D�HD�@ D₏D�� D��)D�@ D�HD�D���D�@ D䂏D侸D��)D�=qD� D��HD���D�@ D悏D��HD���D�>�D�}qD�qD�  D�=qD�}qD�� D��D�C�D�~�D��D��qD�>�D� D�D�HD�>�D�~�D��HD�HD�AHD�HD�� D�  D�@ D킏D���D�HD�>�D�~�D�qD���D�>�D� D�� D���D�@ D��HD�D��D�AHD� D�D�HD�>�D�~�D�� D��qD�=qD�}qD�)D�  D�@ D� D�� D�  D�B�D���D�� D�  D�@ D�� D��HD�HD�B�D�� D��qD���D�>�D�}qD���D�HD�B�D��HD��qD�  G�O�>�Q�?��?u?���?��?�@\)@&ff@:�H@W
=@n{@�ff@�33@��R@���@�@�  @���@ٙ�@�ff@�AG�A�A(�A�AQ�A{A%�A+�A1�A8Q�A>�RAE�AL(�AS�
AZ�HA`��AhQ�Ao\)AuA|��A�G�A�(�A�\)A��\A�A�G�A�(�A��A��\A��RA���A��A��A��\A�p�A��A�=qA��A�  A�=qA���A�
=A���A�z�AƸRAȣ�Aʏ\A���A�{AϮAљ�A�(�A�A�  A��A�(�A�ffA���A�\A�z�A�ffA�  A�G�A�A�p�A�\)A�G�A��HA��A�  A�=qA�(�A�{B (�B ��B�B
=B  BG�B�RB�
B	�B
{B33BQ�B��B{B33BQ�Bp�B
=B  B�B=qB\)BQ�BG�B{B33BQ�Bp�B�RB   B ��B"{B#
=B#�
B$��B%B&�HB'�
B(��B)B*�HB,  B,��B-��B.ffB/33B0(�B0��B1G�B2=qB2�HB3�B4��B5��B6�\B7�B8��B9��B:ffB;33B<(�B<��B=��B>{B?
=B?�
B@��BA��BB�RBC�
BD��BE��BF�\BG�BH��BI�BJ{BK
=BL  BL��BM�BO33BPQ�BQ�BR=qBS
=BS�
BTz�BUG�BVffBW33BX(�BX��BZ=qB[
=B\Q�B]G�B^=qB_33B`(�Ba�Bb{Bc
=Bc�Bd��Be��Bf�RBg�Bh��Bi��Bj�RBk�
Bl��BmBn�RBo�
Bp��Br{Bs33Bt��Bu��Bv�RBw�
Bx��By�Bz�HB|  B|��B~=qB33B�(�B���B��B��B�=qB���B�G�B�B�Q�B��HB�33B�B�=qB���B�G�B��
B�=qB��HB�\)B��B�ffB���B��B�  B�z�B�
=B��B�=qB���B�p�B��B��\B�
=B��B�(�B��RB�33B��
B�ffB���B�p�B�{B���B�33B�B�=qB���B��B�{B���B�33B��B�ffB�
=B��B�(�B���B�G�B��
B��\B��B�B�Q�B��HB�\)B�{B��\B��B��
B�ffB�
=B���B�(�B���B�p�B�  B���B�G�B��
B�ffB���B��B�{B��RB�G�B��
B�ffB�
=B���B�=qB���B�\)B�  B��\B��B�B�=qB���B���B�Q�B���B�G�B��B�z�B�33B�B�=qB���B�\)B�{B��\B�33B�B�ffB�
=B���B�(�B¸RB�G�B�B�Q�B���B�G�B�B�=qBƏ\B��HB��B�G�BǅBǙ�B�  B�Q�B�z�BȸRB��HB��B�G�B�p�Bə�B�B�  B�=qBʏ\B��HB���B�G�B˅B�B��
B�{B�(�B�ffB���B��HB�
=B�\)BͅB�B��B�=qB�ffBΣ�B��HB��B�G�BυB�B�  B�(�B�z�BиRB��B�\)BѮB��
B�=qB�ffBң�B���B�33BӅBӮB��B�  B�=qB�z�BԸRB���B�G�BՅB��
B�  B�=qB֏\B���B�
=B�\)B��
B�{B�ffBأ�B��HB�33BمBٮB��
B�{B�ffBڣ�B���B�33BۅB�B�{B܏\B���B�G�BݮB�  B�=qB�ffB���B��B�\)B�B�Q�B�RB�
=B�\)B�B�{B�z�B�RB���B�G�B�B��B�Q�B��B�
=B�B��B�Q�B��B���B�\)B�B��
B�=qB�\B��HB�33B陚B�  B�z�B���B�33B뙚B��B�=qB�\B�RB�33B�B��
B�z�B��HB�33BB��
B�(�B�z�B���B�33B�B�(�B�\B���B�G�B�p�B��
B�=qB�\B��B��B��B�Q�B�z�B���B��B��B��
B�z�B���B�33B��B��B�{B�z�B���B�G�B��
B�(�B��\B���B�
=B�p�B��B�Q�B���B�
=B�\)B�B��
C {C =qC p�C �RC �HC{C33CQ�C�\C�C�C(�CffC��C�RC�
C
=C33Cp�C�RC�HC{C33C\)C�C��C  C33CQ�Cz�C��C�
C�C\)C�CC�HC  C=qCz�CC��C�C=qCp�C�C��C	(�C	ffC	�C	�C	�
C
(�C
ffC
�C
�C
�
C{C\)C�\CC�HC{C=qC�\CC��C
=C33C�C�RC�HC
=C33C�C�RC�C{C33CffC�C�HC{C(�CffC�\C�HC{CG�CffC�\C�
C�C=qCp�C��C�C�CG�Cz�C��C��C33C\)Cz�C�C  C=qCffC�C�RC
=CG�Cz�C��CC  CQ�C�C�C�
C{CffC�\C�RC�C=qCz�C��C�
C(�C\)Cz�C�C
=CG�CffC��C�C33Cp�C�CC�C\)C�C�RC{CQ�C��C�C�CQ�C�\C�C�C 33C z�C �C �HC!(�C!z�C!��C!�
C"{C"ffC"��C"��C#
=C#ffC#��C#C$  C$ffC$�C$��C%{C%p�C%�RC%�
C&{C&z�C&C&�HC'(�C'�\C'�
C(  C(=qC(��C(�C){C)Q�C)�RC)��C*(�C*ffC*��C+{C+33C+z�C+�
C,�C,G�C,��C,�C-{C-\)C-�RC-�C.33C.�\C.�
C/  C/Q�C/�C/�C0(�C0z�C0��C0��C133C1��C1�HC2{C2\)C2�C2�C3(�C3p�C3��C4  C4=qC4��C4�HC5�C5p�C5C6  C633C6p�C6��C7�C7Q�C7��C7��C8�C8ffC8��C9
=C9G�C9�C9�HC:�C:�\C:�
C;
=C;Q�C;�C;�C<(�C<��C<��C={C=z�C=��C>  C>Q�C>�C>�HC?(�C?�\C?��C@
=C@ffC@�RC@�HCAG�CA�\CACB�CBp�CB��CB�CC=qCCp�CC�CD{CDQ�CD�CD�CE�CE\)CECE��CF33CF�\CF�
CF��CG\)CG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�+A�(�A�+A�+A�(�A�(�A�&�A�(�A�(�A�+A�/A�/A�+A�+A�-A�&�A�&�A�"�A�oA���A�p�A��DAߡ�A�33Aމ7A��`A�&�A��;A۴9Aۥ�A�=qA��
AځAٸRA�A�(�Aש�A�jA��`A�E�A�$�A�K�A��mA�1A�ȴA�?}A�JA��A�x�A�K�AɑhAȗ�A�E�A�ffA�{AĸRAËDA�I�A��mA��PA��#A��-A���A�t�A�A�I�A���A�%A��A���A�A��jA��RA��/A�bNA�{A�
=A��uA�"�A�A� �A��
A��FA���A�&�A��A�A�A�I�A�E�A�7LA��yA���A���A�7LA��hA�=qA���A���A���A�S�A���A��A�-A���A�-A��^Az��Ax�/Aw��AwS�Avz�As\)Ak��Ad�+Aa�wA_t�A^-A\�/A[?}AYƨAX�jAV��AVE�AU�AS|�AQ"�AMG�AJM�AH�yAG�PAE��AD�9ADz�ADVAD  A?O�A<�jA;hsA:{A6^5A1�wA0E�A/�7A/`BA.�A.A,{A+�TA+��A*�yA*�uA(�uA'VA&��A%�;A%��A$E�A#XA"�A"^5A!�A!��A!�A!�PA �A��A5?A(�A1'AZAr�A  A�AAI�AI�A�+A��A��Av�A�`A+A%AffA�AI�A��A��A&�AXA��AE�AM�A�A�AoA�PA�AM�A�^A��A�A�AQ�Av�A%A �A�#A�AĜA5?A��A�FA��A&�A�A�\A$�AA�A
�A
�\A
E�A	�#A	t�A�AJAS�A�uA^5A�#A�7A�AI�A��A��A?}A��AffA�A�A ��A 9X@��F@�|�@�ff@��@��@��u@�j@�33@�E�@��-@�x�@�G�@���@�1'@�\)@�~�@��^@�?}@��`@�r�@�@�^5@�@�S�@�@�M�@�-@�x�@�/@�G�@睲@�M�@�;d@��
@���@��#@�x�@�/@�b@�v�@�/@���@���@�  @ߍP@ޟ�@�X@�V@ܣ�@���@۶F@ڰ!@٩�@ؼj@� �@�dZ@�\)@֏\@�/@�j@�(�@���@ӕ�@�@�-@��@��T@с@�bN@϶F@���@Η�@�E�@�@�Z@�l�@�^5@�&�@���@�^5@�{@�v�@�$�@Ł@�/@���@�Ĝ@�l�@��@�b@�S�@�+@�V@�&�@��@�o@���@���@��+@�5?@��@��@�z�@���@�l�@���@��\@�M�@��@��@���@���@���@��@�j@�  @�S�@��@��@���@���@�E�@�{@��^@�G�@�V@��j@��@��m@���@�\)@�33@�@���@�v�@�5?@���@��-@���@�@��-@�G�@��@���@�j@�1'@�  @��
@��@�\)@�;d@��y@��R@�ff@��^@�G�@���@��@��u@�j@�9X@� �@�1@��
@��@�+@���@��+@�^5@�@��#@���@���@�@���@��@�X@��@�A�@�b@�1@�1@��@���@���@��;@���@�t�@�
=@�=q@��^@�%@��@��j@�I�@��@���@��m@�l�@�ȴ@���@�ff@�$�@��#@�@��7@�X@�&�@���@��/@�Ĝ@��@��D@�Z@��@��F@��P@�l�@�+@���@���@�=q@�$�@��#@���@��@�X@��@��/@�Ĝ@��D@� �@��F@��P@�+@���@�ȴ@��\@�V@�5?@�J@��@���@�7L@��`@��u@�bN@�9X@� �@�1@��;@��w@��@��P@�\)@�K�@�33@��@���@��y@��@���@��R@�n�@�5?@���@�X@�/@��@�V@��@��@�A�@�1'@��@��F@�|�@�K�@��@�@��H@���@�v�@�ff@�ff@�{@��^@���@���@���@�&�@�%@��`@�z�@�9X@��;@���@��@�dZ@�S�@�;d@���@��\@�=q@��@���@�X@�G�@�?}@�7L@���@��D@�I�@� �@��@�+@��\@��@�x�@�?}@�&�@�%@��/@��@�A�@���@�\)@�"�@���@��y@�ȴ@��R@���@�v�@�-@��@���@��h@�x�@�x�@�hs@��`@��@�bN@�1'@��@
=@~��@}�@}O�@}�@}�@|�/@|�D@|z�@|1@{��@{dZ@z��@zn�@z^5@z=q@z�@yX@y�@x�9@xA�@x  @w�P@w\)@v�y@u@t��@tZ@t1@s��@s"�@q�#@qhs@p�`@p�9@pA�@p  @o�w@o\)@o
=@n�@n�+@m@m?}@l�@lI�@k�m@k�m@k�
@k�
@kƨ@k�F@k��@kC�@ko@j�@j��@jM�@i��@ix�@i&�@hbN@hb@g�@g�@gl�@g�@f�y@f$�@e�h@eO�@d��@d(�@c�
@c�F@cS�@co@b=q@a��@aG�@`��@_�@^�@^5?@]��@]?}@\Z@[�F@Z�@ZJ@Y�^@Y��@YX@X�`@X�u@Xr�@W��@W�@VV@U@U�h@U?}@T��@T�j@T��@TI�@S�m@St�@So@R��@R�\@R^5@RM�@R=q@Q�^@QG�@Q&�@P�`@P�@Pr�@P1'@O�w@Ol�@N�y@Nv�@N{@M�@M�-@M?}@L��@Kƨ@K��@KdZ@K@J��@J��@J�\@J~�@J^5@J-@I��@I7L@H�`@H�9@H�9@H�9@HQ�@Hb@G�@G��@Gl�@F��@FE�@F5?@E@D�@DZ@D(�@C�
@CC�@B�@Bn�@B=q@B-@A�@A��@A�@@�@@  @?�w@?�P@?|�@?+@?+@?�@>�y@>�R@>�R@>��@>��@>ff@>5?@=�@=��@=��@=�@=V@<�j@;�m@;��@;S�@;o@:�@:n�@9�#@8��@8bN@7�@7�w@7�@7�P@7l�@7\)@6��@6�+@6v�@6{@5p�@4��@4��@4�D@4I�@3ƨ@3�@3o@2�H@2-@1�@1�#@1�^@17L@0r�@0A�@0b@/�;@/l�@.�@.@-�@-�@-�@-��@-�@,��@,9X@+�F@+��@+��@+�@+S�@+C�@*��@*n�@*M�@*-@)��@)G�@(��@(�u@(bN@( �@'��@';d@&��@&�+@&$�@%�T@%��@%�@%`B@$�/@#��@#S�@#o@"�H@"��@"��@"�!@"�\@"~�@"M�@"�@!�#@!7L@ r�@ Q�@  �@ b@   @�w@�P@\)@K�@+@�y@��@��@ff@$�@�@�T@@�-@�h@O�@V@�@��@�@z�@��@t�@33@"�@"�@"�@"�@o@�@��@�\@^5@M�@-@J@��@��@��@�#@��@�^@��@�7@7L@��@��@�u@�@r�@r�@1'@1'@ �@1'@1'@ �@ �@b@�@��@�@�@��@��@�P@l�@\)@;d@��@�R@�R@��@��@��@�+@v�@E�@�T@�-@��@��@�h@�h@p�@`B@`B@?}@/@�D@9X@9X@(�@�@�m@��@�@�@�@�!@�\@J@��@�7@x�@x�@hs@X@G�@G�@G�@7L@&�@�@��@��@�@Q�@ �@  @�;@��@�w@��@l�@;d@�@��@��@
=A���A���A�"�A� �A��A�$�A�(�A�&�A�"�A�&�A�$�A�$�A�$�A�&�A�&�A�+A�+A�+A�+A�+A�&�A�$�A�&�A�+A�+A�+A�-A�-A�-A�-A�+A�+A�+A�+A�(�A�+A�(�A�(�A�&�A�&�A�(�A�+A�+A�+A�-A�+A�(�A�(�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�(�A�(�A�(�A�-A�-A�+A�+A�(�A�&�A�&�A�$�A�&�A�(�A�&�A�-A�/A�-A�1'A�1'A�1'A�/A�/A�-A�-A�-A�-A�/A�1'A�33A�33A�33A�1'A�1'A�/A�/A�+A�+A�+A�(�A�-A�-A�/A�-A�-A�+A�+A�&�A�(�A�(�A�+A�/A�/A�1'A�/A�/A�-A�(�A�&�A�(�A�(�A�"�A�"�A�"�A�&�A�$�A�$�A�&�A�$�A�$�A�&�A�+A�&�A� �A�$�A�$�A�"�A�"�A�"�A� �A��A��A�$�A��A�$�A� �A�VA��A�A�oA�bA��mA�
=A��TA��mA��A���A�%A�
=A�{A��A��A��A�
=A��A�\)A�A��A�FA�XA�M�A� �A�ȴA�ĜA�jA�+A�{A�  A��yA�A߶FAߥ�Aߙ�Aߗ�AߍPA߅A�p�A�bNA�VA�K�A�7LA�"�A��A��A�oA�
=A��`Aް!AލPA�v�A�hsA�S�A�A�A�1'A�"�A�oA���A��`A���A�ƨAݾwAݶFAݶFAݮAݏ\A�(�A���A�A�\)A�JA��A��HA���A���A���A���A�A�A�AۼjA۶FA۬Aۣ�Aۗ�AۑhAە�A۸RA�A۴9Aۣ�Aۙ�AۋDAۃA�jA�M�A�5?A�+A��A��A�JA�  A��A��#A��
A���A���AڸRAڣ�Aڛ�Aڗ�AڑhA�|�A�r�A�;dA�/A�{A���A٥�AٓuAكA�l�A� �A���A���A��Aز-A�n�A�bNA�p�A�dZA�=qA�(�A�{A���A��TA��/A���A״9Aס�Aם�A׍PAׁA�|�A�v�A�p�A�jA�dZA�ZA�K�A�9XA�&�A�A��/A�ĜA։7A�|�A�r�A�hsA�\)A�I�A�(�A���A��A�r�A�-A�$�A�A���Aԝ�A�n�A�ffA�Q�A�I�A�A�A�33A� �A�VA���A���A��yA��A�Aӣ�A�33A�~�A�(�AэPA�ffA�"�A��mAа!A�G�Aϧ�A�7LA�%A���AάAΕ�A·+A�v�A�`BA�+A��/A̓A�/A�bA�bA�bA�VA�JA�oA�JA�
=A�1A�
=A�1A�
=A�1A�
=A�A���A��A��A��mA̲-A��A��A��mA��`A��`A��`A��`A��HA��A��#A��#A��#A��A��A��
A���A��
A��
A���A���A�ȴA˾wA˺^A˧�A˅A�dZA�G�A��A��#AʶFAʧ�AʓuAʇ+A�x�A�l�A�ffA�dZA�ZA�XA�O�A�E�A�9XA�/A�&�A� �A�{A�bA�A���A���A��A��mA��HA��/A���Aɟ�A�`BA�33A���A���A���A���A��A��A��A��A��yA��#A�AȍPA�VA�5?A��A�
=A��mAǲ-A��AƓuA�K�A�{A�  A��yA��A���AžwAŴ9Aţ�AōPAŁA�v�A�p�A�ffA�\)A�VA�S�A�S�A�M�A�E�A�?}A�9XA�7LA�+A�$�A��A�bA�%A�%A���A��yA��HA��;A���A���AļjAĸRAĴ9Aİ!Aħ�Aĥ�Ağ�Aě�AčPA�XA�(�A���A�t�A�VA���A��A��HA£�A�l�A�\)A�S�A�M�A�G�A�A�A�A�A�A�A�?}A�;dA�33A�33A�33A�(�A�$�A�{A���A��HA�ƨA���A�|�A�K�A��A��A���A��FA���A��DA�dZA�E�A�33A�(�A�oA�A��A��yA��`A��HA��;A��/A��#A���A��^A���A�r�A�A�A�oA�ĜA�VA��mA��A�t�A�1A��A��A��A��yA��mA���A���A��A��PA�n�A�I�A�5?A��A�
=A���A���A���A��
A��DA�+A��;A���A�`BA�1A��uA�l�A�33A�ĜA���A���A��hA��DA��+A��A�z�A�r�A�dZA�Q�A�?}A�/A� �A��A�
=A�  A���A��A��yA��HA��/A���A��jA��A�7LA�oA�1A��A�A�l�A��A�C�A�1A���A��;A���A��!A��7A�C�A�VA���A���A�z�A�dZA�I�A�&�A��A�
=A���A��HA�ȴA��9A���A��hA�bNA�-A�&�A� �A�A���A���A��A��yA��yA��A�A�ƨA��^A��wA��jA��9A��!A��A���A���A��7A�hsA�I�A�
=A��-A�Q�A��#A�|�A�XA�/A��A�%A��A�ƨA��-A���A���A��hA��7A��7A��7A��A�ffA�C�A�-A�oA��A���A�~�A�5?A�bA��mA���A��PA�hsA�=qA�-A��A�1A���A��yA��TA��
A�ƨA��jA��A���A��PA�t�A�^5A�I�A�A�ȴA��!A���A�t�A�~�A���A�"�A�ZA�VA���A���A��A��yA��
A��!A��PA�^5A�(�A�{A��`A��;A��TA��mA��A��mA��A���A�ȴA���A��wA�ĜA�ȴA�ƨA��wA��FA���A���A�|�A�n�A�M�A�"�A���A��A�dZA�C�A�1'A�$�A�$�A�$�A� �A��A��A��A�bA��A��A��`A��#A���A��^A�`BA�1'A�5?A��A�oA��#A��jA��hA�O�A�+A� �A�oA���A��A��A��HA�r�A��A���A�ffA�$�A���A���A�5?A��A���A�ffA�\)A�M�A�7LA�-A���A�E�A��mA��DA�"�A��TA��uA�?}A��A�t�A�S�A�$�A�
=A���A��#A��^A���A�|�A�^5A�K�A�33A��A���A��A��#A�A���A���A�z�A�hsA�C�A��A�ȴA�jA�$�A�JA��A��#A��A��A�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        A��A�$�A�$�A�&�A�&�A�(�A�(�A�(�A�(�A�+A�(�A�+A�+A�(�A�(�A�&�A�(�A�(�A�+A�/A�/A�+A�+A�-A�&�A�&�A�"�A�oA���A�p�A��DAߡ�A�33Aމ7A��`A�&�A��;A۴9Aۥ�A�=qA��
AځAٸRA�A�(�Aש�A�jA��`A�E�A�$�A�K�A��mA�1A�ȴA�?}A�JA��A�x�A�K�AɑhAȗ�A�E�A�ffA�{AĸRAËDA�I�A��mA��PA��#A��-A���A�t�A�A�I�A���A�%A��A���A�A��jA��RA��/A�bNA�{A�
=A��uA�"�A�A� �A��
A��FA���A�&�A��A�A�A�I�A�E�A�7LA��yA���A���A�7LA��hA�=qA���A���A���A�S�A���A��A�-A���A�-A��^Az��Ax�/Aw��AwS�Avz�As\)Ak��Ad�+Aa�wA_t�A^-A\�/A[?}AYƨAX�jAV��AVE�AU�AS|�AQ"�AMG�AJM�AH�yAG�PAE��AD�9ADz�ADVAD  A?O�A<�jA;hsA:{A6^5A1�wA0E�A/�7A/`BA.�A.A,{A+�TA+��A*�yA*�uA(�uA'VA&��A%�;A%��A$E�A#XA"�A"^5A!�A!��A!�A!�PA �A��A5?A(�A1'AZAr�A  A�AAI�AI�A�+A��A��Av�A�`A+A%AffA�AI�A��A��A&�AXA��AE�AM�A�A�AoA�PA�AM�A�^A��A�A�AQ�Av�A%A �A�#A�AĜA5?A��A�FA��A&�A�A�\A$�AA�A
�A
�\A
E�A	�#A	t�A�AJAS�A�uA^5A�#A�7A�AI�A��A��A?}A��AffA�A�A ��A 9X@��F@�|�@�ff@��@��@��u@�j@�33@�E�@��-@�x�@�G�@���@�1'@�\)@�~�@��^@�?}@��`@�r�@�@�^5@�@�S�@�@�M�@�-@�x�@�/@�G�@睲@�M�@�;d@��
@���@��#@�x�@�/@�b@�v�@�/@���@���@�  @ߍP@ޟ�@�X@�V@ܣ�@���@۶F@ڰ!@٩�@ؼj@� �@�dZ@�\)@֏\@�/@�j@�(�@���@ӕ�@�@�-@��@��T@с@�bN@϶F@���@Η�@�E�@�@�Z@�l�@�^5@�&�@���@�^5@�{@�v�@�$�@Ł@�/@���@�Ĝ@�l�@��@�b@�S�@�+@�V@�&�@��@�o@���@���@��+@�5?@��@��@�z�@���@�l�@���@��\@�M�@��@��@���@���@���@��@�j@�  @�S�@��@��@���@���@�E�@�{@��^@�G�@�V@��j@��@��m@���@�\)@�33@�@���@�v�@�5?@���@��-@���@�@��-@�G�@��@���@�j@�1'@�  @��
@��@�\)@�;d@��y@��R@�ff@��^@�G�@���@��@��u@�j@�9X@� �@�1@��
@��@�+@���@��+@�^5@�@��#@���@���@�@���@��@�X@��@�A�@�b@�1@�1@��@���@���@��;@���@�t�@�
=@�=q@��^@�%@��@��j@�I�@��@���@��m@�l�@�ȴ@���@�ff@�$�@��#@�@��7@�X@�&�@���@��/@�Ĝ@��@��D@�Z@��@��F@��P@�l�@�+@���@���@�=q@�$�@��#@���@��@�X@��@��/@�Ĝ@��D@� �@��F@��P@�+@���@�ȴ@��\@�V@�5?@�J@��@���@�7L@��`@��u@�bN@�9X@� �@�1@��;@��w@��@��P@�\)@�K�@�33@��@���@��y@��@���@��R@�n�@�5?@���@�X@�/@��@�V@��@��@�A�@�1'@��@��F@�|�@�K�@��@�@��H@���@�v�@�ff@�ff@�{@��^@���@���@���@�&�@�%@��`@�z�@�9X@��;@���@��@�dZ@�S�@�;d@���@��\@�=q@��@���@�X@�G�@�?}@�7L@���@��D@�I�@� �@��@�+@��\@��@�x�@�?}@�&�@�%@��/@��@�A�@���@�\)@�"�@���@��y@�ȴ@��R@���@�v�@�-@��@���@��h@�x�@�x�@�hs@��`@��@�bN@�1'@��@
=@~��@}�@}O�@}�@}�@|�/@|�D@|z�@|1@{��@{dZ@z��@zn�@z^5@z=q@z�@yX@y�@x�9@xA�@x  @w�P@w\)@v�y@u@t��@tZ@t1@s��@s"�@q�#@qhs@p�`@p�9@pA�@p  @o�w@o\)@o
=@n�@n�+@m@m?}@l�@lI�@k�m@k�m@k�
@k�
@kƨ@k�F@k��@kC�@ko@j�@j��@jM�@i��@ix�@i&�@hbN@hb@g�@g�@gl�@g�@f�y@f$�@e�h@eO�@d��@d(�@c�
@c�F@cS�@co@b=q@a��@aG�@`��@_�@^�@^5?@]��@]?}@\Z@[�F@Z�@ZJ@Y�^@Y��@YX@X�`@X�u@Xr�@W��@W�@VV@U@U�h@U?}@T��@T�j@T��@TI�@S�m@St�@So@R��@R�\@R^5@RM�@R=q@Q�^@QG�@Q&�@P�`@P�@Pr�@P1'@O�w@Ol�@N�y@Nv�@N{@M�@M�-@M?}@L��@Kƨ@K��@KdZ@K@J��@J��@J�\@J~�@J^5@J-@I��@I7L@H�`@H�9@H�9@H�9@HQ�@Hb@G�@G��@Gl�@F��@FE�@F5?@E@D�@DZ@D(�@C�
@CC�@B�@Bn�@B=q@B-@A�@A��@A�@@�@@  @?�w@?�P@?|�@?+@?+@?�@>�y@>�R@>�R@>��@>��@>ff@>5?@=�@=��@=��@=�@=V@<�j@;�m@;��@;S�@;o@:�@:n�@9�#@8��@8bN@7�@7�w@7�@7�P@7l�@7\)@6��@6�+@6v�@6{@5p�@4��@4��@4�D@4I�@3ƨ@3�@3o@2�H@2-@1�@1�#@1�^@17L@0r�@0A�@0b@/�;@/l�@.�@.@-�@-�@-�@-��@-�@,��@,9X@+�F@+��@+��@+�@+S�@+C�@*��@*n�@*M�@*-@)��@)G�@(��@(�u@(bN@( �@'��@';d@&��@&�+@&$�@%�T@%��@%�@%`B@$�/@#��@#S�@#o@"�H@"��@"��@"�!@"�\@"~�@"M�@"�@!�#@!7L@ r�@ Q�@  �@ b@   @�w@�P@\)@K�@+@�y@��@��@ff@$�@�@�T@@�-@�h@O�@V@�@��@�@z�@��@t�@33@"�@"�@"�@"�@o@�@��@�\@^5@M�@-@J@��@��@��@�#@��@�^@��@�7@7L@��@��@�u@�@r�@r�@1'@1'@ �@1'@1'@ �@ �@b@�@��@�@�@��@��@�P@l�@\)@;d@��@�R@�R@��@��@��@�+@v�@E�@�T@�-@��@��@�h@�h@p�@`B@`B@?}@/@�D@9X@9X@(�@�@�m@��@�@�@�@�!@�\@J@��@�7@x�@x�@hs@X@G�@G�@G�@7L@&�@�@��@��@�@Q�@ �@  @�;@��@�w@��@l�@;d@�@��@��G�O�A���A���A�"�A� �A��A�$�A�(�A�&�A�"�A�&�A�$�A�$�A�$�A�&�A�&�A�+A�+A�+A�+A�+A�&�A�$�A�&�A�+A�+A�+A�-A�-A�-A�-A�+A�+A�+A�+A�(�A�+A�(�A�(�A�&�A�&�A�(�A�+A�+A�+A�-A�+A�(�A�(�A�&�A�$�A�$�A�&�A�$�A�$�A�$�A�(�A�(�A�(�A�-A�-A�+A�+A�(�A�&�A�&�A�$�A�&�A�(�A�&�A�-A�/A�-A�1'A�1'A�1'A�/A�/A�-A�-A�-A�-A�/A�1'A�33A�33A�33A�1'A�1'A�/A�/A�+A�+A�+A�(�A�-A�-A�/A�-A�-A�+A�+A�&�A�(�A�(�A�+A�/A�/A�1'A�/A�/A�-A�(�A�&�A�(�A�(�A�"�A�"�A�"�A�&�A�$�A�$�A�&�A�$�A�$�A�&�A�+A�&�A� �A�$�A�$�A�"�A�"�A�"�A� �A��A��A�$�A��A�$�A� �A�VA��A�A�oA�bA��mA�
=A��TA��mA��A���A�%A�
=A�{A��A��A��A�
=A��A�\)A�A��A�FA�XA�M�A� �A�ȴA�ĜA�jA�+A�{A�  A��yA�A߶FAߥ�Aߙ�Aߗ�AߍPA߅A�p�A�bNA�VA�K�A�7LA�"�A��A��A�oA�
=A��`Aް!AލPA�v�A�hsA�S�A�A�A�1'A�"�A�oA���A��`A���A�ƨAݾwAݶFAݶFAݮAݏ\A�(�A���A�A�\)A�JA��A��HA���A���A���A���A�A�A�AۼjA۶FA۬Aۣ�Aۗ�AۑhAە�A۸RA�A۴9Aۣ�Aۙ�AۋDAۃA�jA�M�A�5?A�+A��A��A�JA�  A��A��#A��
A���A���AڸRAڣ�Aڛ�Aڗ�AڑhA�|�A�r�A�;dA�/A�{A���A٥�AٓuAكA�l�A� �A���A���A��Aز-A�n�A�bNA�p�A�dZA�=qA�(�A�{A���A��TA��/A���A״9Aס�Aם�A׍PAׁA�|�A�v�A�p�A�jA�dZA�ZA�K�A�9XA�&�A�A��/A�ĜA։7A�|�A�r�A�hsA�\)A�I�A�(�A���A��A�r�A�-A�$�A�A���Aԝ�A�n�A�ffA�Q�A�I�A�A�A�33A� �A�VA���A���A��yA��A�Aӣ�A�33A�~�A�(�AэPA�ffA�"�A��mAа!A�G�Aϧ�A�7LA�%A���AάAΕ�A·+A�v�A�`BA�+A��/A̓A�/A�bA�bA�bA�VA�JA�oA�JA�
=A�1A�
=A�1A�
=A�1A�
=A�A���A��A��A��mA̲-A��A��A��mA��`A��`A��`A��`A��HA��A��#A��#A��#A��A��A��
A���A��
A��
A���A���A�ȴA˾wA˺^A˧�A˅A�dZA�G�A��A��#AʶFAʧ�AʓuAʇ+A�x�A�l�A�ffA�dZA�ZA�XA�O�A�E�A�9XA�/A�&�A� �A�{A�bA�A���A���A��A��mA��HA��/A���Aɟ�A�`BA�33A���A���A���A���A��A��A��A��A��yA��#A�AȍPA�VA�5?A��A�
=A��mAǲ-A��AƓuA�K�A�{A�  A��yA��A���AžwAŴ9Aţ�AōPAŁA�v�A�p�A�ffA�\)A�VA�S�A�S�A�M�A�E�A�?}A�9XA�7LA�+A�$�A��A�bA�%A�%A���A��yA��HA��;A���A���AļjAĸRAĴ9Aİ!Aħ�Aĥ�Ağ�Aě�AčPA�XA�(�A���A�t�A�VA���A��A��HA£�A�l�A�\)A�S�A�M�A�G�A�A�A�A�A�A�A�?}A�;dA�33A�33A�33A�(�A�$�A�{A���A��HA�ƨA���A�|�A�K�A��A��A���A��FA���A��DA�dZA�E�A�33A�(�A�oA�A��A��yA��`A��HA��;A��/A��#A���A��^A���A�r�A�A�A�oA�ĜA�VA��mA��A�t�A�1A��A��A��A��yA��mA���A���A��A��PA�n�A�I�A�5?A��A�
=A���A���A���A��
A��DA�+A��;A���A�`BA�1A��uA�l�A�33A�ĜA���A���A��hA��DA��+A��A�z�A�r�A�dZA�Q�A�?}A�/A� �A��A�
=A�  A���A��A��yA��HA��/A���A��jA��A�7LA�oA�1A��A�A�l�A��A�C�A�1A���A��;A���A��!A��7A�C�A�VA���A���A�z�A�dZA�I�A�&�A��A�
=A���A��HA�ȴA��9A���A��hA�bNA�-A�&�A� �A�A���A���A��A��yA��yA��A�A�ƨA��^A��wA��jA��9A��!A��A���A���A��7A�hsA�I�A�
=A��-A�Q�A��#A�|�A�XA�/A��A�%A��A�ƨA��-A���A���A��hA��7A��7A��7A��A�ffA�C�A�-A�oA��A���A�~�A�5?A�bA��mA���A��PA�hsA�=qA�-A��A�1A���A��yA��TA��
A�ƨA��jA��A���A��PA�t�A�^5A�I�A�A�ȴA��!A���A�t�A�~�A���A�"�A�ZA�VA���A���A��A��yA��
A��!A��PA�^5A�(�A�{A��`A��;A��TA��mA��A��mA��A���A�ȴA���A��wA�ĜA�ȴA�ƨA��wA��FA���A���A�|�A�n�A�M�A�"�A���A��A�dZA�C�A�1'A�$�A�$�A�$�A� �A��A��A��A�bA��A��A��`A��#A���A��^A�`BA�1'A�5?A��A�oA��#A��jA��hA�O�A�+A� �A�oA���A��A��A��HA�r�A��A���A�ffA�$�A���A���A�5?A��A���A�ffA�\)A�M�A�7LA�-A���A�E�A��mA��DA�"�A��TA��uA�?}A��A�t�A�S�A�$�A�
=A���A��#A��^A���A�|�A�^5A�K�A�33A��A���A��A��#A�A���A���A�z�A�hsA�C�A��A�ȴA�jA�$�A�JA��A��#A��A��A�A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
fB
	B
	lB
	7B
	�B
	7B
	7B
	�B
	lB
	B
	7B
	7B
	7B
	B
�B
fB
fB
fB
�B
	7B
	B
	B
�B
�B
	7B
	7B
	lB
�B
1B
	lB
�B
 �B
�B
�B
GB
YB
fB
B
oB
FB
%�B
0�B
E9B
RTB
jKB
j�B
hsB
d�B
\]B
a�B
ffB
g8B
�B
�B
��B
�B
��B
�B
�dB
�QB�B7BAUBE�BI�B]�Bm�Br�B��B�B��B�B�?B�`B�B�"B�B�B�B�B�BDBB iB��B�+B�B�vBרB�HB��B�EB�B��B�LB�B��B��B��B�uBm]BQ�BF�B<jB-�B!-B
�B
��B
ϫB
�B
��B
�AB
v�B
iB
IRB	��B	��B	��B	֡B	��B	�B	�hB	d&B	TaB	H�B	>wB	:^B	3�B	0!B	+�B	)�B	%zB	%FB	B	~B	$B	�B	 �B�B��B�;B��B�B�B� B��B��BуB��B՛B��BҽBѷB�,B��B��BںB��B�B�B�8B��B�B�8B�B��B�2B��B�8B�8B�2B�`B�TB�oB�cB��B�B��B�[BÖB� B� B�aB�B��B� B	fB	B	&LB	<B	AUB	N<B	EmB	DgB	P}B	jB	tTB	yrB	�B	�B	�+B	�B	��B	�kB	��B	�B	ΥB	�<B	�KB	�3B	��B	�jB	�gB	��B	��B	��B	�}B	��B	��B	� B	��B	��B	��B	��B	��B	�aB	�B	ɺB	�XB	��B	��B	��B	�6B	ϫB	�BB	�NB	��B	ԕB	�,B	�B	��B	�?B	�sB	֡B	�
B	�
B	�?B	רB	�B	��B	خB	�B	�B	�KB	�]B	��B	�#B	ںB	�B	ܒB	��B	��B	��B	یB	�WB	��B	چB	��B	�yB	רB	�gB	�&B	��B	�B	�XB	��B	�gB	�qB	�*B	��B	��B	�HB	�B	��B	�#B	ٴB	خB	ԕB	҉B	��B	�}B	�6B	ɺB	�RB	��B	��B	ӏB	��B	�B	бB	�NB	��B	��B	��B	��B	�&B	�aB	רB	�
B	�EB	�B	�QB	��B	��B	�#B	��B	�WB	�WB	�)B	ݘB	ߤB	�B	��B	�B	�B	��B	�HB	�B	یB	�sB	��B	�,B	�B	�)B	ݘB	�]B	چB	�#B	�jB	��B	�#B	�gB	�9B	��B	�B	�WB	��B	�,B	҉B	�NB	�B	��B	�B	�BB	�pB	��B	˒B	�0B	��B	�B	�B	�NB	ѷB	уB	��B	�TB	��B	҉B	��B	��B	�,B	�aB	�2B	�9B	�
B	�B	�B	ٴB	�#B	ܒB	��B	��B	�5B	�5B	�B	ߤB	�;B	�B	�BB	�BB	�BB	�B	�NB	�B	�B	�B	�2B	��B	�B	�B	�B	�
B	�
B	��B	�yB	�KB	��B	�B	�/B	�B	�B	�5B	�B	�iB	��B	�;B	�AB	�B	�B	��B	�TB	��B	��B	��B	��B	��B	�`B	�`B	��B	�lB	�	B	�	B	��B	��B	�>B	��B	�B	��B	�B	�JB	�B	�B	�B	�B	�B	�PB	�"B	�"B	�VB	�"B	�]B	��B	�.B	�cB
 4B
 iB
 �B
 �B
;B
�B
�B
B
B
AB
AB
�B
GB
�B
�B
B
�B
�B
�B
_B
�B
fB
1B
1B
�B
�B
�B
	lB
	�B
�B
�B
�B
DB

�B
xB
�B
�B
�B
"B
�B
�B
\B
�B
.B
�B
bB
bB
.B
�B
�B
bB
�B
�B
�B
�B
�B
 B
4B
4B
hB
hB
B
:B
B
�B
�B
uB
uB
uB
{B
{B
{B
�B
SB
�B
�B
SB
SB
�B
YB
�B
YB
YB
1B
eB
1B
1B
1B
�B
�B
�B
�B
B
�B
=B
�B
=B
	B
	B
�B
B
�B
CB
B
�B
CB
xB
CB
IB
~B
~B
~B
B
�B
�B
 'B
 �B
 �B
 �B
 �B
!-B
!-B
"4B
"�B
#:B
#�B
$B
$B
$�B
%zB
'B
&�B
'B
($B
(�B
)_B
(�B
(�B
(�B
*eB
)�B
*�B
*�B
+6B
+kB
+6B
,�B
,�B
,�B
,�B
-CB
-CB
-wB
.B
.IB
.IB
.�B
.�B
.�B
.�B
.�B
/�B
/OB
/�B
0UB
0�B
1'B
0�B
0�B
2-B
2aB
2-B
2aB
2aB
2�B
4B
4B
49B
4nB
4�B
5?B
5�B
6FB
6zB
6�B
6�B
7�B
7�B
7�B
8�B
8RB
8B
8�B
8RB
8RB
8B
8�B
8�B
9$B
9$B
9$B
9�B
9�B
:*B
:^B
;dB
:�B
:�B
;0B
;0B
;�B
:�B
<�B
=qB
<�B
=qB
>�B
>B
>BB
>�B
>wB
?�B
?}B
?�B
@�B
A B
AUB
A�B
A�B
A�B
B[B
B[B
B�B
B�B
C-B
B�B
CaB
CaB
C�B
CaB
D�B
D�B
E�B
E�B
E�B
F�B
F?B
GB
FtB
GEB
GB
G�B
G�B
HKB
HB
H�B
HB
HKB
IRB
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K)B
K^B
K�B
K�B
K^B
LdB
L�B
M�B
MjB
M�B
N�B
N<B
NB
N�B
NB
NB
N�B
OvB
OBB
O�B
PB
OvB
OBB
PB
O�B
P�B
P}B
PB
R B
QNB
QNB
R B
R�B
R�B
S&B
R�B
S�B
S�B
TaB
T�B
TaB
T�B
T�B
UgB
U2B
U�B
UgB
U�B
UgB
V9B
UgB
U�B
VB
U�B
VB
V9B
V�B
W?B
W
B
XB
W�B
W�B
W�B
XB
W�B
YKB
XyB
YKB
YKB
YKB
ZQB
Z�B
[WB
[WB
\]B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]/B
^B
^5B
]�B
^B
^�B
_;B
_B
_�B
_�B
`vB
`BB
_�B
_;B
`BB
aHB
`�B
aB
a|B
a�B
bNB
bNB
bB
bNB
b�B
b�B
b�B
b�B
c�B
d&B
dZB
c�B
d&B
c�B
c�B
d�B
d�B
e,B
e`B
e�B
e�B
f�B
ffB
f2B
f�B
gmB
gB
gmB
h>B
h
B
g�B
h�B
hsB
g�B
hsB
jB
jKB
jKB
jB
jB
jB
jB
jB
jB
i�B
j�B
jB
kQB
lWB
lWB
l�B
k�B
lWB
l"B
l�B
l�B
l�B
l�B
m]B
m)B
m�B
m�B
m�B
n�B
ncB
n/B
ncB
n�B
n�B
n�B
o5B
n�B
oiB
o�B
o�B
p�B
p�B
p�B
p�B
qB
p�B
qAB
qAB
qB
qvB
rB
qvB
rB
rGB
q�B
rB
q�B
r|B
rB
rGB
rGB
rGB
r�B
sMB
s�B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tTB
tTB
tB
tB
tTB
tTB
tTB
t�B
t�B
t�B
u%B
u%B
uZB
uZB
uZB
u%B
uZB
u�B
v`B
v`B
v�B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
v�B
v�B
v�B
v`B
v`B
v`B
v+B
v+B
v�B
v�B
v�B
wfB
wfB
w�B
w�B
xB
w�B
xB
w�B
w�B
xB
xB
xB
w�B
w�B
x8B
w�B
xlB
x�B
y>B
y>B
y>B
y>B
y�B
y�B
zB
zDB
zxB
zDB
zB
�B
xB
_B
B
	�B
	�B
�B
fB
	�B
�B
	lB
	�B

rB

	B
	�B
�B
fB
�B
�B
fB

=B

�B

=B
�B
�B
�B
�B
�B
1B
fB
	B
	B
	7B
	lB
	�B
	�B
	�B

rB

=B

rB
	lB
�B
1B
1B
�B
�B
�B
1B
�B

	B
�B
	B
	7B

	B
�B
1B
�B
_B
�B
�B
�B
�B
�B
	B
	lB
	B
	lB
1B
fB
�B
�B
�B
+B
1B
�B
	7B
	lB

=B

=B

	B

=B
	B
�B
�B
1B
1B
	B
�B
�B
fB
	lB
	lB

	B
	�B
fB
�B
�B
�B
1B
fB
	7B

=B
	�B
	lB
	B
1B
�B
_B
�B
�B
	lB

	B
	�B
	lB
	�B
DB
	B
	7B
�B
�B
	B
�B
	�B

	B
	�B
�B
	7B

rB
�B
fB
�B
	7B
	7B

rB

	B

=B
	7B
B
fB
	�B
VB
�B
�B
�B
JB
�B
�B

�B
_B
�B
	lB
	�B
+B
�B
�B
SB
�B
  B
 �B
2�B
�B
uB
�B
�B
;B
{B
(B
 �B

�B
1B
AB
�B	��B
oB	��B
�B
 4B	�cB
 4B
 �B
�B
{B
 �B
�B
uB
�B
B
�B
uB
�B
�B
B
B
MB
{B
�B
AB
�B
�B
{B
�B
�B
�B
�B
AB
B	�cB
B
B
�B
	�B
 �B
bB
DB
B
�B
�B
�B
�B
	lB
DB

�B
�B
�B
B
�B
B
�B

�B
	�B
VB
{B
kB
$B
�B
�B
B
$B
�B
\B
"B
uB
MB
eB
~B
"�B
$�B
%�B
'B
)�B
)�B
+�B
,=B
-�B
/B
3�B
2�B
<�B
<B
@�B
E�B
F�B
E�B
IRB
Q�B
T�B
J�B
E�B
]dB
^5B
V�B
OvB
Z�B
jB
q�B
k�B
n�B
m�B
l�B
i�B
k�B
m)B
l�B
iyB
k�B
kQB
jKB
jKB
iyB
g�B
h
B
f�B
iDB
jKB
f�B
gB
`vB
e,B
c�B
[�B
\�B
\]B
[�B
]�B
^jB
^�B
[�B
kB
VmB
]/B
bB
e�B
d�B
e`B
cTB
e�B
d&B
e�B
f�B
d�B
gB
h�B
g�B
f�B
iB
i�B
h�B
}VB
x�B
�_B
�4B
�B
��B
�B
�xB
�xB
��B
��B
��B
��B
�$B
�YB
��B
�B
�YB
��B
��B
��B
��B
�B
�tB
�B
��B
��B
�B
�RB
��B
��B
�$B
�$B
��B
�XB
��B
��B
��B
��B
�qB
�wB
��B
�B
�tB
ĜB
��B
��B
�[B
��B
B
��B
��B
� B
�UB
� B
��B
��B
��B
��B
��B
��B
�[B
�B
�B
�B
��B
��B
��B
�'B
�B
�NB
҉B
ԕB
��B
�9B
��B
چB
�#B
�WB
�5B
��B
�5B
�B
��B
�B
�B
�B
�B
�B
��B
�vB
�vB
ޞB
ߤB
�B
�BB
��B
�5B
��B
�8B
��B
�rB
�B
�	B
��B
�B
�8B
�8B
�lB
��B
�(B�B
�BVB�B�B�B~B1'B7�B:�B;dB9�B:�B;�B<B=<B=<B?}BA�B@�B@OB@�BA BA�BAUBA BA BB[BC�BCaBD�BC�BE�BEmBEmBDgBE�BD3BHKBGEBGzBHBIRBI�BK)BJ�BI�BJ�BK�BI�BHKBG�BK^BT,BQ�BXEBkQBg�Bb�Ba�Bc�BoiBm]Bl�Bn/Bn�Bn/Bm�Bm]Bm)Bm]Bn�BpoBm�Bm�Bl�Bl�Bp�Bo�Bq�Bu�Bv`B|B��B�YB��B��B��B��B�(B��B��B�B�FB��B�MB��B��B�B��B�MB�FB��B��B�YB�7B��B�!B��B�$B��B��B�<B�KB��BƨB��BÖB�'B��BƨB�EB��B�)B�dB͟B�B�BбB�pB��B�0B��B��B�pB��B�B�/B��B��B�B��B�B��B�>B��B�	B��B��B�B�B�B�B�"B��B�VB�JB��B�JB��B��B�PB��B��B�xB�B�B�B�.B��B;B�B�B7B!-B�B�B�B�B�B�B�B@B�BB.B�B�B�BVB�B�B�B�B"B�BB�B�BxB
�B�BxB�B�B
rB	B�B�BB1B�B_BxB	7BYB%B�BYB	�B	lB�B�BfBB:B	�B�B	�B �B�BuBSB�B�cB�cB �B��B�(B��BB�B�]B�.B�B�B��BB��B�.B�.B�JB�xB��B��B�fB�ZB�ZB�B��B�B�|B�B�|B��B�B��B��B��B�|B�WB�DB�B�B��B�B�.B�B�yB�B�NBуB�TB�NBҽBٴB�9B�,B�BԕB�KBʌBȀB�gB��B�XB�B�RBȀB�B�EB�?B�tB�KB�tBȴB�B��B�0B��BŢB͟B��B�OB��B��B��B�^B��B�$B��B�RB��B��B��B�B�nB�B��B��B��B�3B��B��B��B�}B�=B��B��B�0B�B��B��B�tB�B��B�CB�RB��B�B��B�oB��B�FB��B��B�"B��B��B��BzDB�7B��B{�B|�Bo�Bl�BhsBj�Bo BZ�BVBXEBQ�BO�BNpBO�BL0BL0BJ�BGEBE�BF?BC�BAUBA B?�B>B:�B<B8�B9$B8B:�B5tB-�B$�B%�B �B(XB3�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        B
1B
	B
	lB
	7B
	�B
	7B
	7B
	�B
	lB
	B
	7B
	7B
	RB
	B
�B
fB
fB
fB
�B
	7B
	B
	B
�B
�B
	7B
	RB
	�B
	�B
�B
B

	B
�B
?B
	7B
�B
�B
	�B
�B
�B
�B
(
B
5tB
JXB
U�B
l�B
l=B
kQB
hsB
b�B
fLB
i_B
rB
�vB
�4B
�nB
��B
B
��B
�B
��BBB;�BCGBH1BP�Bd�BpoBz�B�aB��B�uBЗB��B��B��B�B�ByB�B�B(B�B�B�B�B��B�xBuBܒB� B�#BˬBʦB��B��B�*B�|B��B��B��Bv�BW
BKxBEB>]B3�B
�hB
�=B
�jB
�+B
��B
�YB
�UB
�?B
d�B
�B	�B	�B	�QB	ٚB	�B	��B	n}B	\B	MPB	C-B	?�B	8�B	4B	1�B	,B	)�B	+6B	&�B	*0B	!B	
�B	SB	 4B��B�AB�B�wB�zB��B�B�QBޞB�BچB׍BӏB�FB��B�B��B��B�NB�@B�yB�qB�LB�DB�B�OB��B�	B��B��B��B��B�LB�>B�ZB��B��B��B�aB��BňB�oB��B��B�B�B��B	+B	B	$�B	;B	B�B	V�B	GEB	CaB	OvB	h�B	s�B	x�B	}�B	��B	�_B	�$B	��B	�kB	��B	�B	��B	�}B	�DB	ǔB	��B	�TB	��B	��B	��B	��B	��B	�{B	��B	B	��B	�[B	�{B	��B	�aB	��B	ʌB	��B	�xB	�B	��B	̈́B	��B	҉B	ѝB	��B	�{B	ՁB	�B	�?B	׍B	�B	خB	�B	�_B	�1B	�EB	�B	��B	ۦB	�B	�qB	ںB	�WB	ޞB	ܒB	�qB	�=B	�CB	�5B	��B	�)B	�)B	�CB	��B	�IB	�)B	�QB	�eB	�_B	�SB	��B	�&B	�4B	�B	��B	�KB	��B	��B	�DB	�*B	�uB	�B	�AB	�lB	یB	�kB	�gB	�[B	�B	�[B	�\B	ʌB	��B	�B	��B	�MB	��B	��B	ѝB	҉B	�aB	��B	ּB	յB	�aB	յB	��B	خB	ںB	یB	��B	�QB	ۦB	�CB	�xB	��B	یB	�B	ߤB	��B	�B	�B	�bB	��B	�nB	� B	�B	��B	��B	ٴB	ԯB	՛B	��B	��B	��B	�	B	��B	��B	ޞB	ޞB	��B	ּB	چB	��B	�IB	ٴB	��B	ҽB	ѝB	ѷB	�}B	� B	�B	��B	�~B	�~B	�B	�PB	��B	ϑB	��B	ѷB	ѝB	�:B	��B	��B	��B	�uB	�uB	�aB	ԯB	��B	֡B	��B	��B	�B	�kB	�]B	��B	�dB	�5B	ބB	ޞB	߾B	��B	߾B	��B	�vB	�\B	�'B	��B	�B	��B	�B	��B	�B	�fB	��B	�B	�B	�XB	�B	�DB	�0B	�B	��B	�cB	�}B	��B	��B	�B	��B	�B	�;B	��B	��B	��B	��B	�TB	�B	�B	��B	��B	�B	�B	��B	��B	�B	�$B	�rB	�$B	��B	�	B	�>B	��B	�JB	�B	��B	�6B	��B	�"B	�VB	�jB	��B	�"B	��B	�qB	��B	�B	��B	�cB	��B	��B
 �B
 �B
B
;B
�B
�B
'B
AB
AB
�B
�B
aB
�B
B
9B
�B
YB
zB
1B
�B
1B
�B
fB
�B
	B
	7B
�B
	�B

�B
~B
B
dB
�B
B
�B
PB
�B
B
pB
�B
�B
�B
bB
�B
HB
�B
�B
}B
�B
�B
�B
 B
 B
�B
 B
B
4B
NB
NB
�B
�B
�B
B
�B
�B
�B
�B
�B
FB
�B
�B
�B
�B
�B
B
B
�B
�B
sB
�B
�B
sB
�B
�B
B
1B
eB
B
B
�B
�B
B
�B
WB
qB
#B
qB
=B
�B
xB
�B
xB
�B
�B
�B
]B
�B
�B
B
B
�B
B
�B
 �B
 �B
!B
!-B
 �B
!HB
!B
!�B
!�B
#nB
#TB
#�B
#�B
$@B
$@B
$�B
%�B
'mB
'�B
'�B
(sB
(�B
)�B
(�B
(�B
)�B
*�B
*eB
+B
+kB
+�B
+�B
+�B
-CB
-B
,�B
,�B
-�B
-]B
-�B
.}B
.�B
.�B
/B
/ B
.�B
/B
/iB
0;B
/�B
0UB
0�B
1'B
1[B
1AB
2B
2�B
2�B
2|B
2�B
2�B
4B
4�B
4�B
4nB
4�B
5%B
5�B
6B
6�B
6�B
7B
7�B
88B
7�B
8�B
8�B
8RB
88B
8�B
8lB
8lB
8RB
9	B
8�B
9XB
9XB
9�B
:xB
:B
:�B
;B
;�B
:�B
;B
;B
;B
;�B
;�B
="B
=�B
="B
>B
>�B
>BB
>�B
?.B
?HB
@�B
?�B
@iB
AoB
B'B
A�B
A�B
B'B
B�B
B�B
C-B
C�B
CGB
CaB
CGB
C�B
C�B
D3B
C�B
ESB
ESB
F%B
FB
F%B
F�B
F�B
G+B
F�B
G�B
GzB
HKB
H1B
H�B
HKB
H�B
H1B
H�B
I�B
IB
I�B
I�B
J	B
I�B
JXB
J�B
KB
K�B
K�B
K�B
K�B
K�B
MB
MPB
NB
M�B
NB
N�B
NVB
N<B
N�B
N"B
N<B
OB
PB
O�B
O�B
PB
O�B
O�B
PbB
PHB
P�B
P�B
P�B
R�B
QhB
Q�B
S&B
S@B
R�B
SuB
SuB
TFB
TFB
T�B
T�B
T�B
UB
U�B
VB
U�B
U�B
U�B
U�B
U�B
V9B
U�B
VB
V9B
U�B
VB
VSB
W
B
WsB
WYB
X_B
W�B
W�B
XB
XyB
X�B
Y�B
X�B
YB
YB
Y�B
Z�B
[WB
[�B
[�B
\�B
\B
[�B
[�B
\B
\�B
\�B
\�B
]/B
]�B
^jB
^jB
]�B
^OB
_!B
_�B
_pB
_�B
`\B
`�B
`\B
`B
_�B
`�B
a|B
aB
aHB
a�B
bNB
c B
bhB
bB
bNB
b�B
b�B
c:B
c�B
dtB
d@B
dZB
dB
dZB
dB
d&B
d�B
d�B
e`B
e�B
f2B
f2B
f�B
f�B
f�B
gB
g�B
gRB
g�B
h�B
hXB
h
B
h�B
h�B
hsB
i_B
j�B
j�B
jB
j�B
j�B
j�B
j�B
j�B
jKB
jB
kB
j�B
lB
l�B
l�B
l�B
lB
l�B
lWB
l�B
l�B
l�B
l�B
m�B
mCB
m�B
nIB
m�B
n�B
n}B
nIB
n}B
n�B
oB
n�B
oOB
o B
o�B
p!B
p!B
qB
p�B
p�B
p�B
qB
p�B
q[B
q[B
q[B
q�B
r-B
q�B
r-B
raB
q�B
rB
q�B
r�B
r-B
raB
raB
r�B
sB
s�B
s�B
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
tnB
tTB
t9B
tB
tnB
tnB
tnB
t�B
t�B
u%B
u%B
u?B
uZB
utB
utB
u?B
u�B
u�B
v�B
vzB
v�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
v�B
v�B
v�B
v�B
v�B
vzB
vFB
v�B
wB
v�B
wLB
w�B
w�B
w�B
w�B
xB
w�B
xB
w�B
w�B
xB
xB
xB
xB
w�B
xRB
xB
x�B
x�B
yXB
yXB
yXB
yXB
y�B
zB
z*B
z^B
zxB
zDG�O�B
�B
xB
_B
B
	�B
	�B
�B
fB
	�B
�B
	lB
	�B

rB

	B
	�B
�B
fB
�B
�B
fB

=B

�B

=B
�B
�B
�B
�B
�B
1B
fB
	B
	B
	7B
	lB
	�B
	�B
	�B

rB

=B

rB
	lB
�B
1B
1B
�B
�B
�B
1B
�B

	B
�B
	B
	7B

	B
�B
1B
�B
_B
�B
�B
�B
�B
�B
	B
	lB
	B
	lB
1B
fB
�B
�B
�B
+B
1B
�B
	7B
	lB

=B

=B

	B

=B
	B
�B
�B
1B
1B
	B
�B
�B
fB
	lB
	lB

	B
	�B
fB
�B
�B
�B
1B
fB
	7B

=B
	�B
	lB
	B
1B
�B
_B
�B
�B
	lB

	B
	�B
	lB
	�B
DB
	B
	7B
�B
�B
	B
�B
	�B

	B
	�B
�B
	7B

rB
�B
fB
�B
	7B
	7B

rB

	B

=B
	7B
B
fB
	�B
VB
�B
�B
�B
JB
�B
�B

�B
_B
�B
	lB
	�B
+B
�B
�B
SB
�B
  B
 �B
2�B
�B
uB
�B
�B
;B
{B
(B
 �B

�B
1B
AB
�B	��B
oB	��B
�B
 4B	�cB
 4B
 �B
�B
{B
 �B
�B
uB
�B
B
�B
uB
�B
�B
B
B
MB
{B
�B
AB
�B
�B
{B
�B
�B
�B
�B
AB
B	�cB
B
B
�B
	�B
 �B
bB
DB
B
�B
�B
�B
�B
	lB
DB

�B
�B
�B
B
�B
B
�B

�B
	�B
VB
{B
kB
$B
�B
�B
B
$B
�B
\B
"B
uB
MB
eB
~B
"�B
$�B
%�B
'B
)�B
)�B
+�B
,=B
-�B
/B
3�B
2�B
<�B
<B
@�B
E�B
F�B
E�B
IRB
Q�B
T�B
J�B
E�B
]dB
^5B
V�B
OvB
Z�B
jB
q�B
k�B
n�B
m�B
l�B
i�B
k�B
m)B
l�B
iyB
k�B
kQB
jKB
jKB
iyB
g�B
h
B
f�B
iDB
jKB
f�B
gB
`vB
e,B
c�B
[�B
\�B
\]B
[�B
]�B
^jB
^�B
[�B
kB
VmB
]/B
bB
e�B
d�B
e`B
cTB
e�B
d&B
e�B
f�B
d�B
gB
h�B
g�B
f�B
iB
i�B
h�B
}VB
x�B
�_B
�4B
�B
��B
�B
�xB
�xB
��B
��B
��B
��B
�$B
�YB
��B
�B
�YB
��B
��B
��B
��B
�B
�tB
�B
��B
��B
�B
�RB
��B
��B
�$B
�$B
��B
�XB
��B
��B
��B
��B
�qB
�wB
��B
�B
�tB
ĜB
��B
��B
�[B
��B
B
��B
��B
� B
�UB
� B
��B
��B
��B
��B
��B
��B
�[B
�B
�B
�B
��B
��B
��B
�'B
�B
�NB
҉B
ԕB
��B
�9B
��B
چB
�#B
�WB
�5B
��B
�5B
�B
��B
�B
�B
�B
�B
�B
��B
�vB
�vB
ޞB
ߤB
�B
�BB
��B
�5B
��B
�8B
��B
�rB
�B
�	B
��B
�B
�8B
�8B
�lB
��B
�(B�B
�BVB�B�B�B~B1'B7�B:�B;dB9�B:�B;�B<B=<B=<B?}BA�B@�B@OB@�BA BA�BAUBA BA BB[BC�BCaBD�BC�BE�BEmBEmBDgBE�BD3BHKBGEBGzBHBIRBI�BK)BJ�BI�BJ�BK�BI�BHKBG�BK^BT,BQ�BXEBkQBg�Bb�Ba�Bc�BoiBm]Bl�Bn/Bn�Bn/Bm�Bm]Bm)Bm]Bn�BpoBm�Bm�Bl�Bl�Bp�Bo�Bq�Bu�Bv`B|B��B�YB��B��B��B��B�(B��B��B�B�FB��B�MB��B��B�B��B�MB�FB��B��B�YB�7B��B�!B��B�$B��B��B�<B�KB��BƨB��BÖB�'B��BƨB�EB��B�)B�dB͟B�B�BбB�pB��B�0B��B��B�pB��B�B�/B��B��B�B��B�B��B�>B��B�	B��B��B�B�B�B�B�"B��B�VB�JB��B�JB��B��B�PB��B��B�xB�B�B�B�.B��B;B�B�B7B!-B�B�B�B�B�B�B�B@B�BB.B�B�B�BVB�B�B�B�B"B�BB�B�BxB
�B�BxB�B�B
rB	B�B�BB1B�B_BxB	7BYB%B�BYB	�B	lB�B�BfBB:B	�B�B	�B �B�BuBSB�B�cB�cB �B��B�(B��BB�B�]B�.B�B�B��BB��B�.B�.B�JB�xB��B��B�fB�ZB�ZB�B��B�B�|B�B�|B��B�B��B��B��B�|B�WB�DB�B�B��B�B�.B�B�yB�B�NBуB�TB�NBҽBٴB�9B�,B�BԕB�KBʌBȀB�gB��B�XB�B�RBȀB�B�EB�?B�tB�KB�tBȴB�B��B�0B��BŢB͟B��B�OB��B��B��B�^B��B�$B��B�RB��B��B��B�B�nB�B��B��B��B�3B��B��B��B�}B�=B��B��B�0B�B��B��B�tB�B��B�CB�RB��B�B��B�oB��B�FB��B��B�"B��B��B��BzDB�7B��B{�B|�Bo�Bl�BhsBj�Bo BZ�BVBXEBQ�BO�BNpBO�BL0BL0BJ�BGEBE�BF?BC�BAUBA B?�B>B:�B<B8�B9$B8B:�B5tB-�B$�B%�B �B(XB3�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*.H<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<T�<Se�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<W�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<X�2<J�'<#�
<f��<#�
<#�
<<�p<J�'<#�
<#�
<#�
<#�
<6��<#�
<#�
<#�
<#�
<-�!<�D�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Rd{<#�
<nb�<)[�<#�
<#�
<#�
<��><�Ϭ<#�
<�/;<wUR<|�<#�
<#�
<:�l<���<��<#�
<#�
<#�
<#�
<:�l<Ï�<���<4��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<X.<.�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�/;<#�
<#�
<#�
<]ŗ<uNi<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<R��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2018101813281820181018132818IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102817064020181028170640QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102817064020181028170640QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107544720190521075447IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242020230426192420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                