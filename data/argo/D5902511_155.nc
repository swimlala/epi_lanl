CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-19T20:55:55Z creation; 2021-04-29T20:27:09Z DMQC;      
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20201219205555  20210429202818  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_155                 6810_008521_155                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�P1��	l@�P1��	l11  @�P1���@�P1���@2�5�I�P@2�5�I�P�ed���,�ed���,11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	?�=q@   @=p�@�  @�  @�  @�\A ��A  A\)A,(�A@��A_\)A�  A���A�Q�A�  A�  A�  A߮A�  B (�B�
B�B�
B�
B'�
B/�
B8  B@(�BH  BP  BX  B_�Bg�
Bp  Bx  B�  B�{B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��
B�  B�{B�{B��B��B��B�  B�{B�  B�  B�  B�  B�  B�  B��B��B�  B�{B�{C {C
=C
=C  C  C
  C{C{C  C��C��C�C��C  C  C��C   C"
=C${C&{C(
=C*  C,
=C-��C/��C1��C3��C5�C7��C:  C;��C>
=C@
=CB
=CD
=CF  CH{CJ
=CK��CM��CP  CR  CS�CU�CW�CY�C[��C^{C`
=Ca��Cc�Ce��Cg�Ci�Ck��Cn
=Co�Cq��Ct{Cu��Cw�Cz
=C|
=C~  C��C���C���C���C���C�  C���C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C���C�C�C�  C�
=C�C�C�C���C�  C�  C�  C���C�  C�  C�  C�  C���C���C���C���C���C���C�  C���C���C���C���C�C���C���C�C�  C���C�C�  C�  C�  C�C�C�C���C�  C�C�  C���C���C���C�  C���C���C���C�C�C�
=C���C�  C�\C�  C���C���C��C���C�C���C���C���C���C���C�  C�C�  C�C�C���C�C�  C���C���C�  C�  C���C���C�  C�
=C�
=C�  C�  C�  C���C���C���C�  C���C���C�C�  C�  C�  C���C���C�  C���C���C�C�C�C�  C�  C���C���D � D�D��D�D��D  D}qD�qD� D�D��D�D��D  D}qD  D��D	D	� D	�qD
� D  D� D  D}qD�qD� D  D� D�D��D�D� D�D��D  Dz�D  D�D  D� D�D� D��D}qD�qD}qD�qD}qD��D� D  D}qD  D� D�qDz�D��D}qD��D}qD�D� D   D � D!  D!� D!�qD"z�D"�qD#� D$�D$� D%  D%��D&�D&}qD'  D'}qD(  D(��D)�D)� D*�D*��D+�D+� D+�qD,}qD-  D-� D.  D.��D/�D/� D0  D0��D1�D1}qD2  D2��D3  D3� D4�D4��D5  D5� D5�qD6z�D7  D7� D7�qD8}qD9  D9� D9�qD:z�D:��D;��D<  D<� D=�D=� D>  D>��D>�qD?}qD?�qD@}qD@�qDA��DB�DB}qDC  DC��DD  DD}qDD�qDE}qDE��DFz�DF��DG� DH  DH��DI  DI� DJ�DJ��DK�DK� DK�qDL� DM  DM}qDM�qDN��DO�DO}qDO�qDP}qDP�qDQ}qDR  DR}qDS�DS�DT�DT��DU  DU� DV  DV� DW�DW��DX  DX� DX�qDY� DY�qDZ}qDZ�qD[}qD\  D\� D\�qD]� D^  D^� D_  D_� D`�D`��Da  Da��Db�Dbz�Db�qDc��Dd  Dd� De�De�Df  Df}qDf�qDg��Dh�Dh� Dh��Di� DjDj�Dk  Dk}qDl�Dl� Dl��Dm}qDm�qDnz�Dn�qDo� Dp�Dp��Dp�qDq� Dr  Dr��Ds�Ds}qDs�qDt� Du�Du��Dv  Dv� Dv��Dwz�Dx  Dx� Dx�qDy� Dz�Dz� D{  D{� D|  D|��D}�D}�D~�D~z�D~�RD� D�  D�>�D�~�D��HD�HD�@ D�� D���D���D�>�D�� D�� D�HD�B�D���D�� D�  D�AHD��HD�� D�  D�>�D�~�D�� D�  D�AHD��HD�� D�  D�AHD���D�D�HD�@ D�~�D�� D��qD�>�D�~�D�� D���D�>�D�� D���D���D�AHD�� D���D���D�AHD�� D���D���D�@ D�~�D�� D�  D�>�D�� D��HD�  D�@ D�� D��HD�  D�>�D�� D���D��qD�>�D�� D�D���D�@ D��HD���D���D�>�D�� D��HD���D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D��qD���D�@ D��HD�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��HD�  D�>�D�� D���D���D�@ D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D�� D�HD�@ D�~�D�� D�  D�@ D���D�� D�  D�B�D�� D���D�HD�@ D�~�D���D�  D�AHD��HD��HD��D�AHD�~�D�� D���D�>�D�~�D�� D��D�@ D�� D�� D���D�@ D�� D�� D��D�B�D��HD�D�HD�AHD��HD�� D���D�>�D�~�D���D�  D�AHD�� D��HD���D�=qD�|)D���D�  D�>�D�~�D��HD�  D�<)D�~�D��HD�  D�>�D��HD�D��D�B�D��HD�� D���D�=qD�}qD���D�HD�AHD�� D�� D�HD�AHD���D���D�HD�=qD�}qD�� D��D�@ D�~�D�� D�  D�>�D�~�D��qD��qD�<)D�~�D��HD�  D�@ D�� D��HD�HD�@ D��HD���D��qD�@ D��HD���D�  D�<)D�~�D�� D���D�@ D�� D�� D�  D�=qD�~�D�� D�HD�@ D�� D��HD���D�@ D�� D�� D�  D�@ D�� D��qD�  D�B�D�~�D��HD��D�>�D�}qDþ�D�  D�AHDāHDľ�D���D�@ D�~�D�� D��D�AHDƁHD�� D���D�>�D�~�DǾ�D�  D�@ DȀ D��HD�  D�@ D�~�Dɾ�D�  D�>�Dʀ D��HD��D�AHDˀ D˾�D���D�B�D́HD�� D�HD�AHD́HD��HD�  D�@ D�~�D�� D���D�@ Dς�D�� D�  D�>�DЀ D��HD���D�=qD�}qDѽqD�HD�B�DҀ D��HD�HD�>�DӀ D�� D���D�=qD�~�DԾ�D�  D�AHDՀ D�� D�  D�@ DցHD��HD�  D�@ DׁHD�D�HD�>�D؀ D�� D��qD�=qDـ D�� D�  D�>�Dڀ D��HD�HD�@ Dۀ D��HD�HD�@ D܀ D��HD���D�=qD݀ D��HD�  D�>�D�~�D��HD��D�AHD߀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�DᾸD��qD�>�D�~�D⾸D�  D�B�DわD�D�HD�@ D�~�D侸D�  D�@ D� D�� D�  D�@ D� D��HD�HD�>�D�~�D�� D�HD�AHD肏D�� D�  D�@ D�~�D�� D�  D�@ DꂏD�D�  D�>�D� D��HD���D�=qD� D��HD�HD�@ D�HD��HD�HD�AHD� D��HD���D�>�D� D��HD�HD�AHD�� D��HD��D�AHD� D�D���D�@ D� D�� D�HD�@ D�~�D�D�  D�AHD� D�� D��D�AHD�� D��HD���D�@ D��HD�� D�HD�>�D�~�D��HD�  D�>�D�~�D���D�  D�>�D�aH?#�
?.{?aG�?��?���?�Q�?�
=@�\@�@��@#�
@.{@=p�@O\)@Q�@fff@n{@xQ�@�ff@�=q@���@��H@��R@�ff@�{@�z�@��H@��
@�ff@�\)@�
=@�(�@�\@�@�{@���@�p�A�\A
=A��A�RA�\Az�A��A��A\)A$z�A(��A+�A/\)A4z�A7
=A<(�A@  AC33AG�AL��AO\)AS33AW�A[�A^�RAdz�Ag
=Ak�AqG�As�
AxQ�A|��A���A���A�z�A�A��A�=qA��A�p�A�Q�A�G�A�33A�p�A��RA�Q�A��HA�(�A�{A�Q�A���A��HA�A�
=A�Q�A�33A��A�{A���A�=qA�(�A��RA�Q�A�=qA��A�
=A�Q�A�33A�p�AƸRA�G�A��
A�p�A�  A�=qA��
AָRAأ�Aڏ\A��A�
=A�G�A��
A�p�A�Q�A�=qA��
A�RA��A�=qA���A�ffA���A�33A�z�A��RB z�B�B�\B\)BQ�B��B�\B�B��B	B
�\B(�B�B�B�Bz�B��B
=B  B��B�\B�B��B{B
=B(�BBffB�
B!G�B!�B#33B$��B%��B&�RB((�B)p�B*=qB+�B,��B-B/33B0��B1G�B2�HB4(�B4��B6�\B7�B8��B:=qB;\)B<Q�B=B>�HB@  BA��BB�\BC�BD��BE�BF�HBH��BIG�BJ�\BK�
BLz�BM�BO33BP  BP��BRffBS
=BTQ�BU��BVffBW\)BX��BY��BZffB[�B\��B]B^�\B_�
B`��Ba��Bb�HBd  Bd��Be�Bf�HBg�Bi�Bi�Bj�RBk�
Bm�Bm��Bo
=Bp  Bp��BqBr�HBs�Bt��Bu�Bv�\Bw�Bx��ByG�Bz�\B{�B|(�B}p�B~=qB
=B�(�B���B���B��B�(�B�z�B���B��B�{B�z�B��B��B�  B���B�
=B���B�Q�B��RB��B��B�ffB��RB�\)B��B�Q�B���B�p�B�  B�Q�B���B��B��
B�ffB�
=B�\)B��
B��\B���B�\)B�  B�Q�B��RB�\)B��B�Q�B���B��B���B�=qB�z�B���B���B��
B�ffB���B�33B�B�Q�B��\B���B��B�  B�=qB��HB��B��B�{B�ffB��RB�p�B�B�{B��\B��B�p�B�B�ffB���B�
=B���B�(�B�ffB��HB�p�B�B�{B���B��B�\)B��B�Q�B��\B��B���B��
B�Q�B���B�
=B�p�B�  B�=qB��\B��B��B�B�Q�B���B�
=B���B�=qB�z�B�
=B���B��
B�=qB��HB�33B��B�(�B��\B��HB��B�  B�Q�B��HB�p�B��B�=qB���B�\)B�B�ffB���B�33B�B�ffB���B�33B��
B�Q�B���B�33B��
B�=qB£�B�G�B��
B�{Bģ�B�G�BŅB�{Bƣ�B�
=B�\)B��B�z�B���B�33B�B�Q�BʸRB�
=BˮB�(�B�z�B�
=B͙�B��B�Q�B�
=BυB��
B�ffB�
=B�p�B��
B�ffB�
=B�\)B�B�ffB���B�33B��
B�ffBָRB�33B�B�(�B؏\B��B�B�{B�z�B��BۅB��B�z�B��Bݙ�B��B�ffB�
=B�p�B��
B�z�B���B�G�B�B�Q�B���B��B�B�=qB�z�B��HB�p�B�  B�ffB�RB�33B��
B�Q�B��B��B�B�(�B��B���B�B�{B�z�B��HB�p�B�{B�\B���B�\)B�  B��\B���B�G�B��
B�ffB���B�G�B�B�Q�B���B��B���B�=qB���B�
=B��B�=qB��RB��B��B�{B��RB��B�p�B�  B���B�33B��B��B��\B��B�\)B��C G�C �C �RC  CG�Cz�C�C��CG�Cz�C�C��CG�Cp�C�C  C=qCp�C��C  C(�CffC�C  C33CffC��C��C=qCffC��C��C=qCp�C��C�C	=qC	ffC	��C	��C
=qC
p�C
��C
��C=qC�C�RC��CG�C�\C�RC  CQ�C��CC  C\)C��C�
C
=CQ�C��C�
C  CG�C��C�
C  C=qC�\C��C��CG�C�\C�
C
=C=qC�C�
C(�CQ�C��C�C(�C\)C��C��C33CffC�RC
=CQ�Cz�C��C(�CffC��C�C=qCz�C�C  CQ�Cz�CC�CffC�\C��C�CffC��C�C33CffC�C  CG�Cz�C�C  CQ�Cz�C�RC {C \)C �\C ��C!(�C!p�C!��C!�C"33C"z�C"�RC"�HC#=qC#�C#C#��C$G�C$�\C$C%  C%\)C%��C%��C&
=C&\)C&��C&�
C'�C'p�C'�RC'��C(33C(�\C(�
C)  C)G�C)��C)�HC*{C*p�C*�RC*�C+33C+�C+C+��C,G�C,��C,�HC-{C-Q�C-�C-��C.33C.ffC.�C/
=C/Q�C/�C/C0{C0ffC0��C0�
C133C1z�C1��C1�C2G�C2�C2�RC3
=C3Q�C3��C3�HC4�C4Q�C4��C4�C533C5\)C5��C6  C6(�C6ffC6�RC7  C733C7ffC7�C8  C8G�C8�C8�RC8��C9G�C9��C9�
C:{C:Q�C:��C:�HC;33C;�C;C;��C<G�C<��C<��C=(�C=ffC=�C>
=C>\)C>��C>�
C?{C?p�C?C@  C@=qC@z�C@CA{CAffCA�RCB  CB33CBz�CBCC{CCffCC�RCD  CDG�CD�CDCE
=CEQ�CE�CF  CFG�CFz�CFCG
=CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ?�=q@   @=p�@�  @�  @�  @�\A ��A  A\)A,(�A@��A_\)A�  A���A�Q�A�  A�  A�  A߮A�  B (�B�
B�B�
B�
B'�
B/�
B8  B@(�BH  BP  BX  B_�Bg�
Bp  Bx  B�  B�{B�  B�  B�{B�{B�{B�{B�  B�  B�  B��B��
B�  B�{B�{B��B��B��B�  B�{B�  B�  B�  B�  B�  B�  B��B��B�  B�{B�{C {C
=C
=C  C  C
  C{C{C  C��C��C�C��C  C  C��C   C"
=C${C&{C(
=C*  C,
=C-��C/��C1��C3��C5�C7��C:  C;��C>
=C@
=CB
=CD
=CF  CH{CJ
=CK��CM��CP  CR  CS�CU�CW�CY�C[��C^{C`
=Ca��Cc�Ce��Cg�Ci�Ck��Cn
=Co�Cq��Ct{Cu��Cw�Cz
=C|
=C~  C��C���C���C���C���C�  C���C���C�  C�  C�  C�C�  C�  C�  C���C���C�  C���C�C�C�  C�
=C�C�C�C���C�  C�  C�  C���C�  C�  C�  C�  C���C���C���C���C���C���C�  C���C���C���C���C�C���C���C�C�  C���C�C�  C�  C�  C�C�C�C���C�  C�C�  C���C���C���C�  C���C���C���C�C�C�
=C���C�  C�\C�  C���C���C��C���C�C���C���C���C���C���C�  C�C�  C�C�C���C�C�  C���C���C�  C�  C���C���C�  C�
=C�
=C�  C�  C�  C���C���C���C�  C���C���C�C�  C�  C�  C���C���C�  C���C���C�C�C�C�  C�  C���C���D � D�D��D�D��D  D}qD�qD� D�D��D�D��D  D}qD  D��D	D	� D	�qD
� D  D� D  D}qD�qD� D  D� D�D��D�D� D�D��D  Dz�D  D�D  D� D�D� D��D}qD�qD}qD�qD}qD��D� D  D}qD  D� D�qDz�D��D}qD��D}qD�D� D   D � D!  D!� D!�qD"z�D"�qD#� D$�D$� D%  D%��D&�D&}qD'  D'}qD(  D(��D)�D)� D*�D*��D+�D+� D+�qD,}qD-  D-� D.  D.��D/�D/� D0  D0��D1�D1}qD2  D2��D3  D3� D4�D4��D5  D5� D5�qD6z�D7  D7� D7�qD8}qD9  D9� D9�qD:z�D:��D;��D<  D<� D=�D=� D>  D>��D>�qD?}qD?�qD@}qD@�qDA��DB�DB}qDC  DC��DD  DD}qDD�qDE}qDE��DFz�DF��DG� DH  DH��DI  DI� DJ�DJ��DK�DK� DK�qDL� DM  DM}qDM�qDN��DO�DO}qDO�qDP}qDP�qDQ}qDR  DR}qDS�DS�DT�DT��DU  DU� DV  DV� DW�DW��DX  DX� DX�qDY� DY�qDZ}qDZ�qD[}qD\  D\� D\�qD]� D^  D^� D_  D_� D`�D`��Da  Da��Db�Dbz�Db�qDc��Dd  Dd� De�De�Df  Df}qDf�qDg��Dh�Dh� Dh��Di� DjDj�Dk  Dk}qDl�Dl� Dl��Dm}qDm�qDnz�Dn�qDo� Dp�Dp��Dp�qDq� Dr  Dr��Ds�Ds}qDs�qDt� Du�Du��Dv  Dv� Dv��Dwz�Dx  Dx� Dx�qDy� Dz�Dz� D{  D{� D|  D|��D}�D}�D~�D~z�D~�RD� D�  D�>�D�~�D��HD�HD�@ D�� D���D���D�>�D�� D�� D�HD�B�D���D�� D�  D�AHD��HD�� D�  D�>�D�~�D�� D�  D�AHD��HD�� D�  D�AHD���D�D�HD�@ D�~�D�� D��qD�>�D�~�D�� D���D�>�D�� D���D���D�AHD�� D���D���D�AHD�� D���D���D�@ D�~�D�� D�  D�>�D�� D��HD�  D�@ D�� D��HD�  D�>�D�� D���D��qD�>�D�� D�D���D�@ D��HD���D���D�>�D�� D��HD���D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D�~�D��qD���D�@ D��HD�� D���D�@ D��HD�� D���D�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D��HD�  D�>�D�� D���D���D�@ D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D�� D�HD�@ D�~�D�� D�  D�@ D���D�� D�  D�B�D�� D���D�HD�@ D�~�D���D�  D�AHD��HD��HD��D�AHD�~�D�� D���D�>�D�~�D�� D��D�@ D�� D�� D���D�@ D�� D�� D��D�B�D��HD�D�HD�AHD��HD�� D���D�>�D�~�D���D�  D�AHD�� D��HD���D�=qD�|)D���D�  D�>�D�~�D��HD�  D�<)D�~�D��HD�  D�>�D��HD�D��D�B�D��HD�� D���D�=qD�}qD���D�HD�AHD�� D�� D�HD�AHD���D���D�HD�=qD�}qD�� D��D�@ D�~�D�� D�  D�>�D�~�D��qD��qD�<)D�~�D��HD�  D�@ D�� D��HD�HD�@ D��HD���D��qD�@ D��HD���D�  D�<)D�~�D�� D���D�@ D�� D�� D�  D�=qD�~�D�� D�HD�@ D�� D��HD���D�@ D�� D�� D�  D�@ D�� D��qD�  D�B�D�~�D��HD��D�>�D�}qDþ�D�  D�AHDāHDľ�D���D�@ D�~�D�� D��D�AHDƁHD�� D���D�>�D�~�DǾ�D�  D�@ DȀ D��HD�  D�@ D�~�Dɾ�D�  D�>�Dʀ D��HD��D�AHDˀ D˾�D���D�B�D́HD�� D�HD�AHD́HD��HD�  D�@ D�~�D�� D���D�@ Dς�D�� D�  D�>�DЀ D��HD���D�=qD�}qDѽqD�HD�B�DҀ D��HD�HD�>�DӀ D�� D���D�=qD�~�DԾ�D�  D�AHDՀ D�� D�  D�@ DցHD��HD�  D�@ DׁHD�D�HD�>�D؀ D�� D��qD�=qDـ D�� D�  D�>�Dڀ D��HD�HD�@ Dۀ D��HD�HD�@ D܀ D��HD���D�=qD݀ D��HD�  D�>�D�~�D��HD��D�AHD߀ D�� D�  D�>�D�~�D�� D�HD�@ D�~�DᾸD��qD�>�D�~�D⾸D�  D�B�DわD�D�HD�@ D�~�D侸D�  D�@ D� D�� D�  D�@ D� D��HD�HD�>�D�~�D�� D�HD�AHD肏D�� D�  D�@ D�~�D�� D�  D�@ DꂏD�D�  D�>�D� D��HD���D�=qD� D��HD�HD�@ D�HD��HD�HD�AHD� D��HD���D�>�D� D��HD�HD�AHD�� D��HD��D�AHD� D�D���D�@ D� D�� D�HD�@ D�~�D�D�  D�AHD� D�� D��D�AHD�� D��HD���D�@ D��HD�� D�HD�>�D�~�D��HD�  D�>�D�~�D���D�  D�>�G�O�?#�
?.{?aG�?��?���?�Q�?�
=@�\@�@��@#�
@.{@=p�@O\)@Q�@fff@n{@xQ�@�ff@�=q@���@��H@��R@�ff@�{@�z�@��H@��
@�ff@�\)@�
=@�(�@�\@�@�{@���@�p�A�\A
=A��A�RA�\Az�A��A��A\)A$z�A(��A+�A/\)A4z�A7
=A<(�A@  AC33AG�AL��AO\)AS33AW�A[�A^�RAdz�Ag
=Ak�AqG�As�
AxQ�A|��A���A���A�z�A�A��A�=qA��A�p�A�Q�A�G�A�33A�p�A��RA�Q�A��HA�(�A�{A�Q�A���A��HA�A�
=A�Q�A�33A��A�{A���A�=qA�(�A��RA�Q�A�=qA��A�
=A�Q�A�33A�p�AƸRA�G�A��
A�p�A�  A�=qA��
AָRAأ�Aڏ\A��A�
=A�G�A��
A�p�A�Q�A�=qA��
A�RA��A�=qA���A�ffA���A�33A�z�A��RB z�B�B�\B\)BQ�B��B�\B�B��B	B
�\B(�B�B�B�Bz�B��B
=B  B��B�\B�B��B{B
=B(�BBffB�
B!G�B!�B#33B$��B%��B&�RB((�B)p�B*=qB+�B,��B-B/33B0��B1G�B2�HB4(�B4��B6�\B7�B8��B:=qB;\)B<Q�B=B>�HB@  BA��BB�\BC�BD��BE�BF�HBH��BIG�BJ�\BK�
BLz�BM�BO33BP  BP��BRffBS
=BTQ�BU��BVffBW\)BX��BY��BZffB[�B\��B]B^�\B_�
B`��Ba��Bb�HBd  Bd��Be�Bf�HBg�Bi�Bi�Bj�RBk�
Bm�Bm��Bo
=Bp  Bp��BqBr�HBs�Bt��Bu�Bv�\Bw�Bx��ByG�Bz�\B{�B|(�B}p�B~=qB
=B�(�B���B���B��B�(�B�z�B���B��B�{B�z�B��B��B�  B���B�
=B���B�Q�B��RB��B��B�ffB��RB�\)B��B�Q�B���B�p�B�  B�Q�B���B��B��
B�ffB�
=B�\)B��
B��\B���B�\)B�  B�Q�B��RB�\)B��B�Q�B���B��B���B�=qB�z�B���B���B��
B�ffB���B�33B�B�Q�B��\B���B��B�  B�=qB��HB��B��B�{B�ffB��RB�p�B�B�{B��\B��B�p�B�B�ffB���B�
=B���B�(�B�ffB��HB�p�B�B�{B���B��B�\)B��B�Q�B��\B��B���B��
B�Q�B���B�
=B�p�B�  B�=qB��\B��B��B�B�Q�B���B�
=B���B�=qB�z�B�
=B���B��
B�=qB��HB�33B��B�(�B��\B��HB��B�  B�Q�B��HB�p�B��B�=qB���B�\)B�B�ffB���B�33B�B�ffB���B�33B��
B�Q�B���B�33B��
B�=qB£�B�G�B��
B�{Bģ�B�G�BŅB�{Bƣ�B�
=B�\)B��B�z�B���B�33B�B�Q�BʸRB�
=BˮB�(�B�z�B�
=B͙�B��B�Q�B�
=BυB��
B�ffB�
=B�p�B��
B�ffB�
=B�\)B�B�ffB���B�33B��
B�ffBָRB�33B�B�(�B؏\B��B�B�{B�z�B��BۅB��B�z�B��Bݙ�B��B�ffB�
=B�p�B��
B�z�B���B�G�B�B�Q�B���B��B�B�=qB�z�B��HB�p�B�  B�ffB�RB�33B��
B�Q�B��B��B�B�(�B��B���B�B�{B�z�B��HB�p�B�{B�\B���B�\)B�  B��\B���B�G�B��
B�ffB���B�G�B�B�Q�B���B��B���B�=qB���B�
=B��B�=qB��RB��B��B�{B��RB��B�p�B�  B���B�33B��B��B��\B��B�\)B��C G�C �C �RC  CG�Cz�C�C��CG�Cz�C�C��CG�Cp�C�C  C=qCp�C��C  C(�CffC�C  C33CffC��C��C=qCffC��C��C=qCp�C��C�C	=qC	ffC	��C	��C
=qC
p�C
��C
��C=qC�C�RC��CG�C�\C�RC  CQ�C��CC  C\)C��C�
C
=CQ�C��C�
C  CG�C��C�
C  C=qC�\C��C��CG�C�\C�
C
=C=qC�C�
C(�CQ�C��C�C(�C\)C��C��C33CffC�RC
=CQ�Cz�C��C(�CffC��C�C=qCz�C�C  CQ�Cz�CC�CffC�\C��C�CffC��C�C33CffC�C  CG�Cz�C�C  CQ�Cz�C�RC {C \)C �\C ��C!(�C!p�C!��C!�C"33C"z�C"�RC"�HC#=qC#�C#C#��C$G�C$�\C$C%  C%\)C%��C%��C&
=C&\)C&��C&�
C'�C'p�C'�RC'��C(33C(�\C(�
C)  C)G�C)��C)�HC*{C*p�C*�RC*�C+33C+�C+C+��C,G�C,��C,�HC-{C-Q�C-�C-��C.33C.ffC.�C/
=C/Q�C/�C/C0{C0ffC0��C0�
C133C1z�C1��C1�C2G�C2�C2�RC3
=C3Q�C3��C3�HC4�C4Q�C4��C4�C533C5\)C5��C6  C6(�C6ffC6�RC7  C733C7ffC7�C8  C8G�C8�C8�RC8��C9G�C9��C9�
C:{C:Q�C:��C:�HC;33C;�C;C;��C<G�C<��C<��C=(�C=ffC=�C>
=C>\)C>��C>�
C?{C?p�C?C@  C@=qC@z�C@CA{CAffCA�RCB  CB33CBz�CBCC{CCffCC�RCD  CDG�CD�CDCE
=CEQ�CE�CF  CFG�CFz�CFCG
=CGffCG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��;A��TA��TA��TA��`A��TA��`A��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A���A�A� �A�/A�E�A�K�A�?}A�v�A���Aؙ�A��AԍPAҝ�A�%A�VA�ĜA�
=AȶFAǬA���A��#A� �A�A�ZA��HAA�5?A��A�oA�1A���A��A��^A�z�A���A��\A�p�A�x�A�S�A��7A�;dA��DA�G�A�A�bA�+A���A��wA���A�dZA�%A���A�hsA��A�O�A�1'A���A�E�A��yA�Q�A���A�(�A�S�A���A��9A�$�A�1'A���A�$�A�\)A�+A�{A��-A�+A�Q�A��mA�O�A��A�Q�A��A��+A�p�A�A�A~��A~{A}S�A|~�Az��Ay�#Ay�7Ax��Aw��Awp�Aw�Au�Aq�-Ao��Am%Ak�Ak��Ak7LAj��Aj1Ah=qAeXAc?}A`�`A]��A\�!A[�AZ��AZz�AZ1AYAX��AV��AU33ASƨANr�AK&�AI�AHAD�yAB��A@n�A>�A<��A<�RA;7LA6ZA4�+A3��A3l�A2n�A1?}A/��A/33A.�`A.�uA-��A,�A,�A)t�A'�A%�;A$�A$~�A$bNA$A#p�A"�HA"^5A"Q�A!�7A $�A\)AffAA��A?}A
=A|�Az�A �A�A��A��At�AG�A/A�A~�A�hA�A�AA��A�AVA��A�jAv�A��At�A��A�A
�9A	��A	��A	hsA	/A�Al�A;dAVA�`A�!A=qA��A7LA�mA�A��A��A;dAA ~�@��@��F@�ff@�9X@�dZ@��y@�v�@�M�@�$�@��T@�hs@�v�@�~�@�ƨ@웦@��@��@�(�@�A�@�bN@�r�@�1'@�o@�~�@�I�@�@旍@��#@��
@���@��#@��@�l�@ݺ^@݁@�G�@ܛ�@��H@ؼj@Ձ@Ԭ@�Z@��@�t�@�
=@�X@���@Ь@�bN@�(�@��@�b@�l�@���@���@���@�@ɲ-@��T@��H@ʟ�@��#@��@�+@���@�33@�9X@�Z@�bN@ȃ@ȼj@��@Ų-@�ȴ@�hs@�&�@���@��j@�V@�O�@�Ĝ@�l�@��@���@�x�@��7@��h@��h@���@���@���@��h@�x�@�G�@���@���@��u@��@���@�C�@�ȴ@���@�v�@�33@�o@���@���@��R@�v�@���@�x�@�?}@���@��/@�Q�@���@���@�t�@�S�@�"�@���@�n�@��@��h@��7@�O�@���@���@�bN@�1'@���@��w@�t�@�"�@���@��R@��+@�^5@�@���@�@���@�`B@�V@���@�z�@�Z@�9X@�(�@�1@�  @���@�
=@��y@���@�=q@��#@���@�x�@�?}@��@��@�^5@�J@��T@���@�@�`B@��@��/@�bN@�(�@��m@�S�@��H@��R@��+@�n�@�^5@�^5@�=q@�@�V@��@��9@��9@��@���@���@��u@��D@�z�@�j@�Z@��;@��@�;d@���@�5?@�@��@���@��-@��@��/@��@���@�C�@���@�$�@���@�O�@�&�@��/@���@�bN@�b@��@�S�@�33@�ȴ@�v�@�M�@��@��-@��7@�p�@��@�Z@��;@��F@��@���@���@��#@��7@�p�@�`B@�O�@�O�@�G�@��`@��@��@�j@�ƨ@�+@���@�~�@�~�@�v�@�n�@�M�@�M�@�E�@�=q@�5?@�-@��T@�7L@��j@�bN@�A�@�b@��F@��P@�+@�@��@��H@��H@��R@�~�@�v�@�$�@�J@�@���@��@���@��-@���@�`B@�7L@�&�@�%@���@��;@�33@�@��@��H@��H@��\@�-@��@���@���@���@��@�O�@���@���@�j@�Z@�9X@�(�@��@��m@��F@�l�@�33@�@�ȴ@�J@�O�@�%@���@��j@���@��@�Q�@�1'@� �@��;@�33@��@���@�ȴ@�ff@���@��h@���@��D@�z�@� �@
=@~{@}�@}�@|�/@|j@{�m@{ƨ@{ƨ@{ƨ@{ƨ@{�
@{�
@{�
@{�@{o@z�!@y�^@y&�@xr�@xQ�@w��@w\)@w�@vȴ@vv�@v$�@u�-@u�@t��@t1@st�@s"�@r��@rn�@r�@q�^@qG�@p��@p��@pbN@o�@o
=@nv�@n$�@n@m@l��@lI�@l1@k�@k@j�!@jn�@j=q@j�@i�@i�^@i��@ix�@iG�@h��@hb@g;d@g+@fȴ@e�h@d9X@dZ@d(�@d�@d1@c�
@cdZ@b�@a�#@aG�@a&�@a�@a%@`bN@_�@_�;@_�;@_�w@_��@_l�@^V@^$�@]@]��@]��@]p�@]?}@]/@]�@]V@\�@\��@[�
@["�@Z�!@ZJ@YX@Y�@X��@X�@W�@W
=@V$�@U�T@U��@U�-@Up�@U/@T��@T�j@T��@TZ@T�@T�@S��@S�m@S��@S��@T1@T1@T1@S��@S��@St�@SS�@So@R�\@R-@Q��@QG�@PĜ@PĜ@PbN@P1'@P �@Pb@O��@O|�@O+@N�@Nff@M�h@MO�@M�@L1@K�@KdZ@K"�@J�\@JJ@I��@I�7@Ihs@I�@H��@HA�@H  @G�@G|�@G\)@G�@F��@Fȴ@Fv�@E�h@D��@D�/@D�j@Dz�@D9X@D�@D1@C�m@C�F@CC�@B�!@A��@A�7@Ahs@AX@AG�@A7L@@�`@@Q�@@b@?��@?�@?l�@?�@>�R@>�+@>E�@=@<��@<�D@<j@<Z@<9X@<1@;�m@;��@;dZ@;o@:�!@:~�@:~�@:^5@:-@9��@9hs@9G�@8��@8�@8Q�@7�;@7\)@6��@6ȴ@6��@6��@6��@6v�@6V@65?@6{@5�T@5`B@5/@5�@4�/@4�D@3��@3ƨ@3��@3�@3dZ@2��@2^5@2J@1��@1��@1�7@1&�@1&�@1�@0��@0r�@0A�@0b@/�@.�R@.��@.$�@-@-�@-`B@-`B@-O�@-/@,�@,�/@,�j@,z�@+��@+t�@+t�@+t�@+S�@+33@*��@*M�@)�^@)�7@)%@(r�@(  @'�w@'��@'l�@'K�@'+@'�@'�@&�@&�R@&�+@&$�@&@&@%�@%/@$��@$�@$��@$��@$�D@$9X@$�@#��@#�
@#�@#S�@#33@#@"�!@"=q@!��@!�#@!��@!��@!x�@!G�@ ��@ �u@ 1'@�w@K�@�@��@ȴ@�R@��@5?@@@@@?}@/@�@�@�D@j@��@�
@�F@33@��@M�@�@x�@7L@Ĝ@�u@bN@b@�w@K�@�y@ȴ@��@ff@E�@$�@{@�@�T@�-@�@O�@/@�@�j@��@z�@j@I�@�@��@��@�m@�
@ƨ@t�@C�@33@@�!@��@��@~�@^5@^5@M�@-@-@�@��@�@��@��@x�@&�@�@��@��@�9@��@�u@bN@Q�@A�@1'@b@b@  A��#A��/A��;A��HA��/A��A��HA��`A��;A��`A��TA��HA��`A��TA��HA��`A��TA��HA��mA��HA��mA��mA��HA��`A��`A��`A��HA��mA��HA��`A��A��`A��TA��mA��HA��mA��HA��`A��A��`A��A��A��TA��yA��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��`A��mA��`A��`A��A��mA��`A��A��`A��mA��A��`A��yA��A��mA��yA��A��A��yA��A��A��`A��A��yA��mA��yA��A��mA��A��A��yA��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A���A��A��A���A��A���A���A��A��A���A��A��A���A��A���A���A��A���A��A��A���A���A��A��A���A��A��A���A��A��A���A���A��A���A���A���A���A���A��A��A���A��A���A���A���A��A���A���A��A���A���A��A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�A�A�A�A�  A���A�  A�  A���A�  A�  A���A�  A�  A���A���A�A���A���A�  A���A���A���A��A�oA��A�oA�A�
=A�+A�1'A�$�A�(�A�+A�$�A�(�A�"�A� �A�$�A�/A�7LA�=qA�=qA�9XA�;dA�?}A�I�A�E�A�K�A�K�A�G�A�I�A�O�A�M�A�M�A�S�A�Q�A�I�A�E�A�E�A�?}A�;dA�C�A�A�A�=qA�?}A�A�A�=qA�Q�A�\)A�ffA�^5A�v�A؁A؁A؉7A���AؾwAضFA���A���A���AؾwA���A��TA���A��yA��HA���A؟�A�hsA�n�A�\)A�K�A��A׋DA�dZA�^5A��A��A֙�A�33A� �A�I�A�1'A���Aԇ+A�JA���AӬA�~�A�1AҼjAҮA�t�A�VA�bNA�K�A�M�A�I�A�=qA�JA�A���A��/AѺ^A��AѮAѕ�A�p�A�hsA�E�A�
=A�ƨA�O�A�t�AΕ�A͇+A��A�z�A�?}A��A��mA���A�ƨA˙�A�t�A�bNA�7LA��A�dZA�%A���A�v�A�{Aȴ9AȍPA�r�A�ZA�&�A�JA�1A��mA���A���Aǡ�AǅA�r�A�K�A��AƏ\A�A��
A���A���A���AŋDA�;dA� �A�bA���A�ĜAĺ^Aİ!Aġ�Aĉ7A�hsA�S�A�1'A��A�
=A���A��A��mA��`A��#A��A���A�ȴAç�AÓuAÍPAÁA�v�A�bNA�^5A�O�A�C�A�=qA�9XA��A�  A���A��#A�ƨA¾wAº^A´9A°!A©�A©�A�AA�^5A�K�A�?}A�=qA�5?A�5?A�9XA�9XA�1'A�-A�(�A� �A�"�A��A��A��A��A��A��A��A��A�bA�bA�bA�oA�bA�VA�bA�oA�VA�1A�A�%A���A��A��mA��`A���A���A��wA��jA��-A��\A��A��7A��A�
=A��#A�z�A�;dA�-A�&�A�(�A� �A��HA��A��RA�;dA�{A��+A�;dA�&�A�bA�A��`A���A�A���A���A��DA�r�A�bNA�7LA�bA�A���A���A��\A�jA���A��wA���A��A�p�A�VA�E�A� �A�
=A�A��#A��-A��A�S�A���A��!A���A��A�XA�M�A�G�A�;dA�1'A�"�A�1A��A�ĜA���A�p�A�?}A�VA��A���A�M�A�&�A��wA�JA��7A��A���A���A��A��A��7A��A�n�A�bNA�XA�XA�O�A�C�A�A�A�33A��A�{A�%A�  A���A��A�A�VA�A�%A�$�A�+A� �A�VA��A��
A���A���A�9XA��A�A���A���A���A�z�A��`A���A��hA�G�A�$�A�VA�A��A��A���A��FA�^5A�  A��TA��
A��7A��A���A�JA�7LA�M�A�{A���A���A��A�=qA�%A���A��A�~�A�hsA�1'A��A��A��uA��HA��hA�+A��jA�bNA�9XA��yA��A��\A��A�z�A�p�A�dZA�Q�A�-A�{A���A���A��A��DA�hsA�^5A�^5A�\)A�ZA�XA�Q�A�K�A�;dA��A�  A��PA�=qA��;A���A�hsA�C�A�-A��HA��-A�^5A��A�1A��;A��!A���A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 A��;A��TA��TA��TA��`A��TA��`A��`A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��mA��yA��A��yA��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A�  A���A�A� �A�/A�E�A�K�A�?}A�v�A���Aؙ�A��AԍPAҝ�A�%A�VA�ĜA�
=AȶFAǬA���A��#A� �A�A�ZA��HAA�5?A��A�oA�1A���A��A��^A�z�A���A��\A�p�A�x�A�S�A��7A�;dA��DA�G�A�A�bA�+A���A��wA���A�dZA�%A���A�hsA��A�O�A�1'A���A�E�A��yA�Q�A���A�(�A�S�A���A��9A�$�A�1'A���A�$�A�\)A�+A�{A��-A�+A�Q�A��mA�O�A��A�Q�A��A��+A�p�A�A�A~��A~{A}S�A|~�Az��Ay�#Ay�7Ax��Aw��Awp�Aw�Au�Aq�-Ao��Am%Ak�Ak��Ak7LAj��Aj1Ah=qAeXAc?}A`�`A]��A\�!A[�AZ��AZz�AZ1AYAX��AV��AU33ASƨANr�AK&�AI�AHAD�yAB��A@n�A>�A<��A<�RA;7LA6ZA4�+A3��A3l�A2n�A1?}A/��A/33A.�`A.�uA-��A,�A,�A)t�A'�A%�;A$�A$~�A$bNA$A#p�A"�HA"^5A"Q�A!�7A $�A\)AffAA��A?}A
=A|�Az�A �A�A��A��At�AG�A/A�A~�A�hA�A�AA��A�AVA��A�jAv�A��At�A��A�A
�9A	��A	��A	hsA	/A�Al�A;dAVA�`A�!A=qA��A7LA�mA�A��A��A;dAA ~�@��@��F@�ff@�9X@�dZ@��y@�v�@�M�@�$�@��T@�hs@�v�@�~�@�ƨ@웦@��@��@�(�@�A�@�bN@�r�@�1'@�o@�~�@�I�@�@旍@��#@��
@���@��#@��@�l�@ݺ^@݁@�G�@ܛ�@��H@ؼj@Ձ@Ԭ@�Z@��@�t�@�
=@�X@���@Ь@�bN@�(�@��@�b@�l�@���@���@���@�@ɲ-@��T@��H@ʟ�@��#@��@�+@���@�33@�9X@�Z@�bN@ȃ@ȼj@��@Ų-@�ȴ@�hs@�&�@���@��j@�V@�O�@�Ĝ@�l�@��@���@�x�@��7@��h@��h@���@���@���@��h@�x�@�G�@���@���@��u@��@���@�C�@�ȴ@���@�v�@�33@�o@���@���@��R@�v�@���@�x�@�?}@���@��/@�Q�@���@���@�t�@�S�@�"�@���@�n�@��@��h@��7@�O�@���@���@�bN@�1'@���@��w@�t�@�"�@���@��R@��+@�^5@�@���@�@���@�`B@�V@���@�z�@�Z@�9X@�(�@�1@�  @���@�
=@��y@���@�=q@��#@���@�x�@�?}@��@��@�^5@�J@��T@���@�@�`B@��@��/@�bN@�(�@��m@�S�@��H@��R@��+@�n�@�^5@�^5@�=q@�@�V@��@��9@��9@��@���@���@��u@��D@�z�@�j@�Z@��;@��@�;d@���@�5?@�@��@���@��-@��@��/@��@���@�C�@���@�$�@���@�O�@�&�@��/@���@�bN@�b@��@�S�@�33@�ȴ@�v�@�M�@��@��-@��7@�p�@��@�Z@��;@��F@��@���@���@��#@��7@�p�@�`B@�O�@�O�@�G�@��`@��@��@�j@�ƨ@�+@���@�~�@�~�@�v�@�n�@�M�@�M�@�E�@�=q@�5?@�-@��T@�7L@��j@�bN@�A�@�b@��F@��P@�+@�@��@��H@��H@��R@�~�@�v�@�$�@�J@�@���@��@���@��-@���@�`B@�7L@�&�@�%@���@��;@�33@�@��@��H@��H@��\@�-@��@���@���@���@��@�O�@���@���@�j@�Z@�9X@�(�@��@��m@��F@�l�@�33@�@�ȴ@�J@�O�@�%@���@��j@���@��@�Q�@�1'@� �@��;@�33@��@���@�ȴ@�ff@���@��h@���@��D@�z�@� �@
=@~{@}�@}�@|�/@|j@{�m@{ƨ@{ƨ@{ƨ@{ƨ@{�
@{�
@{�
@{�@{o@z�!@y�^@y&�@xr�@xQ�@w��@w\)@w�@vȴ@vv�@v$�@u�-@u�@t��@t1@st�@s"�@r��@rn�@r�@q�^@qG�@p��@p��@pbN@o�@o
=@nv�@n$�@n@m@l��@lI�@l1@k�@k@j�!@jn�@j=q@j�@i�@i�^@i��@ix�@iG�@h��@hb@g;d@g+@fȴ@e�h@d9X@dZ@d(�@d�@d1@c�
@cdZ@b�@a�#@aG�@a&�@a�@a%@`bN@_�@_�;@_�;@_�w@_��@_l�@^V@^$�@]@]��@]��@]p�@]?}@]/@]�@]V@\�@\��@[�
@["�@Z�!@ZJ@YX@Y�@X��@X�@W�@W
=@V$�@U�T@U��@U�-@Up�@U/@T��@T�j@T��@TZ@T�@T�@S��@S�m@S��@S��@T1@T1@T1@S��@S��@St�@SS�@So@R�\@R-@Q��@QG�@PĜ@PĜ@PbN@P1'@P �@Pb@O��@O|�@O+@N�@Nff@M�h@MO�@M�@L1@K�@KdZ@K"�@J�\@JJ@I��@I�7@Ihs@I�@H��@HA�@H  @G�@G|�@G\)@G�@F��@Fȴ@Fv�@E�h@D��@D�/@D�j@Dz�@D9X@D�@D1@C�m@C�F@CC�@B�!@A��@A�7@Ahs@AX@AG�@A7L@@�`@@Q�@@b@?��@?�@?l�@?�@>�R@>�+@>E�@=@<��@<�D@<j@<Z@<9X@<1@;�m@;��@;dZ@;o@:�!@:~�@:~�@:^5@:-@9��@9hs@9G�@8��@8�@8Q�@7�;@7\)@6��@6ȴ@6��@6��@6��@6v�@6V@65?@6{@5�T@5`B@5/@5�@4�/@4�D@3��@3ƨ@3��@3�@3dZ@2��@2^5@2J@1��@1��@1�7@1&�@1&�@1�@0��@0r�@0A�@0b@/�@.�R@.��@.$�@-@-�@-`B@-`B@-O�@-/@,�@,�/@,�j@,z�@+��@+t�@+t�@+t�@+S�@+33@*��@*M�@)�^@)�7@)%@(r�@(  @'�w@'��@'l�@'K�@'+@'�@'�@&�@&�R@&�+@&$�@&@&@%�@%/@$��@$�@$��@$��@$�D@$9X@$�@#��@#�
@#�@#S�@#33@#@"�!@"=q@!��@!�#@!��@!��@!x�@!G�@ ��@ �u@ 1'@�w@K�@�@��@ȴ@�R@��@5?@@@@@?}@/@�@�@�D@j@��@�
@�F@33@��@M�@�@x�@7L@Ĝ@�u@bN@b@�w@K�@�y@ȴ@��@ff@E�@$�@{@�@�T@�-@�@O�@/@�@�j@��@z�@j@I�@�@��@��@�m@�
@ƨ@t�@C�@33@@�!@��@��@~�@^5@^5@M�@-@-@�@��@�@��@��@x�@&�@�@��@��@�9@��@�u@bN@Q�@A�@1'@b@bG�O�A��#A��/A��;A��HA��/A��A��HA��`A��;A��`A��TA��HA��`A��TA��HA��`A��TA��HA��mA��HA��mA��mA��HA��`A��`A��`A��HA��mA��HA��`A��A��`A��TA��mA��HA��mA��HA��`A��A��`A��A��A��TA��yA��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A���A��A��A��A��A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��yA��`A��mA��`A��`A��A��mA��`A��A��`A��mA��A��`A��yA��A��mA��yA��A��A��yA��A��A��`A��A��yA��mA��yA��A��mA��A��A��yA��A��A��yA��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A��A��A��A���A��A��A���A��A���A���A��A��A���A��A��A���A��A���A���A��A���A��A��A���A���A��A��A���A��A��A���A��A��A���A���A��A���A���A���A���A���A��A��A���A��A���A���A���A��A���A���A��A���A���A��A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�A�A�A�A�  A���A�  A�  A���A�  A�  A���A�  A�  A���A���A�A���A���A�  A���A���A���A��A�oA��A�oA�A�
=A�+A�1'A�$�A�(�A�+A�$�A�(�A�"�A� �A�$�A�/A�7LA�=qA�=qA�9XA�;dA�?}A�I�A�E�A�K�A�K�A�G�A�I�A�O�A�M�A�M�A�S�A�Q�A�I�A�E�A�E�A�?}A�;dA�C�A�A�A�=qA�?}A�A�A�=qA�Q�A�\)A�ffA�^5A�v�A؁A؁A؉7A���AؾwAضFA���A���A���AؾwA���A��TA���A��yA��HA���A؟�A�hsA�n�A�\)A�K�A��A׋DA�dZA�^5A��A��A֙�A�33A� �A�I�A�1'A���Aԇ+A�JA���AӬA�~�A�1AҼjAҮA�t�A�VA�bNA�K�A�M�A�I�A�=qA�JA�A���A��/AѺ^A��AѮAѕ�A�p�A�hsA�E�A�
=A�ƨA�O�A�t�AΕ�A͇+A��A�z�A�?}A��A��mA���A�ƨA˙�A�t�A�bNA�7LA��A�dZA�%A���A�v�A�{Aȴ9AȍPA�r�A�ZA�&�A�JA�1A��mA���A���Aǡ�AǅA�r�A�K�A��AƏ\A�A��
A���A���A���AŋDA�;dA� �A�bA���A�ĜAĺ^Aİ!Aġ�Aĉ7A�hsA�S�A�1'A��A�
=A���A��A��mA��`A��#A��A���A�ȴAç�AÓuAÍPAÁA�v�A�bNA�^5A�O�A�C�A�=qA�9XA��A�  A���A��#A�ƨA¾wAº^A´9A°!A©�A©�A�AA�^5A�K�A�?}A�=qA�5?A�5?A�9XA�9XA�1'A�-A�(�A� �A�"�A��A��A��A��A��A��A��A��A�bA�bA�bA�oA�bA�VA�bA�oA�VA�1A�A�%A���A��A��mA��`A���A���A��wA��jA��-A��\A��A��7A��A�
=A��#A�z�A�;dA�-A�&�A�(�A� �A��HA��A��RA�;dA�{A��+A�;dA�&�A�bA�A��`A���A�A���A���A��DA�r�A�bNA�7LA�bA�A���A���A��\A�jA���A��wA���A��A�p�A�VA�E�A� �A�
=A�A��#A��-A��A�S�A���A��!A���A��A�XA�M�A�G�A�;dA�1'A�"�A�1A��A�ĜA���A�p�A�?}A�VA��A���A�M�A�&�A��wA�JA��7A��A���A���A��A��A��7A��A�n�A�bNA�XA�XA�O�A�C�A�A�A�33A��A�{A�%A�  A���A��A�A�VA�A�%A�$�A�+A� �A�VA��A��
A���A���A�9XA��A�A���A���A���A�z�A��`A���A��hA�G�A�$�A�VA�A��A��A���A��FA�^5A�  A��TA��
A��7A��A���A�JA�7LA�M�A�{A���A���A��A�=qA�%A���A��A�~�A�hsA�1'A��A��A��uA��HA��hA�+A��jA�bNA�9XA��yA��A��\A��A�z�A�p�A�dZA�Q�A�-A�{A���A���A��A��DA�hsA�^5A�^5A�\)A�ZA�XA�Q�A�K�A�;dA��A�  A��PA�=qA��;A���A�hsA�C�A�-A��HA��-A�^5A��A�1A��;A��!A���A�n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
бB
�HB
�HB
�}B
�HB
�}B
�HB
�B
�}B
�B
�}B
�HB
�HB
�HB
�HB
�B
�HB
�}B
�HB
�}B
�}B
�B
�HB
�B
�HB
�HB
�HB
�B
�HB
�B
ϫB
�B
ϫB
ϫB
��B
��B
��B
�B
��B
��B
ϫB
�B
бB
�NB
�B
��B
уB
ҽB
уB
�,B
�pB
�`B
��B
�oB
�B
�(B�BH�B/OB
�/B
��B
�wB
�RB
�B
�6B
ǮB
уB
�B  BfB\BeB&LB4BAUBXBe�BtB�1B�!B��B�<B�mBϫB�yB�B�2B�DB�B��BںB�B�BS�Bd�BaB_pBE�B9$B4�B�B�B�B�B�B�B�QBҽB�<B�EB�B��B��B�=B��B�.B�;By�BYKB>B$�B�BoB"BMB
��B
��B
�B
��B
�B
��B
��B
��B
�xB
�B
}�B
qB
m]B
jB
`�B
]/B
YB
O�B
8�B
�B
�B
�B
�B
SB
SB
�B
�B	��B	�B	��B	�tB	�9B	��B	��B	�'B	�IB	��B	�eB	�\B	�1B	��B	��B	m]B	b�B	\]B	IRB	;dB	9�B	3�B	4nB	5B	;�B	-�B	'RB	"4B	#nB	B	B	�B	B	B	B	�B	\B	 B	.B	\B	�B	�B	%B	SB	SB	%B	YB	�B	�B		�B	
�B		�B	1B	�B	
rB	
�B	�B	�B	�B	{B	�B	�B	!bB	#nB	#:B	"�B	!�B	$tB	1�B	@OB	<B	33B	/�B	0!B	,B	+B	,B	,B	.IB	-�B	2�B	A B	D�B	C-B	?�B	@B	@�B	J�B	H�B	HB	G�B	GB	GB	F�B	J�B	K�B	K)B	R�B	Y�B	]/B	\�B	[�B	^jB	a�B	bNB	ffB	ffB	b�B	aB	`�B	`BB	`B	_;B	^�B	^jB	Z�B	Z�B	WsB	XB	Z�B	]/B	`B	d&B	h>B	kQB	m]B	l�B	m�B	k�B	j�B	kB	j�B	h�B	hsB	iB	iB	j�B	l�B	n�B	r|B	rGB	p�B	u�B	u�B	xB	z�B	}�B	~�B	��B	��B	�1B	��B	��B	�xB	�xB	�B	��B	��B	�\B	�MB	�_B	��B	�'B	��B	�[B	��B	�wB	�IB	�B	�[B	�gB	�B	ȀB	��B	��B	�EB	�B	ȀB	�B	�B	ʌB	�BB	��B	��B	�EB	�B	��B	��B	�`B	�B	��B	�B	��B	��B	��B	�B	�sB	�WB	�B	�B	�oB	��B	�MB	��B	��B	��B
uB
�B
�B
�B
"B
(B
�B
�B
�B
�B
SB
�B
B
7B
7B
kB
�B
	B
qB
xB
B
�B
VB
!-B
 �B
 �B
 �B
"4B
#�B
$@B
$B
$B
$tB
$tB
$tB
$@B
$@B
$B
$�B
%�B
'RB
'�B
(�B
(�B
(�B
(�B
)*B
(�B
+B
,=B
,�B
-�B
.IB
/OB
/�B
/�B
/�B
0�B
49B
4B
49B
4nB
4�B
5?B
7�B
7�B
7�B
8�B
8B
8B
9XB
8�B
8B
8�B
8B
8B
7�B
7�B
7�B
:�B
:�B
:�B
:�B
;0B
;dB
;dB
;�B
;�B
;�B
;�B
;�B
=B
<�B
=�B
=qB
>BB
>B
>B
>BB
>BB
>wB
@B
@B
B�B
CaB
D�B
FB
E�B
FB
E�B
EmB
E�B
EmB
F?B
F?B
F�B
F�B
HB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
K�B
K�B
K�B
K�B
L�B
L�B
NpB
M�B
NB
NpB
N<B
N<B
N<B
O�B
O�B
P}B
Q�B
S&B
S[B
T�B
T,B
T,B
S�B
TaB
S�B
T,B
T,B
T,B
T,B
T,B
UgB
V�B
V9B
VmB
V�B
WsB
WsB
W�B
X�B
X�B
X�B
X�B
XyB
YKB
YB
YKB
ZB
ZQB
ZQB
Z�B
Z�B
\)B
\]B
\�B
]/B
]/B
]/B
\�B
]dB
]/B
\)B
[�B
[�B
\)B
[�B
\�B
]dB
]�B
^jB
]�B
^B
^�B
a�B
c�B
b�B
dZB
dZB
d�B
d�B
d�B
e,B
e`B
e`B
e`B
e�B
e,B
e�B
c�B
d�B
e,B
e�B
h>B
i�B
iyB
h�B
h�B
h�B
i�B
iB
iB
h�B
i�B
i�B
iyB
j�B
i�B
i�B
jKB
i�B
i�B
iyB
iyB
i�B
jKB
j�B
kB
kQB
k�B
l"B
k�B
l"B
lWB
m�B
ncB
n�B
oiB
pB
p;B
p;B
poB
pB
poB
p�B
qB
qB
q�B
qvB
rGB
sB
r�B
sMB
s�B
s�B
s�B
tB
tB
t�B
tB
t�B
t�B
s�B
r�B
sB
s�B
s�B
tTB
t�B
u%B
t�B
u�B
u�B
u�B
v+B
v�B
w2B
w2B
w2B
wfB
w�B
xlB
y�B
y�B
yrB
y�B
zDB
zB
y�B
zB
y�B
y�B
zB
y�B
z�B
{�B
{�B
{�B
{�B
|B
}�B
|�B
|�B
}"B
|�B
}"B
|�B
~]B
}�B
~]B
~(B
~(B
~�B
~�B
~�B
~�B
~]B
~�B
~�B
�B
�4B
�4B
�oB
��B
��B
��B
�B
��B
��B
��B
�MB
��B
��B
��B
�SB
�B
�SB
�SB
��B
��B
�%B
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
�YB
��B
��B
��B
��B
�_B
��B
��B
�fB
�fB
��B
�7B
�lB
�lB
�	B
��B
��B
��B
��B
�JB
�~B
��B
��B
�"B
�VB
��B
��B
��B
�\B
��B
��B
�.B
�.B
�.B
�bB
�bB
�bB
� B
��B
��B
��B
��B
�:B
�B
�:B
�:B
�B
�oB
�oB
�B
�uB
�uB
�uB
�uB
�uB
�B
��B
��B
�B
�B
�FB
�FB
��B
��B
��B
��B
�B
��B
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
��B
��B
��B
��B
��B
�_B
��B
�eB
�1B
�eB
��B
�kB
��B
��B
��B
��B
��B
�	B
�	B
�	B
�	B
�=B
��B
��B
��B
�B
�B
�B
��B
��B
��B
�B
��B
�B
�OB
�OB
��B
��B
�!B
��B
��B
�VB
��B
��B
��B
��B
��B
��B
�bB
��B
��B
��B
��B
��B
�4B
�hB
�4B
�4B
��B
��B
��B
�nB
�nB
�nB
�nB
��B
�@B
��B
��B
�FB
��B
�LB
��B
��B
��B
��B
�B
�B
�B
�RB
�RB
��B
��B
��B
��B
��B
��B
�*B
�*B
�*B
�*B
�*B
��B
��B
��B
��B
��B
�B
�6B
�kB
��B
��B
�B
�=B
�=B
�=B
��B
�qB
��B
�CB
�wB
�B
�}B
�}B
�}B
��B
��B
�}B
��B
�B
��B
��B
��B
��B
��B
��B
�!B
�UB
��B
��B
��B
��B
�[B
��B
��B
��B
��B
�-B
��B
��B
��B
�3B
�3B
��B
�9B
�nB
��B
��B
�B
�B
�tB
�tB
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
��B
��B
�B
�B
��B
��B
��B
��B
�$B
�XB
�$B
�XB
�XB
�XB
��B
��B
��B
��B
��B
��B
��B
�*B
�^B
��B
��B
��B
�dB
�0B
�0B
�dB
�0B
�dB
�dB
��B
��B
��B
�jB
ΥB
��B
�HB
��B
�}B
��B
��B
ϫB
уB
�B
�B
�NB
��B
�}B
��B
�vB
�}B
ѷB
�BB
��B
бB
ϫB
ѷB
�HB
�BB
�B
уB
�B
�B
ϫB
��B
уB
бB
�B
уB
�B
�B
�vB
�B
��B
�jB
�B
уB
�B
��B
бB
��B
�BB
� B
�NB
ϫB
�NB
��B
бB
�}B
�NB
��B
�B
��B
�B
��B
уB
��B
�B
� B
ΥB
��B
бB
�BB
�vB
ѷB
ΥB
бB
�B
�vB
бB
�B
�BB
�NB
��B
�BB
��B
�HB
�BB
�NB
�}B
�BB
бB
�B
��B
�B
��B
ϫB
��B
уB
�B
бB
�B
�BB
�}B
�NB
бB
��B
уB
�BB
�vB
уB
�B
��B
уB
�vB
ϫB
� B
ϫB
ϫB
уB
�B
�B
�B
��B
�B
�vB
�vB
ѷB
ϫB
�}B
� B
��B
�}B
��B
�B
��B
�B
ϫB
��B
�BB
�}B
�B
��B
бB
�B
��B
�}B
��B
�BB
��B
уB
�BB
��B
ѷB
�vB
�HB
�B
��B
бB
уB
��B
�B
�}B
��B
�NB
�}B
�vB
�B
�HB
�B
�HB
�NB
�B
�B
уB
�B
�HB
��B
�vB
�BB
ѷB
��B
�vB
уB
��B
ϫB
ѷB
�B
�B
�NB
�BB
�B
�}B
�pB
��B
�}B
��B
бB
ϫB
ΥB
�B
ϫB
ΥB
�NB
�vB
�vB
�}B
�}B
�BB
��B
�vB
ΥB
�}B
�B
��B
ϫB
��B
�BB
ΥB
�}B
��B
ϫB
�B
�B
�B
�B
��B
�BB
�vB
�B
�B
��B
�B
�BB
�BB
��B
�vB
ΥB
��B
�vB
�vB
��B
��B
�B
�HB
�}B
ΥB
��B
�vB
�B
�B
�BB
�B
�}B
��B
��B
�B
�}B
��B
бB
��B
��B
ϫB
�B
��B
�vB
бB
ΥB
�}B
�}B
ΥB
ϫB
�NB
�B
��B
��B
��B
��B
ϫB
��B
��B
�BB
��B
�vB
�BB
бB
�}B
�B
�}B
�HB
�BB
�NB
��B
ϫB
��B
�NB
ϫB
ѷB
��B
�vB
ѷB
ѷB
�B
�B
�&B
�B
ҽB
��B
ϫB
��B
�}B
бB
уB
ҽB
�NB
ϫB
�B
бB
��B
��B
�NB
ҽB
ӏB
бB
ѷB
҉B
� B
�B
ѷB
� B
бB
�NB
� B
�NB
�HB
ѷB
҉B
��B
�[B
՛B
��B
бB
�TB
��B
��B
��B
бB
бB
�TB
�B
�HB
ѷB
уB
�}B
уB
�TB
�HB
�B
ѷB
��B
�jB
�)B
��B
�jB
یB
��B
��B
�mB
� B
�pB
�B
�B
�B
�HB
�NB
�B
�TB
��B
�B
�B
�B
�B
�B
�B
��B
�B
�B
�oB
�B
�QB
��B
�GB
�B
�]B
�iB
��B
�B
�B
�B
�B
�B
��B
�KB
�B
�QB
�>B
�xB
��B
��B
��B�BYBAB�B�B�B$tB�B!�B"4B�B{B0�BGBJXBL�BL�BGEBD3BD3B@�B?HB?}B'�B&LBH�B	B*�B
�#B
��B
�B
�B
�B
�/B
ʌB
֡B
��B
�B
�/B
�FB
��B
��B
��B
�=B
�B
��B
�B
�B
�wB
�aB
��B
�*B
��B
�6B
�CB
��B
�hB
��B
�_B
�4B
��B
��B
��B
�EB
��B
��B
�qB
��B
��B
�$B
��B
�OB
��B
�:B
��B
�$B
��B
��B
�B
�OB
�<B
��B
˒B
�B
�gB
ɺB
��B
˒B
�#B
�BB
�vB
�TB
�B
�,B
ӏB
�yB
��B
�B
��B
� B
��B
��B
�B
�	B
��B
��B
��B iBB iB 4B�B�BGBB	lB�B	�B	7BxB
	B	�BJBJB�B�B�BB�BSBB1B�B�BCBxB�B!bB#�B!�B(�B)�B)*B*0B*�B+kB-B-B0UB3hB9�B;0B>BB=�B?�B@OB>�B?BB'BF?BI�BT�BU�BWsBYKBXyBYKB[#B_�B^�B`vBf2Be`BiBg�BjKBk�Bm�Bo Bq�Bu%Bw�Bw2B|�B�AB��B��B�YB�B��B�B��B�$B��B�:B��B��B��B�!B�qB�B�6B��B��B�XB�HB��B�EB�B�gB�gB�9B�aBB�RB�B�mB��B�BĜB�BŢB��B�?BĜB�aB� BیB�B�B��B��B�9B��B�sBٴB�5B��B�yBߤB�B�8B�NB�iB��B�B��B��B��B�ZB�,B�B�B�B�ZB�B�KB��B�WB�B�B�B�AB$B��BیB�9B�KB�BϫB�B͟B��B͟B҉B҉B�gB��B�
B��B�dB�|B��B�
B�B��B��B��B��B�.B	�BB�BbB�B#:B/�B5?BAUB\�B^jB\�BYKBU�BU2BV�Bc�B��B_�Ba�BiyBb�B`B^�B^�B]dB_;B^�Bm�B`�BT�BS�Bd�B^jB_B]�BpoBS�BD�BB�B?�B?BB�B?}B>BqAB>�B2�B7�B0�B3�B8�BQ�BV�B5B2�B/�B'RB/�B"�B!-B�BBxB�BxB �B�BBB�B�B�B�B@BB�B�B�BoB�BhB"B�B�BVB�B��B��B��BoB��B��B�B�oB�oB�/B�B�yG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2020121920555520201219205555IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401220210224164012QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401220210224164012QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194020210429101940IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270220210429202702IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                