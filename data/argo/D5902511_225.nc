CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-08-17T16:38:26Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      =�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      dx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �T   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20220817163826  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_225                 6810_008521_225                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��w�n.�@��w�n.�11  @��w���@��w���@22$�LD|@22$�LD|�d�iu��d�iu�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�  @��R@�p�@�p�@�p�A\)A ��A+�A>�RA`  A���A���A�Q�A��A��AϮA�\)A�\)B   B(�B  B  B�
B'�
B/�
B8  B@(�BH(�BPQ�BX(�B_�
Bg�Bo�Bw�B�
B��
B��B�(�B�(�B�(�B�(�B�{B��B�  B��B��B��
B��B�  B�  B�  B�  B��B�  B�  B�  B�(�B�(�B�{B�{B�{B�  B��
B�  B��B��
C   C
=C��C��C  C

=C
=C��C��C��C  C��C�C
=C  C  C   C"
=C$
=C%�C'��C*
=C,
=C.  C/��C1�C3��C5��C7��C:  C<  C>  C@  CA��CD  CF{CH
=CI��CK�CM��CO��CQ��CS��CU��CW��CY��C\  C^  C_��Cb  Cc��Ce��Ch  Cj  Cl  Cm��Co�Cr  Cs��Cv
=Cx  Cz  C|  C}��C�C���C�  C���C���C�C�  C���C�  C���C���C���C�  C�C�  C���C���C�  C�  C�  C���C�  C�  C�  C�C�
=C���C���C�C�C���C���C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C���C���C���C�C�  C�  C�  C�  C�  C�C�  C�  C���C���C���C���C�  C���C�  C�
=C�  C���C���C�  C���C���C�C�C�  C���C���C���C�  C�  C���C���C��C���C�C�C�  C���C�  C���C���C�C�  C�  C�  C�  C�  C���C���C���C�C�C�C�  C�  C�
=C�C�  C�C�  C�  C���C���C�C�  C���C���C���C�  C�
=C�C�  C�  C�C�  C�  D �D ��D �qD� D�D� D�D� D�qD� D�D� D�qD��D�D}qD  D� D�qD	z�D	��D
}qD  D}qD  D� D�qDz�D�qD� D�D� D��D}qD  D��D  D}qD�qD��D�D�D�D}qD  D� D�qD��D  D� D�D��DD��D�D��D�qD}qD  D� D�D� D  D��D   D � D!�D!��D"�D"�D#�D#��D$  D$z�D%  D%� D%�qD&}qD&�qD'��D(  D(��D)�D)� D*  D*� D+  D+� D,�D,��D-  D-z�D-��D.z�D.�qD/��D0  D0��D0�qD1z�D1�qD2}qD3  D3��D3�qD4}qD4�qD5}qD6�D6� D7�D7��D8  D8}qD9�D9��D9�qD:}qD:�qD;� D<  D<z�D<��D=}qD>  D>}qD>��D?� D@  D@}qDA  DA��DA�qDB��DC  DC}qDDDD�DE�DE� DE��DFz�DF��DG}qDH  DH��DI  DI}qDI�qDJ}qDK�DK� DK�qDL��DM  DM� DN  DN}qDN�qDO��DP  DP}qDP�qDQ}qDQ��DR� DS�DS� DS�qDT� DT��DUxRDU�qDV� DV�qDW}qDW�qDX}qDY�DY��DZ�DZ}qD[  D[��D\  D\}qD\�qD]� D^�D^��D_�D_� D`  D`� Da  Da��Da�qDb� Dc  Dc}qDd  Dd� De  De��Df  Df� Dg  Dg� Dh�Dh��Dh�qDi}qDj�Dj��Dk�Dk�Dl�Dl� Dm  Dm��Dn�Dn��Dn�qDo}qDp�Dp�Dq  Dq}qDr  Dr� Dr�qDs� Dt  Dt��Du  Du� Dv�Dv� Dv�qDw� Dw�qDx}qDx�qDy� Dz�Dz� Dz�qD{� D|�D|� D}�D}� D~�D~�D�D��D�  D�>�D�~�D�� D�HD�AHD�� D���D�  D�@ D�� D�� D���D�@ D��HD�� D�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD��HD��HD��D�C�D��HD���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D��HD���D��qD�@ D��HD��HD��D�B�D��HD��HD�HD�>�D�� D�� D���D�>�D�}qD��qD�HD�AHD���D�D��D�AHD�� D��HD�  D�>�D�~�D���D���D�>�D��HD�D��D�AHD��HD�D�HD�@ D�~�D�� D�  D�AHD���D�D�  D�>�D�~�D��qD�  D�@ D�}qD�� D�HD�@ D�� D�� D�  D�AHD�� D��HD��D�AHD��HD�� D���D�@ D�~�D�� D�HD�AHD��HD���D���D�@ D��HD�D�HD�@ D�� D���D��qD�@ D��HD�� D�HD�AHD�� D�D�HD�AHD�� D���D�HD�AHD�~�D�� D���D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�>�D�}qD���D�HD�B�D��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D��qD���D�@ D�~�D���D�  D�=qD�}qD���D�HD�AHD�~�D�� D�  D�>�D�� D��HD�  D�@ D��HD��HD�  D�>�D�|)D��qD�  D�AHD�� D��HD�HD�=qD�~�D�� D�HD�AHD�~�D���D���D�@ D�� D�� D�HD�AHD��HD���D�  D�B�D�� D��HD�  D�>�D�~�D���D���D�@ D�~�D��qD���D�>�D�~�D���D�HD�AHD�~�D�� D�HD�AHD���D��HD���D�>�D�� D�� D�HD�B�D�~�D��qD�  D�@ D�}qD�� D��D�AHD D�� D�HD�AHD�}qDýqD�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�@ Dƀ Dƾ�D�  D�B�DǁHD�� D��D�AHD�~�DȾ�D���D�>�Dɀ D�� D�  D�=qD�}qD��HD�HD�@ D�~�D˾�D��qD�>�D̀ D̾�D�HD�B�D́HD;�D�  D�B�D΁HDξ�D���D�>�Dπ DϾ�D���D�AHDЀ DнqD�  D�>�D�~�DѾ�D���D�@ DҀ D�� D�  D�@ DӀ D�D�HD�@ D�~�DԽqD���D�@ DՀ Dվ�D��D�B�DցHD�D�HD�@ D�~�D׾�D���D�>�D؁HD��HD�HD�AHDـ D�� D�  D�B�Dڀ D��HD�HD�AHDہHD�� D�  D�=qD܀ D��HD���D�@ D݁HD�D�  D�>�Dހ D�� D��qD�@ D߁HD�� D�HD�@ D�~�D�� D�HD�AHD�HD�� D���D�>�D�~�D⾸D���D�@ D� D��HD�HD�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�~�D澸D���D�@ D� D羸D�  D�B�D�HD��HD�  D�>�D�~�D龸D��qD�>�D�~�D꾸D�HD�B�D� D뾸D���D�>�D� D��HD��D�>�D�}qD��qD��qD�=qD�~�D�� D��qD�>�D�HD�D�HD�@ D��HD�D�  D�AHD�D��HD���D�=qD�}qD�D��qD�>�D� D�� D�HD�>�D�}qD���D���D�=qD�~�D��HD���D�<)D�~�D��HD�  D�%>���?.{?aG�?��R?���?�@��@�R@0��@J=q@Y��@p��@��\@��@�Q�@�  @�=q@�
=@�  @�=q@�
=@޸R@�=q@�
=@�p�AA	��A�RA�AQ�A{A#�
A'
=A-p�A333A6ffA<��AA�AEAL(�AP  AU�A[�A`  Adz�Ak�Ap  Atz�A{�A\)A��\A�p�A�\)A��\A�p�A��A��\A�A��A�33A�p�A�  A��A�p�A���A�33A�A���A��HA�A���A��HA�p�A���A��HA�A���Aʏ\A�A�Q�A�=qA�p�A�Q�A�=qA�p�A��A�\A���A�Q�A�\A�p�A�Q�A�=qA��A�Q�A�=qA�p�B   B ��B�RB  B��B�\B�
B��B
�RB�
B��B�\B�B��B�\B�B��B�\B�B��BffB�B��B=qB�B ��B!�B#�B$��B&{B'�B(��B)�B+�B,��B-�B/�B1�B1�B3�
B5�B6{B7�B9�B:=qB;�B=G�B>=qB?�BAG�BB�\BC�BEG�BF�\BG�BIp�BJ�\BK�
BMp�BN�RBO�BQp�BR�HBS�
BU��BV�HBX  BY��B[
=B\  B]B_\)B`Q�BaBc�Bdz�Be�Bg�Bh��Bi�Bk�Bl��Bm�Bo�Bp��BqBs�Bt��Bu�Bw�Bx��By�B{�B|��B}�B�B�ffB��B��
B�ffB��B�  B��\B�33B�  B���B��B�  B��RB�33B�  B��RB�G�B��B��RB�33B�  B���B�G�B��B���B�\)B��B��RB�\)B��
B���B�p�B��B��RB�p�B��B���B�\)B��B���B�G�B�  B���B�\)B�{B���B�\)B�{B��HB�p�B�  B���B��B�{B��HB���B�(�B��RB��B�=qB��RB�p�B�=qB��RB�p�B�=qB���B�G�B�(�B���B�G�B�{B��\B�G�B�{B��\B��B�  B�z�B��B�  B�z�B�
=B��
B�ffB��HB�p�B�=qB��RB�33B��B��\B���B�\)B�{B�z�B���B�\)B��
B�(�B���B�33B�\)B��B�Q�B�z�B��HB�G�B�p�BîB�{B�Q�B�ffB���B��B�33BŅB��B�(�B�=qBƣ�B���B�33B�\)B�B�{B�=qB�z�B���B�33B�\)BɮB�(�B�Q�Bʏ\B���B�\)BˮB�B�=qḄ�B���B�
=BͅB��B�{B�Q�B���B�33B�p�BϮB�(�B�z�BУ�B�
=B�p�B��
B�  B�ffB��HB��B�\)B��
B�Q�B�z�B��HB�G�BծB�  B�=qB���B��B�\)B��B�Q�B؏\B��HB�p�B�B�  B�ffB��HB�G�BۅB��B�ffBܣ�B�
=B݅B��
B�{Bޏ\B�
=B�\)B߮B�  B�z�B���B�G�B�B�  B�ffB�RB���B�\)B��
B�=qB�ffB���B�\)B噚B��
B�Q�B��HB�
=B�\)B��B�=qB�z�B��HB�p�B�B��B�z�B���B�G�B뙚B�{B��B��HB�33B�B�(�B�ffB���B�\)B�B��B�z�B���B�33B�B�(�B�z�B�RB��B�B�{B�Q�B���B�33B��B�  B�=qB��RB�33B��B�B�Q�B��RB���B�G�B�B�(�B�ffB��RB�33B���B��
B�{B��\B�
=B�33B��B�  B�ffB���B��HB�G�B�C   C {C G�C �C �C �
C ��C�C\)C�C��C��C
=C(�CG�C�\C�RC��C��C33CQ�Cp�C��C�
C�C  CG�Cp�C�\C�C�C�C=qCffC��C�
C�C{CQ�C�C��C��C{C(�CQ�C�C�RC��C  C=qCQ�Cz�C�RC�HC��C	(�C	p�C	�\C	�C	�HC
�C
G�C
ffC
�C
�HC
��C�C\)C��CC�HC�C\)Cz�C�C�C  C=qC�C��C��C
=CG�Cp�C�\C��C
=C33C\)C��C�
C��C=qCp�C�\C�
C�C=qCz�C�RC�C
=C\)C�\C�RC  C=qC\)C��C�
C
=C(�CffC�C�
C  C33Cz�C��C��C  CQ�Cp�C��C�
C
=C33CffC�C�HC��C33Cz�C��CC  CG�CffC��C�HC{C33C\)C��C�
C��C=qCp�C��CC
=C=qCffC�\C�
C
=C(�Cp�C�C��C  CG�Cz�C��C��C{CG�Cp�C��C�HC �C =qC p�C C!  C!(�C!Q�C!��C!�HC"  C"33C"�C"C"�C#�C#ffC#��C#C$  C$G�C$�C$��C$�
C%(�C%\)C%�C%�RC&  C&=qC&ffC&��C&�
C'{C'=qC'z�C'C'��C({C(G�C(��C(��C(��C)�C)ffC)��C)�
C*  C*=qC*�C*��C*��C+{C+\)C+�\C+C+�C,33C,p�C,�C,��C-
=C-Q�C-�\C-�C-�C.(�C.p�C.��C.��C/{C/Q�C/�\C/�C/��C0=qC0p�C0�\C0�HC1(�C1Q�C1z�C1C2
=C233C2\)C2�C2�C3�C3G�C3�\C3�
C4
=C433C4ffC4�C4�C5�C5Q�C5z�C5�C6  C6=qC6\)C6�C6C7
=C7G�C7p�C7��C7�
C8�C8G�C8p�C8�C8�C933C9\)C9�C9��C:
=C:=qC:ffC:��C:�HC;(�C;ffC;��C;��C<  C<=qC<z�C<��C=
=C=(�C=\)C=��C=�C>(�C>ffC>�\C>C?  C?G�C?�\C?C?�C@�C@\)C@��C@�CA(�CA\)CA�\CA��CB  CBG�CB�\CB��CC  CC33CCffCC�RCD  CD33CDffCD�\CD�
CE�CE\)CE�CE�RCF  CFG�CF�CF�RCF�HCG{CGG�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                              ?�  @�\@B�\@�  @��R@�p�@�p�@�p�A\)A ��A+�A>�RA`  A���A���A�Q�A��A��AϮA�\)A�\)B   B(�B  B  B�
B'�
B/�
B8  B@(�BH(�BPQ�BX(�B_�
Bg�Bo�Bw�B�
B��
B��B�(�B�(�B�(�B�(�B�{B��B�  B��B��B��
B��B�  B�  B�  B�  B��B�  B�  B�  B�(�B�(�B�{B�{B�{B�  B��
B�  B��B��
C   C
=C��C��C  C

=C
=C��C��C��C  C��C�C
=C  C  C   C"
=C$
=C%�C'��C*
=C,
=C.  C/��C1�C3��C5��C7��C:  C<  C>  C@  CA��CD  CF{CH
=CI��CK�CM��CO��CQ��CS��CU��CW��CY��C\  C^  C_��Cb  Cc��Ce��Ch  Cj  Cl  Cm��Co�Cr  Cs��Cv
=Cx  Cz  C|  C}��C�C���C�  C���C���C�C�  C���C�  C���C���C���C�  C�C�  C���C���C�  C�  C�  C���C�  C�  C�  C�C�
=C���C���C�C�C���C���C�C�C�  C���C���C�  C�  C�  C�  C�  C�  C�  C���C���C���C�  C���C���C���C�C�  C�  C�  C�  C�  C�C�  C�  C���C���C���C���C�  C���C�  C�
=C�  C���C���C�  C���C���C�C�C�  C���C���C���C�  C�  C���C���C��C���C�C�C�  C���C�  C���C���C�C�  C�  C�  C�  C�  C���C���C���C�C�C�C�  C�  C�
=C�C�  C�C�  C�  C���C���C�C�  C���C���C���C�  C�
=C�C�  C�  C�C�  C�  D �D ��D �qD� D�D� D�D� D�qD� D�D� D�qD��D�D}qD  D� D�qD	z�D	��D
}qD  D}qD  D� D�qDz�D�qD� D�D� D��D}qD  D��D  D}qD�qD��D�D�D�D}qD  D� D�qD��D  D� D�D��DD��D�D��D�qD}qD  D� D�D� D  D��D   D � D!�D!��D"�D"�D#�D#��D$  D$z�D%  D%� D%�qD&}qD&�qD'��D(  D(��D)�D)� D*  D*� D+  D+� D,�D,��D-  D-z�D-��D.z�D.�qD/��D0  D0��D0�qD1z�D1�qD2}qD3  D3��D3�qD4}qD4�qD5}qD6�D6� D7�D7��D8  D8}qD9�D9��D9�qD:}qD:�qD;� D<  D<z�D<��D=}qD>  D>}qD>��D?� D@  D@}qDA  DA��DA�qDB��DC  DC}qDDDD�DE�DE� DE��DFz�DF��DG}qDH  DH��DI  DI}qDI�qDJ}qDK�DK� DK�qDL��DM  DM� DN  DN}qDN�qDO��DP  DP}qDP�qDQ}qDQ��DR� DS�DS� DS�qDT� DT��DUxRDU�qDV� DV�qDW}qDW�qDX}qDY�DY��DZ�DZ}qD[  D[��D\  D\}qD\�qD]� D^�D^��D_�D_� D`  D`� Da  Da��Da�qDb� Dc  Dc}qDd  Dd� De  De��Df  Df� Dg  Dg� Dh�Dh��Dh�qDi}qDj�Dj��Dk�Dk�Dl�Dl� Dm  Dm��Dn�Dn��Dn�qDo}qDp�Dp�Dq  Dq}qDr  Dr� Dr�qDs� Dt  Dt��Du  Du� Dv�Dv� Dv�qDw� Dw�qDx}qDx�qDy� Dz�Dz� Dz�qD{� D|�D|� D}�D}� D~�D~�D�D��D�  D�>�D�~�D�� D�HD�AHD�� D���D�  D�@ D�� D�� D���D�@ D��HD�� D�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D�� D��HD�HD�AHD��HD��HD��D�C�D��HD���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�}qD���D�  D�@ D��HD���D��qD�@ D��HD��HD��D�B�D��HD��HD�HD�>�D�� D�� D���D�>�D�}qD��qD�HD�AHD���D�D��D�AHD�� D��HD�  D�>�D�~�D���D���D�>�D��HD�D��D�AHD��HD�D�HD�@ D�~�D�� D�  D�AHD���D�D�  D�>�D�~�D��qD�  D�@ D�}qD�� D�HD�@ D�� D�� D�  D�AHD�� D��HD��D�AHD��HD�� D���D�@ D�~�D�� D�HD�AHD��HD���D���D�@ D��HD�D�HD�@ D�� D���D��qD�@ D��HD�� D�HD�AHD�� D�D�HD�AHD�� D���D�HD�AHD�~�D�� D���D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD��HD��HD�  D�>�D�~�D���D�  D�>�D�}qD���D�HD�B�D��HD�� D�  D�>�D�~�D�� D�  D�>�D�� D��qD���D�@ D�~�D���D�  D�=qD�}qD���D�HD�AHD�~�D�� D�  D�>�D�� D��HD�  D�@ D��HD��HD�  D�>�D�|)D��qD�  D�AHD�� D��HD�HD�=qD�~�D�� D�HD�AHD�~�D���D���D�@ D�� D�� D�HD�AHD��HD���D�  D�B�D�� D��HD�  D�>�D�~�D���D���D�@ D�~�D��qD���D�>�D�~�D���D�HD�AHD�~�D�� D�HD�AHD���D��HD���D�>�D�� D�� D�HD�B�D�~�D��qD�  D�@ D�}qD�� D��D�AHD D�� D�HD�AHD�}qDýqD�  D�AHDĀ Dľ�D���D�>�Dŀ D�� D�  D�@ Dƀ Dƾ�D�  D�B�DǁHD�� D��D�AHD�~�DȾ�D���D�>�Dɀ D�� D�  D�=qD�}qD��HD�HD�@ D�~�D˾�D��qD�>�D̀ D̾�D�HD�B�D́HD;�D�  D�B�D΁HDξ�D���D�>�Dπ DϾ�D���D�AHDЀ DнqD�  D�>�D�~�DѾ�D���D�@ DҀ D�� D�  D�@ DӀ D�D�HD�@ D�~�DԽqD���D�@ DՀ Dվ�D��D�B�DցHD�D�HD�@ D�~�D׾�D���D�>�D؁HD��HD�HD�AHDـ D�� D�  D�B�Dڀ D��HD�HD�AHDہHD�� D�  D�=qD܀ D��HD���D�@ D݁HD�D�  D�>�Dހ D�� D��qD�@ D߁HD�� D�HD�@ D�~�D�� D�HD�AHD�HD�� D���D�>�D�~�D⾸D���D�@ D� D��HD�HD�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�~�D澸D���D�@ D� D羸D�  D�B�D�HD��HD�  D�>�D�~�D龸D��qD�>�D�~�D꾸D�HD�B�D� D뾸D���D�>�D� D��HD��D�>�D�}qD��qD��qD�=qD�~�D�� D��qD�>�D�HD�D�HD�@ D��HD�D�  D�AHD�D��HD���D�=qD�}qD�D��qD�>�D� D�� D�HD�>�D�}qD���D���D�=qD�~�D��HD���D�<)D�~�D��HD�  G�O�>���?.{?aG�?��R?���?�@��@�R@0��@J=q@Y��@p��@��\@��@�Q�@�  @�=q@�
=@�  @�=q@�
=@޸R@�=q@�
=@�p�AA	��A�RA�AQ�A{A#�
A'
=A-p�A333A6ffA<��AA�AEAL(�AP  AU�A[�A`  Adz�Ak�Ap  Atz�A{�A\)A��\A�p�A�\)A��\A�p�A��A��\A�A��A�33A�p�A�  A��A�p�A���A�33A�A���A��HA�A���A��HA�p�A���A��HA�A���Aʏ\A�A�Q�A�=qA�p�A�Q�A�=qA�p�A��A�\A���A�Q�A�\A�p�A�Q�A�=qA��A�Q�A�=qA�p�B   B ��B�RB  B��B�\B�
B��B
�RB�
B��B�\B�B��B�\B�B��B�\B�B��BffB�B��B=qB�B ��B!�B#�B$��B&{B'�B(��B)�B+�B,��B-�B/�B1�B1�B3�
B5�B6{B7�B9�B:=qB;�B=G�B>=qB?�BAG�BB�\BC�BEG�BF�\BG�BIp�BJ�\BK�
BMp�BN�RBO�BQp�BR�HBS�
BU��BV�HBX  BY��B[
=B\  B]B_\)B`Q�BaBc�Bdz�Be�Bg�Bh��Bi�Bk�Bl��Bm�Bo�Bp��BqBs�Bt��Bu�Bw�Bx��By�B{�B|��B}�B�B�ffB��B��
B�ffB��B�  B��\B�33B�  B���B��B�  B��RB�33B�  B��RB�G�B��B��RB�33B�  B���B�G�B��B���B�\)B��B��RB�\)B��
B���B�p�B��B��RB�p�B��B���B�\)B��B���B�G�B�  B���B�\)B�{B���B�\)B�{B��HB�p�B�  B���B��B�{B��HB���B�(�B��RB��B�=qB��RB�p�B�=qB��RB�p�B�=qB���B�G�B�(�B���B�G�B�{B��\B�G�B�{B��\B��B�  B�z�B��B�  B�z�B�
=B��
B�ffB��HB�p�B�=qB��RB�33B��B��\B���B�\)B�{B�z�B���B�\)B��
B�(�B���B�33B�\)B��B�Q�B�z�B��HB�G�B�p�BîB�{B�Q�B�ffB���B��B�33BŅB��B�(�B�=qBƣ�B���B�33B�\)B�B�{B�=qB�z�B���B�33B�\)BɮB�(�B�Q�Bʏ\B���B�\)BˮB�B�=qḄ�B���B�
=BͅB��B�{B�Q�B���B�33B�p�BϮB�(�B�z�BУ�B�
=B�p�B��
B�  B�ffB��HB��B�\)B��
B�Q�B�z�B��HB�G�BծB�  B�=qB���B��B�\)B��B�Q�B؏\B��HB�p�B�B�  B�ffB��HB�G�BۅB��B�ffBܣ�B�
=B݅B��
B�{Bޏ\B�
=B�\)B߮B�  B�z�B���B�G�B�B�  B�ffB�RB���B�\)B��
B�=qB�ffB���B�\)B噚B��
B�Q�B��HB�
=B�\)B��B�=qB�z�B��HB�p�B�B��B�z�B���B�G�B뙚B�{B��B��HB�33B�B�(�B�ffB���B�\)B�B��B�z�B���B�33B�B�(�B�z�B�RB��B�B�{B�Q�B���B�33B��B�  B�=qB��RB�33B��B�B�Q�B��RB���B�G�B�B�(�B�ffB��RB�33B���B��
B�{B��\B�
=B�33B��B�  B�ffB���B��HB�G�B�C   C {C G�C �C �C �
C ��C�C\)C�C��C��C
=C(�CG�C�\C�RC��C��C33CQ�Cp�C��C�
C�C  CG�Cp�C�\C�C�C�C=qCffC��C�
C�C{CQ�C�C��C��C{C(�CQ�C�C�RC��C  C=qCQ�Cz�C�RC�HC��C	(�C	p�C	�\C	�C	�HC
�C
G�C
ffC
�C
�HC
��C�C\)C��CC�HC�C\)Cz�C�C�C  C=qC�C��C��C
=CG�Cp�C�\C��C
=C33C\)C��C�
C��C=qCp�C�\C�
C�C=qCz�C�RC�C
=C\)C�\C�RC  C=qC\)C��C�
C
=C(�CffC�C�
C  C33Cz�C��C��C  CQ�Cp�C��C�
C
=C33CffC�C�HC��C33Cz�C��CC  CG�CffC��C�HC{C33C\)C��C�
C��C=qCp�C��CC
=C=qCffC�\C�
C
=C(�Cp�C�C��C  CG�Cz�C��C��C{CG�Cp�C��C�HC �C =qC p�C C!  C!(�C!Q�C!��C!�HC"  C"33C"�C"C"�C#�C#ffC#��C#C$  C$G�C$�C$��C$�
C%(�C%\)C%�C%�RC&  C&=qC&ffC&��C&�
C'{C'=qC'z�C'C'��C({C(G�C(��C(��C(��C)�C)ffC)��C)�
C*  C*=qC*�C*��C*��C+{C+\)C+�\C+C+�C,33C,p�C,�C,��C-
=C-Q�C-�\C-�C-�C.(�C.p�C.��C.��C/{C/Q�C/�\C/�C/��C0=qC0p�C0�\C0�HC1(�C1Q�C1z�C1C2
=C233C2\)C2�C2�C3�C3G�C3�\C3�
C4
=C433C4ffC4�C4�C5�C5Q�C5z�C5�C6  C6=qC6\)C6�C6C7
=C7G�C7p�C7��C7�
C8�C8G�C8p�C8�C8�C933C9\)C9�C9��C:
=C:=qC:ffC:��C:�HC;(�C;ffC;��C;��C<  C<=qC<z�C<��C=
=C=(�C=\)C=��C=�C>(�C>ffC>�\C>C?  C?G�C?�\C?C?�C@�C@\)C@��C@�CA(�CA\)CA�\CA��CB  CBG�CB�\CB��CC  CC33CCffCC�RCD  CD33CDffCD�\CD�
CE�CE\)CE�CE�RCF  CFG�CF�CF�RCF�HCG{CGG�CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aٴ9AٶFAٍPA�n�A�ZA�K�A�?}A�;dA�7LA�-A� �A�oA���A��;AظRA؇+A�|�A�t�A�hsA�`BA�ZA�M�A�A�A�;dA�-A�$�A� �A��A�{A�VA�
=A�JA�%A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��mA��/A�ĜAק�A�bNA�JA�oA���A�;dA�E�A�O�A�S�Aϥ�A�`BA͝�A�\)A��`A��AǕ�AƧ�AŁA�ffA�"�A�ĜA�9XA�1A�;dA�M�A�=qA��^A�M�A�7LA��PA��A��A���A��A�(�A���A�33A���A��FA�G�A��A��A���A��A�ffA��RA�1A���A�^5A���A���A���A��+A��A��A�VA�v�A�z�A��^A���A��DA��A�bA���A��A���A�~�A��^A� �A��A��FA�M�A��jA�v�AhsA{�
Aw`BAq��Al1'Aj�HAi��Ah�Ah�Af=qA_�-A]��A\�AZ�`AY%AV��AT1AN��AK�AJ�+AH��AF��AES�AC��AAl�A@E�A?dZA=&�A;��A8��A6VA5�A4M�A3`BA1hsA/�wA.�uA-"�A,(�A*$�A'��A& �A$�HA$  A"��A!�wA ^5AdZAbNAJA?}A�AS�A1'A&�A{A�PAC�A~�A�A{AG�A�HAS�A�jAA�A�-A
=A�jAE�AC�A
VA	��A	�;A	`BA1Az�A�#A\)A�A�RA1'A`BAz�A�A �yA z�@�|�@��^@���@�ƨ@��\@�z�@�h@��@�ƨ@�
=@���@���@�@�
=@�Z@��m@��@�9@���@�=q@��@���@� �@�{@�p�@�Q�@�~�@ّh@�%@�Z@ו�@�"�@�dZ@�+@�;d@ץ�@�Ĝ@җ�@�S�@�/@��/@���@�33@�M�@�5?@Ѻ^@�G�@���@�/@Л�@Ϯ@�|�@�+@θR@�v�@͉7@̋D@��
@�+@ʰ!@��#@�O�@ț�@� �@��@��m@ǥ�@��@Ə\@�E�@��@�hs@�O�@��@þw@��@+@��@�J@���@��h@�V@�r�@�1@�;d@�
=@���@���@��+@�v�@�=q@��#@���@��h@���@�9X@��F@�C�@�o@�
=@��@���@�~�@�-@��T@���@�hs@�&�@���@�z�@���@�@��@�ȴ@��\@�n�@�E�@���@��@��#@���@�&�@���@���@�A�@���@��@���@��@�|�@�t�@�l�@�dZ@�S�@�;d@��y@�v�@�{@���@�/@��j@��@�A�@��@��@�ƨ@��@��@��!@��\@�=q@��^@��@��/@��D@�1'@�b@��w@�o@���@��T@�bN@��
@���@�|�@�C�@�@��H@�ȴ@��+@�5?@��@��#@�X@���@��@���@��@�Z@�  @��F@���@�|�@�S�@�C�@���@���@���@�M�@��7@���@��u@��D@�Z@� �@���@�
=@�ȴ@��+@�{@���@��@��@��@���@�z�@�j@�A�@��@���@��
@��w@��@�S�@�;d@�33@��@��@�ff@�J@��^@�p�@��@���@�9X@�b@��@��m@��w@�dZ@�S�@�33@��y@��!@�^5@�@��^@�V@���@��@�z�@�r�@�r�@�Z@�9X@���@���@�C�@�"�@��y@���@��+@�=q@��@��-@�`B@�%@��@�r�@� �@��F@���@�t�@��y@�n�@�$�@���@��7@�X@��/@���@��@�bN@�b@��m@��;@��w@�S�@��@��H@��@�ȴ@�n�@�V@�5?@��@��h@�O�@�V@���@���@��/@��@�r�@�1'@���@���@�K�@��@��@�^5@�=q@��#@���@�X@��@��@��/@��9@��u@�b@�ƨ@���@�K�@���@��@���@���@�ff@�@���@�p�@�/@���@���@��j@��9@��@���@�j@�I�@�1'@��@�P@K�@
=@~ȴ@~��@~$�@}�T@}�T@}�T@}�-@}�h@}�@}p�@}?}@|��@|�@{�m@{�F@{�@{"�@z�H@z�\@zJ@y�^@y��@y��@y7L@x��@xQ�@w�@w|�@w
=@v�R@v�+@vff@v$�@v@u@t�/@t�@t��@t9X@sƨ@sS�@r=q@q�#@q��@q��@qX@p��@p��@p1'@pb@p  @o�@ol�@o;d@n�y@n��@n{@m��@m�-@mV@kdZ@ko@k@j�H@j��@j�!@j��@j~�@j=q@i�^@ihs@iG�@i%@h��@h�9@h�u@hr�@hA�@h1'@hb@gl�@fE�@e@e�@e�@d�/@d�@d�D@dZ@dI�@c��@c��@cdZ@b�H@bM�@a��@a��@ax�@a7L@a�@`��@`��@`Q�@_�P@_K�@^��@^�+@^$�@^@]@]�h@]`B@]�@\�j@\Z@[�
@[�F@[��@[o@Zn�@Y�^@XĜ@X�u@Xr�@X �@W�w@W|�@W\)@W+@W
=@Vȴ@U�T@UO�@T�D@T(�@S�m@S��@St�@S"�@R�@R�!@R�@Q��@Q�7@Q&�@P�@O�@O+@O
=@Nȴ@NE�@M��@Mp�@L��@K��@KS�@J^5@I�#@Ix�@H�`@H��@Hb@Gl�@F�y@F$�@E��@E�h@E`B@E/@D��@C��@CC�@C@B�@B��@B��@B~�@BM�@A�#@Ahs@@�`@@Ĝ@@��@@bN@@ �@?�w@?;d@?+@>�@>��@>V@>E�@>V@>ff@>5?@=�T@=p�@<��@<Z@;��@;�
@;�F@;�@;t�@;dZ@;"�@:��@9��@9�#@9G�@9G�@8r�@7�;@7\)@7;d@7+@7�@7�@6ȴ@6�+@6ff@5�T@5V@4��@4�D@4j@4j@49X@3�F@333@2�@2��@2~�@2M�@2�@2J@1�@1�^@1�7@0��@0Q�@/��@/�@/l�@/;d@.�y@.�R@.��@.{@-@-�@-`B@-O�@-?}@-/@,��@,��@+�
@+33@+o@+@*��@*�!@*�\@*n�@*^5@*=q@*J@)G�@(�`@(�9@(��@(�u@(�@(bN@(A�@'�w@'|�@';d@&��@&��@&��@&�+@&E�@&{@%�@%�@%?}@%�@$��@$Z@$(�@#��@#�F@#t�@#dZ@#S�@#33@#@"�H@"�!@"~�@"=q@"-@!��@!��@!�@!��@!��@!�^@!�7@!&�@ �u@ bN@ A�@ b@�@��@��@|�@K�@
=@�y@ȴ@��@E�@�T@�T@�T@@�h@`B@V@�/@�@�D@z�@I�@�
@��@S�@o@��@J@�#@��@�^@x�@7L@&�@�`@��@r�@ �@  @�w@\)@
=@�R@v�@V@5?@$�@{@@@��@�@`B@�@V@�/@��@�j@�@�D@j@�@�
@��@dZ@C�@"�@�@��@M�@J@�#@�^@hs@&�@�@��@��@�`@��@Ĝ@Ĝ@�9@��@bN@ �@b@  @�@�@�@�;@��@�@l�@\)@;d@
=@�y@ȴ@�R@��@��@��@��@�+@E�@{@�T@��@@��@p�@`B@O�@�@�j@��@�D@j@Z@�@1@�
@�F@�F@��@��@�@dZ@33AٮAٺ^Aٰ!Aٲ-AٸRAٲ-AټjAٮAٍPAُ\AًDA�n�A�t�A�\)A�\)A�XA�Q�A�O�A�K�A�E�A�C�A�;dA�=qA�=qA�9XA�=qA�;dA�/A�5?A�/A�/A�1'A�$�A��A��A��A�{A�bA�
=A�VA�%A�%A�A���A��A���A��A��TA��mA��;A��A��A���A���A�ȴA�AخAا�A؝�Aؗ�A؅A؁A؃A�z�A�|�A�|�A�|�A�~�A�z�A�z�A�z�A�t�A�r�A�t�A�p�A�t�A�v�A�n�A�n�A�ffA�`BA�dZA�bNA�^5A�dZA�dZA�^5A�\)A�`BA�ZA�ZA�\)A�VA�VA�XA�O�A�Q�A�M�A�G�A�I�A�E�A�A�A�C�A�A�A�=qA�A�A�?}A�;dA�?}A�9XA�9XA�;dA�7LA�-A�1'A�-A�+A�+A�(�A�$�A�(�A�(�A�"�A� �A�$�A� �A� �A�$�A��A��A��A��A�{A��A��A�{A��A��A�oA�{A�{A�bA�VA�oA�JA�JA�bA�VA�1A�VA�
=A�%A�JA�
=A�JA�bA�JA�1A�JA�VA�1A�JA�1A�A�A�A���A���A���A���A���A�  A���A���A�  A���A���A�  A���A���A���A���A���A�  A���A���A�  A���A���A���A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��mA��A��A��mA��A��A��mA��A��yA��yA��A��yA��yA��A��yA��yA��A��mA��yA��A��yA��yA��A��A��yA��A��A��A��yA��A��A��yA��yA��A��mA��mA��A��`A��HA��`A��;A��;A��HA��
A���A���A�ȴA�ĜA�ĜA���A׾wA���A׶FAײ-A״9Aף�Aם�Aח�AדuAׁA�r�A�n�A�jA�=qA�/A�9XA�7LA�7LA�9XA�&�A��A��A��TAֲ-AօA�VA�;dA�(�A��A�JA�bA��A�{A�JA�1A�  A��A��TA���Aգ�A�^5A�+A� �A��A�bA���A��`A��
AԺ^A�n�A�-A�{A�A��`A���AӓuAӃA�v�A�^5A�9XA�+A�oA���A��/AҼjAҲ-Aҟ�Aҏ\A҉7A�|�A�\)A�G�A�9XA�+A��A�JA�
=A�
=A�  A���A��yAљ�A�K�A�;dA�7LA�&�A�{A��A���AЗ�AЋDA�z�A�n�A�dZA�dZA�\)A�I�A�7LA�-A� �A�VA�A���A��`A��/A���A�ƨAϩ�A�n�A�;dA��mA���A�ĜAΣ�A΅A�p�A�^5A�Q�A�33A��A���A��A��mA��HA���A���A�A͟�A̓A�p�A�ZA�?}A�-A�
=A���A̼jA̡�A̕�A�dZA�%A��A��A��#AˋDA��A�M�A��AɼjAɥ�Aɏ\A�v�A�dZA�XA�G�A�1'A�"�A��A�
=A�  A��A�AȑhA�XA�+A���A�ȴAǮAǙ�Aǉ7A�t�A�dZA�XA�M�A�A�A�-A� �A�
=A���AƍPA�p�A�bNA�O�A�A�A�1'A��A���A���Aũ�Aŝ�AŃA�VA�C�A�1'A��A���A��A�Aİ!Aġ�Aę�Ać+A�hsA�7LA�$�A�&�A�&�A��A���A��#A���A�ȴAìA�r�A�?}A��`A¸RA¬AA�ZA�7LA�(�A��A�oA�A���A��A��`A��A���A�XA�&�A��wA��DA�\)A�33A��A��TA���A��PA�bNA�7LA��A�bA�
=A��A��-A�`BA�&�A�JA��A���A���A��A��A�v�A�ffA�S�A�
=A�
=A���A���A��FA�p�A�;dA� �A�oA�%A��mA���A�ƨA��A�~�A�C�A�oA�ȴA��A��\A�t�A�hsA�dZA�\)A�K�A�G�A�-A�oA�bA�JA�A��A��`A���A�ĜA��FA��!A���A��PA�p�A�bA���A��-A���A�M�A��A�1A��A��!A��A��+A�p�A�\)A�G�A�=qA�+A�
=A���A��A��/A���A�A��!A���A��uA�x�A�ffA�I�A�9XA�A�A�9XA�=qA�(�A�bA�A�JA�oA�JA���A��/A���A��!A���A��!A�x�A�S�A�S�A�Q�A�?}A�9XA�1'A�JA��yA��`A��jA���A�ȴA��PA�n�A��A��FA�C�A�%A���A��hA�l�A�`BA�XA�O�A�VA�O�A�C�A�?}A�7LA�1'A��A���A���A��A��`A��
A���A���A��!A���A�~�A�p�A�^5A�VA�O�A�C�A�5?A�/A�-A�$�A��A�{A�VA���A��A���A�A��A�=qA��A�A��A��`A��/A���A���A��RA���A��uA�t�A�/A��A��!A�hsA�;dA�&�A� �A�VA���A��;A�ƨA��-A���A�S�A��FA��+A��A�jA�XA�K�A�E�A�/A��A��A�{A�bA�A���A��A��yA��;A���A���A���A��FA��A���A��hA�jA�M�A�33A���A���A�ƨA�A��jA��!A���A�~�A�p�A�ZA�K�A�A�A�33A��A�JA���A��A��/A��#A��RA��7A�S�A�A�A�9XA�bA��`A���A�dZA��A�~�A��A���A���A�|�A�VA�A�A��+A�XA�1'A��A�A��A���A��A��7A�^5A�bA��A��`A��TA���A�ƨA��RA��A���A���A��\A�v�A�/A��A�oA�1A��A�ĜA���A��+A�ffA�&�A��yA���A���A�~�A�bNA�1'A�A��A��HA�ĜA���A��A�n�A�ZA�E�A�5?A�(�A��A�VA�  A���A��mA���A�ƨA��RA��9A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                              Aٴ9AٶFAٍPA�n�A�ZA�K�A�?}A�;dA�7LA�-A� �A�oA���A��;AظRA؇+A�|�A�t�A�hsA�`BA�ZA�M�A�A�A�;dA�-A�$�A� �A��A�{A�VA�
=A�JA�%A���A���A���A���A��A��A��A��A��A��A��A��A��A��A��A��mA��/A�ĜAק�A�bNA�JA�oA���A�;dA�E�A�O�A�S�Aϥ�A�`BA͝�A�\)A��`A��AǕ�AƧ�AŁA�ffA�"�A�ĜA�9XA�1A�;dA�M�A�=qA��^A�M�A�7LA��PA��A��A���A��A�(�A���A�33A���A��FA�G�A��A��A���A��A�ffA��RA�1A���A�^5A���A���A���A��+A��A��A�VA�v�A�z�A��^A���A��DA��A�bA���A��A���A�~�A��^A� �A��A��FA�M�A��jA�v�AhsA{�
Aw`BAq��Al1'Aj�HAi��Ah�Ah�Af=qA_�-A]��A\�AZ�`AY%AV��AT1AN��AK�AJ�+AH��AF��AES�AC��AAl�A@E�A?dZA=&�A;��A8��A6VA5�A4M�A3`BA1hsA/�wA.�uA-"�A,(�A*$�A'��A& �A$�HA$  A"��A!�wA ^5AdZAbNAJA?}A�AS�A1'A&�A{A�PAC�A~�A�A{AG�A�HAS�A�jAA�A�-A
=A�jAE�AC�A
VA	��A	�;A	`BA1Az�A�#A\)A�A�RA1'A`BAz�A�A �yA z�@�|�@��^@���@�ƨ@��\@�z�@�h@��@�ƨ@�
=@���@���@�@�
=@�Z@��m@��@�9@���@�=q@��@���@� �@�{@�p�@�Q�@�~�@ّh@�%@�Z@ו�@�"�@�dZ@�+@�;d@ץ�@�Ĝ@җ�@�S�@�/@��/@���@�33@�M�@�5?@Ѻ^@�G�@���@�/@Л�@Ϯ@�|�@�+@θR@�v�@͉7@̋D@��
@�+@ʰ!@��#@�O�@ț�@� �@��@��m@ǥ�@��@Ə\@�E�@��@�hs@�O�@��@þw@��@+@��@�J@���@��h@�V@�r�@�1@�;d@�
=@���@���@��+@�v�@�=q@��#@���@��h@���@�9X@��F@�C�@�o@�
=@��@���@�~�@�-@��T@���@�hs@�&�@���@�z�@���@�@��@�ȴ@��\@�n�@�E�@���@��@��#@���@�&�@���@���@�A�@���@��@���@��@�|�@�t�@�l�@�dZ@�S�@�;d@��y@�v�@�{@���@�/@��j@��@�A�@��@��@�ƨ@��@��@��!@��\@�=q@��^@��@��/@��D@�1'@�b@��w@�o@���@��T@�bN@��
@���@�|�@�C�@�@��H@�ȴ@��+@�5?@��@��#@�X@���@��@���@��@�Z@�  @��F@���@�|�@�S�@�C�@���@���@���@�M�@��7@���@��u@��D@�Z@� �@���@�
=@�ȴ@��+@�{@���@��@��@��@���@�z�@�j@�A�@��@���@��
@��w@��@�S�@�;d@�33@��@��@�ff@�J@��^@�p�@��@���@�9X@�b@��@��m@��w@�dZ@�S�@�33@��y@��!@�^5@�@��^@�V@���@��@�z�@�r�@�r�@�Z@�9X@���@���@�C�@�"�@��y@���@��+@�=q@��@��-@�`B@�%@��@�r�@� �@��F@���@�t�@��y@�n�@�$�@���@��7@�X@��/@���@��@�bN@�b@��m@��;@��w@�S�@��@��H@��@�ȴ@�n�@�V@�5?@��@��h@�O�@�V@���@���@��/@��@�r�@�1'@���@���@�K�@��@��@�^5@�=q@��#@���@�X@��@��@��/@��9@��u@�b@�ƨ@���@�K�@���@��@���@���@�ff@�@���@�p�@�/@���@���@��j@��9@��@���@�j@�I�@�1'@��@�P@K�@
=@~ȴ@~��@~$�@}�T@}�T@}�T@}�-@}�h@}�@}p�@}?}@|��@|�@{�m@{�F@{�@{"�@z�H@z�\@zJ@y�^@y��@y��@y7L@x��@xQ�@w�@w|�@w
=@v�R@v�+@vff@v$�@v@u@t�/@t�@t��@t9X@sƨ@sS�@r=q@q�#@q��@q��@qX@p��@p��@p1'@pb@p  @o�@ol�@o;d@n�y@n��@n{@m��@m�-@mV@kdZ@ko@k@j�H@j��@j�!@j��@j~�@j=q@i�^@ihs@iG�@i%@h��@h�9@h�u@hr�@hA�@h1'@hb@gl�@fE�@e@e�@e�@d�/@d�@d�D@dZ@dI�@c��@c��@cdZ@b�H@bM�@a��@a��@ax�@a7L@a�@`��@`��@`Q�@_�P@_K�@^��@^�+@^$�@^@]@]�h@]`B@]�@\�j@\Z@[�
@[�F@[��@[o@Zn�@Y�^@XĜ@X�u@Xr�@X �@W�w@W|�@W\)@W+@W
=@Vȴ@U�T@UO�@T�D@T(�@S�m@S��@St�@S"�@R�@R�!@R�@Q��@Q�7@Q&�@P�@O�@O+@O
=@Nȴ@NE�@M��@Mp�@L��@K��@KS�@J^5@I�#@Ix�@H�`@H��@Hb@Gl�@F�y@F$�@E��@E�h@E`B@E/@D��@C��@CC�@C@B�@B��@B��@B~�@BM�@A�#@Ahs@@�`@@Ĝ@@��@@bN@@ �@?�w@?;d@?+@>�@>��@>V@>E�@>V@>ff@>5?@=�T@=p�@<��@<Z@;��@;�
@;�F@;�@;t�@;dZ@;"�@:��@9��@9�#@9G�@9G�@8r�@7�;@7\)@7;d@7+@7�@7�@6ȴ@6�+@6ff@5�T@5V@4��@4�D@4j@4j@49X@3�F@333@2�@2��@2~�@2M�@2�@2J@1�@1�^@1�7@0��@0Q�@/��@/�@/l�@/;d@.�y@.�R@.��@.{@-@-�@-`B@-O�@-?}@-/@,��@,��@+�
@+33@+o@+@*��@*�!@*�\@*n�@*^5@*=q@*J@)G�@(�`@(�9@(��@(�u@(�@(bN@(A�@'�w@'|�@';d@&��@&��@&��@&�+@&E�@&{@%�@%�@%?}@%�@$��@$Z@$(�@#��@#�F@#t�@#dZ@#S�@#33@#@"�H@"�!@"~�@"=q@"-@!��@!��@!�@!��@!��@!�^@!�7@!&�@ �u@ bN@ A�@ b@�@��@��@|�@K�@
=@�y@ȴ@��@E�@�T@�T@�T@@�h@`B@V@�/@�@�D@z�@I�@�
@��@S�@o@��@J@�#@��@�^@x�@7L@&�@�`@��@r�@ �@  @�w@\)@
=@�R@v�@V@5?@$�@{@@@��@�@`B@�@V@�/@��@�j@�@�D@j@�@�
@��@dZ@C�@"�@�@��@M�@J@�#@�^@hs@&�@�@��@��@�`@��@Ĝ@Ĝ@�9@��@bN@ �@b@  @�@�@�@�;@��@�@l�@\)@;d@
=@�y@ȴ@�R@��@��@��@��@�+@E�@{@�T@��@@��@p�@`B@O�@�@�j@��@�D@j@Z@�@1@�
@�F@�F@��@��@�@dZG�O�AٮAٺ^Aٰ!Aٲ-AٸRAٲ-AټjAٮAٍPAُ\AًDA�n�A�t�A�\)A�\)A�XA�Q�A�O�A�K�A�E�A�C�A�;dA�=qA�=qA�9XA�=qA�;dA�/A�5?A�/A�/A�1'A�$�A��A��A��A�{A�bA�
=A�VA�%A�%A�A���A��A���A��A��TA��mA��;A��A��A���A���A�ȴA�AخAا�A؝�Aؗ�A؅A؁A؃A�z�A�|�A�|�A�|�A�~�A�z�A�z�A�z�A�t�A�r�A�t�A�p�A�t�A�v�A�n�A�n�A�ffA�`BA�dZA�bNA�^5A�dZA�dZA�^5A�\)A�`BA�ZA�ZA�\)A�VA�VA�XA�O�A�Q�A�M�A�G�A�I�A�E�A�A�A�C�A�A�A�=qA�A�A�?}A�;dA�?}A�9XA�9XA�;dA�7LA�-A�1'A�-A�+A�+A�(�A�$�A�(�A�(�A�"�A� �A�$�A� �A� �A�$�A��A��A��A��A�{A��A��A�{A��A��A�oA�{A�{A�bA�VA�oA�JA�JA�bA�VA�1A�VA�
=A�%A�JA�
=A�JA�bA�JA�1A�JA�VA�1A�JA�1A�A�A�A���A���A���A���A���A�  A���A���A�  A���A���A�  A���A���A���A���A���A�  A���A���A�  A���A���A���A���A��A���A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��A��A��A��A��A��A��A��A��A��A��yA��A��A��mA��A��A��mA��A��A��mA��A��yA��yA��A��yA��yA��A��yA��yA��A��mA��yA��A��yA��yA��A��A��yA��A��A��A��yA��A��A��yA��yA��A��mA��mA��A��`A��HA��`A��;A��;A��HA��
A���A���A�ȴA�ĜA�ĜA���A׾wA���A׶FAײ-A״9Aף�Aם�Aח�AדuAׁA�r�A�n�A�jA�=qA�/A�9XA�7LA�7LA�9XA�&�A��A��A��TAֲ-AօA�VA�;dA�(�A��A�JA�bA��A�{A�JA�1A�  A��A��TA���Aգ�A�^5A�+A� �A��A�bA���A��`A��
AԺ^A�n�A�-A�{A�A��`A���AӓuAӃA�v�A�^5A�9XA�+A�oA���A��/AҼjAҲ-Aҟ�Aҏ\A҉7A�|�A�\)A�G�A�9XA�+A��A�JA�
=A�
=A�  A���A��yAљ�A�K�A�;dA�7LA�&�A�{A��A���AЗ�AЋDA�z�A�n�A�dZA�dZA�\)A�I�A�7LA�-A� �A�VA�A���A��`A��/A���A�ƨAϩ�A�n�A�;dA��mA���A�ĜAΣ�A΅A�p�A�^5A�Q�A�33A��A���A��A��mA��HA���A���A�A͟�A̓A�p�A�ZA�?}A�-A�
=A���A̼jA̡�A̕�A�dZA�%A��A��A��#AˋDA��A�M�A��AɼjAɥ�Aɏ\A�v�A�dZA�XA�G�A�1'A�"�A��A�
=A�  A��A�AȑhA�XA�+A���A�ȴAǮAǙ�Aǉ7A�t�A�dZA�XA�M�A�A�A�-A� �A�
=A���AƍPA�p�A�bNA�O�A�A�A�1'A��A���A���Aũ�Aŝ�AŃA�VA�C�A�1'A��A���A��A�Aİ!Aġ�Aę�Ać+A�hsA�7LA�$�A�&�A�&�A��A���A��#A���A�ȴAìA�r�A�?}A��`A¸RA¬AA�ZA�7LA�(�A��A�oA�A���A��A��`A��A���A�XA�&�A��wA��DA�\)A�33A��A��TA���A��PA�bNA�7LA��A�bA�
=A��A��-A�`BA�&�A�JA��A���A���A��A��A�v�A�ffA�S�A�
=A�
=A���A���A��FA�p�A�;dA� �A�oA�%A��mA���A�ƨA��A�~�A�C�A�oA�ȴA��A��\A�t�A�hsA�dZA�\)A�K�A�G�A�-A�oA�bA�JA�A��A��`A���A�ĜA��FA��!A���A��PA�p�A�bA���A��-A���A�M�A��A�1A��A��!A��A��+A�p�A�\)A�G�A�=qA�+A�
=A���A��A��/A���A�A��!A���A��uA�x�A�ffA�I�A�9XA�A�A�9XA�=qA�(�A�bA�A�JA�oA�JA���A��/A���A��!A���A��!A�x�A�S�A�S�A�Q�A�?}A�9XA�1'A�JA��yA��`A��jA���A�ȴA��PA�n�A��A��FA�C�A�%A���A��hA�l�A�`BA�XA�O�A�VA�O�A�C�A�?}A�7LA�1'A��A���A���A��A��`A��
A���A���A��!A���A�~�A�p�A�^5A�VA�O�A�C�A�5?A�/A�-A�$�A��A�{A�VA���A��A���A�A��A�=qA��A�A��A��`A��/A���A���A��RA���A��uA�t�A�/A��A��!A�hsA�;dA�&�A� �A�VA���A��;A�ƨA��-A���A�S�A��FA��+A��A�jA�XA�K�A�E�A�/A��A��A�{A�bA�A���A��A��yA��;A���A���A���A��FA��A���A��hA�jA�M�A�33A���A���A�ƨA�A��jA��!A���A�~�A�p�A�ZA�K�A�A�A�33A��A�JA���A��A��/A��#A��RA��7A�S�A�A�A�9XA�bA��`A���A�dZA��A�~�A��A���A���A�|�A�VA�A�A��+A�XA�1'A��A�A��A���A��A��7A�^5A�bA��A��`A��TA���A�ƨA��RA��A���A���A��\A�v�A�/A��A�oA�1A��A�ĜA���A��+A�ffA�&�A��yA���A���A�~�A�bNA�1'A�A��A��HA�ĜA���A��A�n�A�ZA�E�A�5?A�(�A��A�VA�  A���A��mA���A�ƨA��RA��9A���A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�"B
��B
�(B
� B
�"B
�"B
�"B
��B
��B
�VB
��B
�"B
��B
��B
��B
��B
�(B
��B
��B
�"B
�VB
��B
�"B
��B
��B
�"B
�VB
�VB
��B
�"B
��B
�\B
�\B
��B
�(B
�(B
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
�(B
�(B
�\B
��B
�bB
�oB
�$B
�-B
�	BDgBi�B�hB��B�}B��B��BAB��B  B�(B��B�B�BDB�BVB%�B2�B7B8�B7�B7�B4nB;0B=<B@B?�BFtB@�B@OB=<B=�B:^B:^B8�B49B4nB6FB1�B+kB!�B�B�B�cB�%B��B�BB՛B�jB�B�~Bs�Bd&BH�B(�B�B
�8B
��B
�B
��B
�@B
��B
�lB
�4B
wfB
_pB
T�B
?HB
OB
?B
'RB
@B	�B	�5B	֡B	�BB	�EB	��B	��B	�.B	��B	~]B	p�B	d&B	V�B	@B	'RB	!�B	qB	4B	xB	AB��B�GB�;B�B��B�|B��B�B�)BɆBŢB��B��B�B�?B�zB��B��B�B�XB�eB�B�nB��B��B��B�\B��B�\B��B�\B��B��B�B�IB��B��B��B�MB�1B�uB��B�bB�VB��B�B��B��B�	B�B��B��B��B�lB��B��B�%B��B�MB��B�GB��B�B�;B��B��B��B��B��B��B�YB��B��B��B��B��B��B�rB�7B�PB�B��B��B��B�_B��B��B��B�FB�B��B�B��B�$B��BÖBȀB�B��B�BٴBܒB�B��B��B		�B	�B	�B	B	B	&LB	0UB	3�B	1�B	4B	5�B	7�B	:�B	@�B	CaB	FtB	J�B	Q�B	ZB	_�B	e�B	hsB	j�B	j�B	l�B	o�B	p;B	qvB	s�B	w�B	|PB	~]B	��B	�_B	�lB	�=B	�=B	��B	�"B	� B	�MB	��B	��B	�-B	��B	��B	�LB	�B	��B	�6B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�XB	��B	��B	�0B	��B	��B	��B	��B	�B	��B	�EB	ǮB	�KB	ȀB	ɆB	�XB	�XB	ʌB	˒B	͟B	�BB	� B	�B	�sB	�B	�EB	خB	��B	��B	�B	�B	�B	�B	یB	�/B	�;B	��B	�B	�TB	�B	�B	�2B	�B	�8B	��B	��B	�B	�B	��B	�B	��B	�|B	�B	�B	�B	��B	�ZB	��B	�2B	�lB	�	B	��B	�	B	��B	��B	��B	��B	�B	�"B	�"B	��B	�]B	��B	��B	�cB	��B
;B
;B
�B
GB
B
�B
�B
�B
�B
	B

�B
�B
JB
�B
�B
�B
xB
PB
�B
�B
\B
4B
4B
4B
MB
�B
$B
YB
�B
_B
�B
1B
�B
�B
B
IB
�B
�B
B
VB
 'B
 �B
!-B
!�B
"4B
$B
$tB
$�B
$�B
$�B
%B
%�B
%�B
&B
&LB
&�B
'B
&�B
($B
(XB
)_B
*eB
*�B
*eB
*eB
*�B
*�B
+B
+�B
+�B
,B
,�B
,�B
-wB
-�B
.IB
.}B
.�B
/B
/�B
/�B
0UB
0�B
0�B
0�B
1�B
2-B
2�B
2�B
3hB
3hB
4nB
5B
5B
4�B
5B
5?B
5tB
5�B
6�B
6�B
7B
6�B
7B
7�B
7�B
7�B
7�B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9XB
9$B
9�B
9�B
9�B
:*B
:�B
:�B
<B
<6B
=B
=<B
=B
=B
=<B
=<B
>BB
>BB
>�B
?HB
?HB
?B
?HB
?HB
?}B
@B
@�B
@�B
A�B
A�B
B'B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
C�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
EmB
EmB
E�B
F?B
F?B
FB
FtB
GB
HB
HB
HB
HB
H�B
H�B
IB
I�B
I�B
J#B
I�B
JXB
J�B
K^B
K)B
K)B
K�B
K�B
K�B
L0B
LdB
L0B
L�B
MjB
M�B
MjB
M�B
M�B
NB
OvB
OB
OB
OB
O�B
O�B
PB
P�B
P}B
PHB
P}B
P�B
P�B
P�B
QB
Q�B
Q�B
QB
R�B
S[B
S[B
S&B
S�B
S[B
S�B
S[B
S�B
S�B
TaB
T,B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
TaB
TaB
UgB
VB
VmB
VmB
V�B
W?B
WsB
W�B
W�B
W�B
XB
W�B
XEB
XyB
X�B
X�B
X�B
YKB
YKB
YB
YB
YB
ZQB
Z�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
\]B
\�B
]�B
^5B
^B
^B
^�B
^�B
^�B
^�B
_B
_B
_pB
`B
`B
aHB
aB
a|B
a|B
a|B
a�B
a�B
bB
b�B
b�B
b�B
cTB
c�B
dZB
d�B
d�B
d�B
e`B
e`B
e�B
e�B
f�B
g8B
h
B
h>B
hsB
h�B
h�B
iyB
iyB
jKB
jB
j�B
kB
j�B
j�B
j�B
l"B
l�B
l�B
m)B
m)B
m)B
m]B
m]B
m�B
n/B
ncB
m�B
ncB
n�B
n�B
o5B
o5B
o5B
oiB
o�B
pB
pB
pB
p;B
p�B
p�B
qvB
rB
rGB
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
t�B
u�B
u�B
v+B
v`B
v�B
v�B
v�B
wfB
wfB
w2B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
y>B
y�B
y�B
y�B
zB
zDB
zxB
zxB
zxB
zxB
zxB
{B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}"B
}"B
|�B
}"B
}�B
~(B
~�B
~�B
~�B
~�B
~�B
~�B
.B
~�B
.B
.B
�4B
� B
�4B
�4B
�4B
�4B
�4B
�4B
�oB
�;B
�oB
��B
�B
�B
�AB
�AB
�AB
�uB
�B
�GB
�GB
�GB
��B
��B
�B
��B
��B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
��B
��B
�%B
��B
��B
�%B
�YB
�+B
��B
��B
�+B
�+B
�_B
�_B
��B
��B
��B
�1B
�1B
�fB
�B
�B
�B
�B
�7B
�7B
��B
��B
�	B
�=B
�=B
�=B
��B
�DB
�DB
��B
��B
�B
��B
��B
��B
��B
�B
�B
�PB
�PB
��B
��B
�VB
�"B
��B
��B
�\B
��B
��B
��B
�.B
�.B
�.B
�.B
�bB
��B
��B
� B
� B
�4B
�hB
�hB
��B
�hB
��B
��B
�:B
�oB
��B
��B
��B
��B
�@B
�uB
��B
��B
�B
�B
�{B
��B
��B
�B
�B
�B
�MB
�MB
�MB
�MB
��B
��B
��B
��B
��B
�B
��B
�B
�B
�B
�SB
�SB
�B
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
�YB
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
�1B
��B
�eB
�B
�B
�B
�7B
�B
�7B
�7B
��B
��B
�xB
�bB
�VB
��B
��B
�JB
��B
�PB
��B
�"B
��B
�B
��B
��B
�\B
�(B
�PB
�VB
�(B
�~B
��B
�VB
��B
��B
�B
��B
��B
�JB
��B
�PB
�PB
��B
�bB
��B
��B
��B
��B
�(B
��B
�(B
��B
�(B
��B
��B
��B
��B
�.B
�PB
�VB
��B
��B
�hB
��B
�VB
��B
��B
��B
� B
��B
��B
�bB
�"B
�bB
��B
�(B
��B
��B
��B
�.B
��B
��B
��B
��B
�(B
�(B
��B
��B
�(B
��B
�(B
�~B
�B
�VB
��B
�B
��B
��B
��B
��B
�"B
��B
��B
�(B
��B
��B
��B
�VB
��B
��B
��B
�\B
��B
�VB
�(B
��B
��B
��B
��B
��B
��B
��B
�VB
�(B
�B
�"B
�(B
�B
��B
��B
�PB
�B
��B
��B
��B
��B
�VB
��B
�\B
�(B
��B
��B
�(B
��B
�PB
�(B
�PB
�VB
�.B
��B
�PB
��B
�(B
��B
��B
��B
�B
�VB
�\B
�PB
��B
��B
��B
��B
�\B
�VB
��B
�bB
��B
��B
��B
��B
��B
��B
�VB
��B
��B
��B
�"B
��B
�\B
��B
��B
�\B
��B
��B
�\B
�VB
�(B
�.B
�"B
��B
�bB
��B
��B
��B
�\B
��B
�bB
�"B
��B
��B
��B
��B
��B
��B
�\B
��B
��B
��B
�(B
�PB
��B
�(B
��B
��B
�\B
�VB
�B
��B
�(B
��B
��B
��B
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
�"B
��B
��B
�"B
��B
��B
��B
�\B
�PB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�(B
��B
�"B
��B
�.B
��B
�"B
�\B
��B
��B
�"B
��B
�\B
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
� B
��B
��B
�bB
��B
�VB
�.B
��B
��B
��B
�.B
��B
��B
��B
�bB
� B
�oB
�oB
��B
��B
��B
�@B
�4B
��B
�{B
�.B
�_B
�:B
��B
�IB
��B
��B
��B
��B
�B
��B
��B
�kB
��B
��B
�B
��B
��B
�B
��B
�?B
бB
�B
�QB
�B
�B
��B
�vB
�ZB
�2B
�"B�B�BqB!B#nB+�B6�B8B8�B=qBE�BF�BK�BQ�BVB[�B[WB_�Bb�Ba�Bc�BkBl�BlWBn�Bn/Bm�BkQBk�Bn�Bl�Br�B�1B��B�SB��B��B�xB�tB�qB�3B��B�B��B�0B��B�0B�}B�OB� B��B�B�?B��B��BȴB��B�RB�pB�B��B�B�B��B��B�B��B�B��B�B��B�B�B��B��B�B�xB��B��BoBB �B;B iBB��BB�.B��B�B�B��B�fB�BB�BB��B�>B�+B�MB�B�"B��B��B�.B  B��B��B��B �B �B�B�B��BBoB�.B��B��B��B�(B�VB��B�B��B�B�DB iB��B��B�rB�PB��B�PB�B�cBB iB�BB�BB�B�BfB�BSB�B�B�B�B�B�B�B%B�BfB�B�B�BB1B	lB
	B�B
	BPB�BbB(B"B"B�BbB�B\B"B�BkBBMB#B�B_BFBFBB5tB%zB$@B!�B#:B!�B�B$tB*eB-CB$�B%zB&B&�B'�B&�B"�B"�B#nB"hB.�BHB2aBCaB8RB1[B3hB/�B-�B/�B4B1�B/OB2�B:�B8B:�B?�B8�B:�B:^B8�B7LB7�B;dB6B;0B6�B7�B8�B6B:�B8�B:^B8RB7�B5�B6FB6FB9�BC�B7�B1'B5B7LB8�B4nB7�B5tB1�B0!B7�B9$B0UB2-B2�B:*B5tB2-B9�B8�B6FB9$B:^B9XB<�B?�B>�B<6B:^B8B;dB9�B<�B>B:^B;�B<jBC�B?}B@�BF?B<jB:*B7�BA BA B=�BC�B?�B>�B?HBB'B<�BD3B9�B6zBN<BB�BB[BL0BN�BD�BL�BFtBCaB@�BB'B@�B?}B@BA B@�B?�B@BC�BDgB?�B>wB@�BA�B?B?BA�BC�B@B>�B@OB>�B>B=�B>�B=�B<6B;�B<�B<jB:�B<6B?HB;0B9�BGEB>�B>�B<�B<B:�B9�B6zB9XB9�B<B7�B<B?�B>BBA�B=<B:�B8B6zB7�B6�B7�B6�B5�B5B@BDgB9$B1�B8B4�B5�B3�B3�B5?B4B2aB2-B4�B5B2�B3hB5tB5B4B4B3�B5?B4nB5�B8B8RB6FB@B3�B4�B2�B0�B2�B5�B33B1[B1�B/�B0�B0!B/OB.�B.�B-�B-B+B0�B+6B(�B!bB�B$�B$�B(�B$�B'B)�B�B�B�B�B�B�B�B�BxB�B�B%B�BSB�B�B+B
	B�B�VB�]B;BBoB��B��B�xB�VB 4B�B��B��B��B�B��B�B�oB��B�MB�B��B�WB�B�sB�]B��B��B�B�B��B�pB��B��B�dB�)B�yB�yB�EB�sB�B��B�aB��B�[B�B��B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022081716382620220817163826IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022082619012120220826190121QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022082619012120220826190121QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                