CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-08-13T15:07:10Z creation; 2020-06-20T00:40:10Z DMQC;      
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
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190813150710  20210326170157  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               
   
AA  AOAO7836_008777_010                 7836_008777_010                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�Ԧ����@�Ԧ����11  @�ԧ�)_@�ԧ�)_@<��K3�@<��K3��d/A�!��d/A�!�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�@B�\@�  @�  @�  @޸RA   A��A ��A,(�A@��A`  A�  A���A�  A��A��A�
=A�  A���B Q�B�
B�
B�
B   B((�B0(�B8Q�B@  BH  BP(�BW�
B_�
Bh  Bp  Bx  B�B��
B�{B�  B�{B�  B��B�  B�  B��B�  B�{B�{B�  B�  B�{B�{B�  B�{B�{B�{B�(�B�{B�{B�  B�  B�  B��B�  B�  B��B�{C 
=C  C
=C
=C��C	��C  C  C��C  C
=C
=C��C�C  C
=C   C"  C$
=C&
=C(  C*  C,{C.
=C/��C2
=C4{C6  C8
=C:
=C<  C>  C@{CB
=CD{CF{CG��CI��CL  CM��CO��CR
=CT
=CU�CX  CZ{C\
=C^
=C`
=Cb
=Cd
=Cf  Ch
=Cj  Ck��Cm�Cp  Cr
=Ct  Cv{Cx{Cz
=C|
=C~{C�  C���C���C�  C�C���C���C�  C�  C�  C���C���C���C�  C�  C�  C�C�C���C���C�  C�  C�  C�
=C�  C���C�C�C�  C���C���C���C���C�C�  C���C�  C�
=C�  C���C�C�C�C�
=C�C�  C���C���C���C�  C�C���C�  C�  C�
=C�  C���C���C�C�  C�C�C�  C�
=C�
=C�  C�  C�  C�  C�  C���C���C���C�C�
=C�C�C�
=C�
=C�
=C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�C�C���C���C�  C���C��C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�  C���C�C�
=C�
=C�C�\C�\C�C���C�C�C�C���C���D ��D�D� D  D� D��D}qD  D� D�D��D�qD��D�D�D  D� D�qD	� D
�D
��D  D��D�D��D�D��DD�D  D}qD��Dz�D�qD}qD��D}qD  D� D�D��D  D� D�D}qD�RDz�D  D��D�D� D�D� D  D��D�D� D��D}qD�qD}qD  D� D�qD � D!  D!z�D!��D"z�D"�qD#}qD#�qD$}qD%  D%� D&  D&��D'�D'� D(�D(��D(�qD)z�D)�qD*��D+  D+}qD,  D,��D-  D-� D-�qD.}qD.�qD/� D0�D0}qD1  D1}qD1�qD2}qD2�qD3�D4D4� D4�qD5z�D6  D6��D7  D7z�D8  D8�D9  D9� D:  D:� D:�qD;}qD<  D<}qD<�qD=}qD>  D>� D?  D?� D@�D@� D@�qDA}qDB�DB�DB�qDC}qDD�DD��DE  DE}qDE��DFz�DG  DG� DH  DH��DH�qDI��DI�qDJz�DK�DK�DL�DLz�DL�qDM��DN  DN� DN��DOxRDO�qDP��DQ�DQ��DR  DR}qDR��DS}qDT�DT� DU�DU}qDU�qDV� DW  DW��DX�DX� DY  DY��DZ  DZ}qDZ�qD[}qD[��D\� D\�qD]}qD]��D^}qD_  D_��D`�D`��Da�Da��DbDb�DcDc�Dc�qDd��De�De}qDe�qDf� Dg  Dg}qDg��Dhz�Di  Di��Dj  Dj}qDj��Dk}qDl  Dl� DmDmz�Dm�qDn� Dn��Dou�Do�qDp�DqDq� Dq��Dr}qDs  Ds� DtDt}qDt��Du� Dv�Dv�DwDw� Dw�qDx��Dy�Dy}qDz  Dz}qD{  D{��D|  D|z�D|�qD}}qD~�D~��D  D� D�  D�@ D��HD�D�HD�@ D���D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD�}qD�� D�HD�AHD�~�D���D�HD�@ D�� D�� D���D�>�D��HD��HD�HD�B�D���D�� D���D�@ D�~�D���D�HD�@ D��HD��HD��D�@ D�� D�� D���D�@ D��HD��HD���D�@ D�� D�� D�  D�=qD�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D���D�@ D�}qD��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD���D�D�  D�@ D��HD�D�HD�>�D��HD���D�  D�>�D�~�D��HD��D�AHD�~�D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD���D���D���D�B�D���D�� D�  D�@ D�� D�� D�  D�<)D�}qD�� D�  D�@ D�� D�� D�HD�>�D�� D�� D�  D�AHD�~�D��HD�HD�>�D��HD���D���D�@ D�~�D�� D��D�>�D�� D�D�  D�=qD�~�D���D��D�@ D�� D��HD�  D�>�D��HD�� D�HD�AHD��HD��HD�HD�>�D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D���D�  D�AHD�}qD��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD���D�  D�@ D�� D�� D�HD�>�D��HD��HD��D�>�D�� D�D�HD�AHD�� D��HD�HD�AHD��HD��HD��D�AHD�~�D�� D�HD�@ D�� D�D�HD�AHD�� D��HD�HD�>�D�� D���D�HD�>�D�~�D��qD���D�>�D�}qD�� D�  D�@ D�� D���D�HD�@ D�� D�D�  D�@ D��HD�� D���D�>�D�~�D½qD�  D�AHD�~�D�� D���D�@ DāHD��HD���D�AHDŀ D�� D�  D�@ D�~�Dƾ�D�HD�@ DǁHD��HD�  D�>�DȁHD��HD�  D�@ Dɀ D�� D�  D�>�Dʀ Dʾ�D���D�>�DˁHD˾�D�  D�>�D́HD��HD�HD�AHD�~�D;�D�HD�AHD�~�DνqD�  D�@ Dπ DϽqD���D�@ D�~�Dо�D�  D�@ Dр D��HD�  D�=qDҀ DҾ�D�HD�@ DӀ DӾ�D���D�AHDԁHD�� D�  D�AHDՀ D�� D�  D�@ Dր D־�D�  D�>�D�~�D׾�D���D�@ D؁HD�� D��D�@ D�~�Dپ�D��qD�>�D�~�D�� D�HD�@ Dۀ D۾�D�  D�B�D܀ D�� D�  D�@ D݀ Dݾ�D�  D�@ D�}qD�� D�  D�@ D�~�D�� D���D�>�D�~�D�D���D�AHD�HD�qD�  D�AHD�~�D�� D�HD�AHD� D��HD�  D�B�D䂏D�D�  D�>�D�HD��HD��D�AHD� D�qD��qD�=qD�HD�� D��qD�>�D�}qD�� D���D�>�D邏D�� D�  D�>�D�}qD�qD�  D�AHD낏D��HD�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD� D��HD�HD�@ D� D�� D�HD�AHD�~�D�D�HD�AHD� D�qD�  D�AHD�~�D�D�HD�>�D�HD��HD�HD�>�D�|)D���D��D�@ D�� D��HD�HD�AHD�~�D��qD�HD�@ D���D�� D��qD�=qD�� D��HD���D�>�D�~�D���D���D�AHD���D��H?��?#�
?k�?��R?�p�?�(�?�@�@��@(��@=p�@J=q@Tz�@^�R@p��@��\@��@���@�
=@�p�@��@�\)@�Q�@�  @��
@˅@��@�(�@��
@�@�33@�Q�@�p�A�AffA
�HA�RA�\A�A�A=qA�RA#33A'
=A(��A+�A.�RA3�
A7�A:=qA<��A@  AA�AFffAJ�HAN�RAQG�AS�
AVffAZ=qA^{Ab�\Ae�Ag�Aj�HAmp�Aq�AuAx��A|��A\)A���A��\A��
A�{A�  A��A��A�z�A�A��RA���A��HA��A�z�A�p�A�\)A���A��A��HA��A�z�A�ffA�  A���A���A��\A��A�p�A�
=A��A�Q�A���A��HA�z�A�{A��RA��A�Q�A��A��A��A�{A�
=A�  A�G�A��\A���A�ffA�\)A�Q�A�G�A�33A��A�
=A�Q�A�G�Aʏ\A�(�A�{A�  A��A��
A�{A�Q�Aڏ\A���A޸RA���A��HA��A�A陚A�A�A�A�A��
A�A�  A��A�(�A�ffB (�BG�BffB�Bz�B��B�RB�
B	�B	�B
=B  B�B{B33BQ�Bp�B�\B�B��B��B�RB�
B��B�B
=B(�BG�B=qB\)B z�B!��B"�RB#�
B$��B&{B'33B(Q�B)p�B*�\B+�
B-�B.=qB/\)B0z�B1��B2�RB3�B4��B5�B7
=B8(�B9G�B:ffB;\)B<z�B=��B>�RB?�
B@��BB=qBC�BE�BFffBG�BH��BI�BK
=BL  BM�BNffBO�
BP��BR=qBS�BT��BV=qBW\)BX��BYB[
=B\(�B]p�B^�HB`Q�BaBc33Bdz�Bf{Bg�Bh��Bj=qBk�Bl��Bn{Bo\)Bp��Br=qBs�Bu�Bv�\Bx  ByB{
=B|Q�B}��B~�HB�{B��RB�p�B�(�B���B��B�z�B�33B��B��\B�33B��B��\B�33B�B�z�B�\)B�  B���B��B�(�B��RB�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B��\B��B���B�  B�z�B���B�p�B��
B�=qB���B�p�B�  B��\B�
=B�p�B��B�Q�B��RB�33B�B�Q�B���B�p�B�  B�ffB���B�G�B��B�=qB��RB�G�B��
B�ffB��HB�G�B��B�{B�z�B�
=B���B�  B���B�33B���B�  B�ffB���B�G�B��
B�ffB���B�p�B��
B�=qB���B�
=B���B�{B���B�G�B�B�(�B���B���B�p�B��B�z�B�
=B��B�  B�Q�B��RB�33B��B�(�B��RB�G�B�B�(�B���B�
=B�\)B�B�Q�B��HB�p�B��B�=qB���B���B�p�B�  B��\B��B��B��
B�(�B\B��BîB�=qB���B�33BŅB��
B�=qB���B�G�B�B�Q�B���B�33BɅB��B�Q�B���B�G�B��
B�ffB���B��BͅB��
B�Q�B���B�\)B��B�ffB���B�33BхB��
B�Q�B��HB�\)B��B�ffBԸRB��B�p�B��B�z�B��HBׅB��B�Q�Bأ�B���B�p�B�  Bڏ\B���B�G�BۮB�  B�z�B���B݅B�{B�z�B���B��B߅B��B�ffB��HB�p�B��B�Q�B��B���B�G�B�B�Q�B���B�\)B�B�{B�Q�B���B�\)B�B�Q�B���B�
=B�p�B�B�=qB��B�33B�B�  B�ffB��B�
=B홚B�{B�\B�
=B�\)BB�  B�ffB���B�B�  B�Q�B��B���B�B�{B���B�
=B�\)B�B�{B���B�33B�B�(�B�z�B���B�\)B��
B�ffB��HB�G�B��B�  B�Q�B��HB�\)B��
B�z�B��HB�G�B���B��C =qC z�C ��C  C(�CQ�C�C�
C
=CQ�C��C�RC�C{CQ�C��C�HC(�C\)C�C�RC��C33C�CC��C�CG�C��C�
C(�C\)C�C�RC�C(�Cp�C�RC�C	�C	G�C	z�C	C

=C
Q�C
�\C
�RC
�C{CQ�C��C�HC�CQ�C�C��C�C(�Cp�C�RC�HC
=C=qC�\C��C{C=qCp�C��C�
C(�CffC��C�
C
=C33CffC�C  C33CffC�\CC
=C\)C��C��C��C(�C\)C��C�C33CffC��CC  CG�C�\C��C  C(�C\)C�C�C=qCffC��C��C
=C\)C��C��C��C33Cz�CC
=C33CffC�\C�
C�CffC�\CC��C33Cp�CC�C�CG�C�\C�
C�CQ�Cz�C�C   C G�C z�C �C �
C!(�C!p�C!�C!�HC"
=C"Q�C"��C"�C#{C#=qC#�C#��C$�C$Q�C$�C$�C$�C%=qC%z�C%C%��C&�C&Q�C&��C&�C'33C'\)C'�\C'C(
=C(Q�C(��C(�
C)
=C)=qC)z�C)C*
=C*Q�C*z�C*�RC*�C+33C+�C+��C+��C,(�C,\)C,�C,��C-=qC-p�C-��C-�
C.(�C.p�C.�RC.�C/�C/Q�C/��C/�C0=qC0p�C0��C0��C1{C1ffC1�RC1�C2�C2Q�C2��C2��C3=qC3ffC3��C3�C433C4�\C4C4��C5(�C5p�C5��C6{C6Q�C6�C6�RC7
=C7Q�C7�C7�
C8  C8Q�C8��C8�C9(�C9Q�C9��C9�C:=qC:p�C:��C:�C;33C;�\C;C;��C<G�C<�\C<�HC=�C=G�C=��C=�C>=qC>p�C>��C>�C?=qC?��C?�
C@
=C@G�C@��C@�CA=qCAp�CA��CA��CBQ�CB��CB�
CC{CCG�CC��CC��CD=qCD�CD�RCD��CEG�CE��CE��CF(�CFffCF��CF��CGQ�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          ?�=q@�@B�\@�  @�  @�  @޸RA   A��A ��A,(�A@��A`  A�  A���A�  A��A��A�
=A�  A���B Q�B�
B�
B�
B   B((�B0(�B8Q�B@  BH  BP(�BW�
B_�
Bh  Bp  Bx  B�B��
B�{B�  B�{B�  B��B�  B�  B��B�  B�{B�{B�  B�  B�{B�{B�  B�{B�{B�{B�(�B�{B�{B�  B�  B�  B��B�  B�  B��B�{C 
=C  C
=C
=C��C	��C  C  C��C  C
=C
=C��C�C  C
=C   C"  C$
=C&
=C(  C*  C,{C.
=C/��C2
=C4{C6  C8
=C:
=C<  C>  C@{CB
=CD{CF{CG��CI��CL  CM��CO��CR
=CT
=CU�CX  CZ{C\
=C^
=C`
=Cb
=Cd
=Cf  Ch
=Cj  Ck��Cm�Cp  Cr
=Ct  Cv{Cx{Cz
=C|
=C~{C�  C���C���C�  C�C���C���C�  C�  C�  C���C���C���C�  C�  C�  C�C�C���C���C�  C�  C�  C�
=C�  C���C�C�C�  C���C���C���C���C�C�  C���C�  C�
=C�  C���C�C�C�C�
=C�C�  C���C���C���C�  C�C���C�  C�  C�
=C�  C���C���C�C�  C�C�C�  C�
=C�
=C�  C�  C�  C�  C�  C���C���C���C�C�
=C�C�C�
=C�
=C�
=C�C�  C�  C�C�  C�  C�  C�  C�  C�  C�  C�  C���C�  C�  C�C�C���C���C�  C���C��C�  C�C�  C���C�  C�C�C�C�C�  C�  C�  C�  C���C�C�
=C�
=C�C�\C�\C�C���C�C�C�C���C���D ��D�D� D  D� D��D}qD  D� D�D��D�qD��D�D�D  D� D�qD	� D
�D
��D  D��D�D��D�D��DD�D  D}qD��Dz�D�qD}qD��D}qD  D� D�D��D  D� D�D}qD�RDz�D  D��D�D� D�D� D  D��D�D� D��D}qD�qD}qD  D� D�qD � D!  D!z�D!��D"z�D"�qD#}qD#�qD$}qD%  D%� D&  D&��D'�D'� D(�D(��D(�qD)z�D)�qD*��D+  D+}qD,  D,��D-  D-� D-�qD.}qD.�qD/� D0�D0}qD1  D1}qD1�qD2}qD2�qD3�D4D4� D4�qD5z�D6  D6��D7  D7z�D8  D8�D9  D9� D:  D:� D:�qD;}qD<  D<}qD<�qD=}qD>  D>� D?  D?� D@�D@� D@�qDA}qDB�DB�DB�qDC}qDD�DD��DE  DE}qDE��DFz�DG  DG� DH  DH��DH�qDI��DI�qDJz�DK�DK�DL�DLz�DL�qDM��DN  DN� DN��DOxRDO�qDP��DQ�DQ��DR  DR}qDR��DS}qDT�DT� DU�DU}qDU�qDV� DW  DW��DX�DX� DY  DY��DZ  DZ}qDZ�qD[}qD[��D\� D\�qD]}qD]��D^}qD_  D_��D`�D`��Da�Da��DbDb�DcDc�Dc�qDd��De�De}qDe�qDf� Dg  Dg}qDg��Dhz�Di  Di��Dj  Dj}qDj��Dk}qDl  Dl� DmDmz�Dm�qDn� Dn��Dou�Do�qDp�DqDq� Dq��Dr}qDs  Ds� DtDt}qDt��Du� Dv�Dv�DwDw� Dw�qDx��Dy�Dy}qDz  Dz}qD{  D{��D|  D|z�D|�qD}}qD~�D~��D  D� D�  D�@ D��HD�D�HD�@ D���D��HD�HD�@ D�~�D�� D�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�AHD�}qD�� D�HD�AHD�~�D���D�HD�@ D�� D�� D���D�>�D��HD��HD�HD�B�D���D�� D���D�@ D�~�D���D�HD�@ D��HD��HD��D�@ D�� D�� D���D�@ D��HD��HD���D�@ D�� D�� D�  D�=qD�� D�� D�  D�AHD���D�� D�  D�@ D�~�D���D���D�@ D�}qD��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�>�D�� D��HD�HD�AHD���D�D�  D�@ D��HD�D�HD�>�D��HD���D�  D�>�D�~�D��HD��D�AHD�~�D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�HD�AHD���D���D���D�B�D���D�� D�  D�@ D�� D�� D�  D�<)D�}qD�� D�  D�@ D�� D�� D�HD�>�D�� D�� D�  D�AHD�~�D��HD�HD�>�D��HD���D���D�@ D�~�D�� D��D�>�D�� D�D�  D�=qD�~�D���D��D�@ D�� D��HD�  D�>�D��HD�� D�HD�AHD��HD��HD�HD�>�D�� D�� D�  D�@ D�~�D�� D�  D�>�D�� D���D�  D�AHD�}qD��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�AHD��HD��HD�HD�AHD��HD���D�  D�@ D�� D�� D�HD�>�D��HD��HD��D�>�D�� D�D�HD�AHD�� D��HD�HD�AHD��HD��HD��D�AHD�~�D�� D�HD�@ D�� D�D�HD�AHD�� D��HD�HD�>�D�� D���D�HD�>�D�~�D��qD���D�>�D�}qD�� D�  D�@ D�� D���D�HD�@ D�� D�D�  D�@ D��HD�� D���D�>�D�~�D½qD�  D�AHD�~�D�� D���D�@ DāHD��HD���D�AHDŀ D�� D�  D�@ D�~�Dƾ�D�HD�@ DǁHD��HD�  D�>�DȁHD��HD�  D�@ Dɀ D�� D�  D�>�Dʀ Dʾ�D���D�>�DˁHD˾�D�  D�>�D́HD��HD�HD�AHD�~�D;�D�HD�AHD�~�DνqD�  D�@ Dπ DϽqD���D�@ D�~�Dо�D�  D�@ Dр D��HD�  D�=qDҀ DҾ�D�HD�@ DӀ DӾ�D���D�AHDԁHD�� D�  D�AHDՀ D�� D�  D�@ Dր D־�D�  D�>�D�~�D׾�D���D�@ D؁HD�� D��D�@ D�~�Dپ�D��qD�>�D�~�D�� D�HD�@ Dۀ D۾�D�  D�B�D܀ D�� D�  D�@ D݀ Dݾ�D�  D�@ D�}qD�� D�  D�@ D�~�D�� D���D�>�D�~�D�D���D�AHD�HD�qD�  D�AHD�~�D�� D�HD�AHD� D��HD�  D�B�D䂏D�D�  D�>�D�HD��HD��D�AHD� D�qD��qD�=qD�HD�� D��qD�>�D�}qD�� D���D�>�D邏D�� D�  D�>�D�}qD�qD�  D�AHD낏D��HD�  D�AHD�HD�� D�  D�@ D�HD�� D�  D�AHD� D��HD�HD�@ D� D�� D�HD�AHD�~�D�D�HD�AHD� D�qD�  D�AHD�~�D�D�HD�>�D�HD��HD�HD�>�D�|)D���D��D�@ D�� D��HD�HD�AHD�~�D��qD�HD�@ D���D�� D��qD�=qD�� D��HD���D�>�D�~�D���D���D�AHD���G�O�?��?#�
?k�?��R?�p�?�(�?�@�@��@(��@=p�@J=q@Tz�@^�R@p��@��\@��@���@�
=@�p�@��@�\)@�Q�@�  @��
@˅@��@�(�@��
@�@�33@�Q�@�p�A�AffA
�HA�RA�\A�A�A=qA�RA#33A'
=A(��A+�A.�RA3�
A7�A:=qA<��A@  AA�AFffAJ�HAN�RAQG�AS�
AVffAZ=qA^{Ab�\Ae�Ag�Aj�HAmp�Aq�AuAx��A|��A\)A���A��\A��
A�{A�  A��A��A�z�A�A��RA���A��HA��A�z�A�p�A�\)A���A��A��HA��A�z�A�ffA�  A���A���A��\A��A�p�A�
=A��A�Q�A���A��HA�z�A�{A��RA��A�Q�A��A��A��A�{A�
=A�  A�G�A��\A���A�ffA�\)A�Q�A�G�A�33A��A�
=A�Q�A�G�Aʏ\A�(�A�{A�  A��A��
A�{A�Q�Aڏ\A���A޸RA���A��HA��A�A陚A�A�A�A�A��
A�A�  A��A�(�A�ffB (�BG�BffB�Bz�B��B�RB�
B	�B	�B
=B  B�B{B33BQ�Bp�B�\B�B��B��B�RB�
B��B�B
=B(�BG�B=qB\)B z�B!��B"�RB#�
B$��B&{B'33B(Q�B)p�B*�\B+�
B-�B.=qB/\)B0z�B1��B2�RB3�B4��B5�B7
=B8(�B9G�B:ffB;\)B<z�B=��B>�RB?�
B@��BB=qBC�BE�BFffBG�BH��BI�BK
=BL  BM�BNffBO�
BP��BR=qBS�BT��BV=qBW\)BX��BYB[
=B\(�B]p�B^�HB`Q�BaBc33Bdz�Bf{Bg�Bh��Bj=qBk�Bl��Bn{Bo\)Bp��Br=qBs�Bu�Bv�\Bx  ByB{
=B|Q�B}��B~�HB�{B��RB�p�B�(�B���B��B�z�B�33B��B��\B�33B��B��\B�33B�B�z�B�\)B�  B���B��B�(�B��RB�G�B��
B�Q�B���B�\)B��B�z�B���B��B�{B��\B��B���B�  B�z�B���B�p�B��
B�=qB���B�p�B�  B��\B�
=B�p�B��B�Q�B��RB�33B�B�Q�B���B�p�B�  B�ffB���B�G�B��B�=qB��RB�G�B��
B�ffB��HB�G�B��B�{B�z�B�
=B���B�  B���B�33B���B�  B�ffB���B�G�B��
B�ffB���B�p�B��
B�=qB���B�
=B���B�{B���B�G�B�B�(�B���B���B�p�B��B�z�B�
=B��B�  B�Q�B��RB�33B��B�(�B��RB�G�B�B�(�B���B�
=B�\)B�B�Q�B��HB�p�B��B�=qB���B���B�p�B�  B��\B��B��B��
B�(�B\B��BîB�=qB���B�33BŅB��
B�=qB���B�G�B�B�Q�B���B�33BɅB��B�Q�B���B�G�B��
B�ffB���B��BͅB��
B�Q�B���B�\)B��B�ffB���B�33BхB��
B�Q�B��HB�\)B��B�ffBԸRB��B�p�B��B�z�B��HBׅB��B�Q�Bأ�B���B�p�B�  Bڏ\B���B�G�BۮB�  B�z�B���B݅B�{B�z�B���B��B߅B��B�ffB��HB�p�B��B�Q�B��B���B�G�B�B�Q�B���B�\)B�B�{B�Q�B���B�\)B�B�Q�B���B�
=B�p�B�B�=qB��B�33B�B�  B�ffB��B�
=B홚B�{B�\B�
=B�\)BB�  B�ffB���B�B�  B�Q�B��B���B�B�{B���B�
=B�\)B�B�{B���B�33B�B�(�B�z�B���B�\)B��
B�ffB��HB�G�B��B�  B�Q�B��HB�\)B��
B�z�B��HB�G�B���B��C =qC z�C ��C  C(�CQ�C�C�
C
=CQ�C��C�RC�C{CQ�C��C�HC(�C\)C�C�RC��C33C�CC��C�CG�C��C�
C(�C\)C�C�RC�C(�Cp�C�RC�C	�C	G�C	z�C	C

=C
Q�C
�\C
�RC
�C{CQ�C��C�HC�CQ�C�C��C�C(�Cp�C�RC�HC
=C=qC�\C��C{C=qCp�C��C�
C(�CffC��C�
C
=C33CffC�C  C33CffC�\CC
=C\)C��C��C��C(�C\)C��C�C33CffC��CC  CG�C�\C��C  C(�C\)C�C�C=qCffC��C��C
=C\)C��C��C��C33Cz�CC
=C33CffC�\C�
C�CffC�\CC��C33Cp�CC�C�CG�C�\C�
C�CQ�Cz�C�C   C G�C z�C �C �
C!(�C!p�C!�C!�HC"
=C"Q�C"��C"�C#{C#=qC#�C#��C$�C$Q�C$�C$�C$�C%=qC%z�C%C%��C&�C&Q�C&��C&�C'33C'\)C'�\C'C(
=C(Q�C(��C(�
C)
=C)=qC)z�C)C*
=C*Q�C*z�C*�RC*�C+33C+�C+��C+��C,(�C,\)C,�C,��C-=qC-p�C-��C-�
C.(�C.p�C.�RC.�C/�C/Q�C/��C/�C0=qC0p�C0��C0��C1{C1ffC1�RC1�C2�C2Q�C2��C2��C3=qC3ffC3��C3�C433C4�\C4C4��C5(�C5p�C5��C6{C6Q�C6�C6�RC7
=C7Q�C7�C7�
C8  C8Q�C8��C8�C9(�C9Q�C9��C9�C:=qC:p�C:��C:�C;33C;�\C;C;��C<G�C<�\C<�HC=�C=G�C=��C=�C>=qC>p�C>��C>�C?=qC?��C?�
C@
=C@G�C@��C@�CA=qCAp�CA��CA��CBQ�CB��CB�
CC{CCG�CC��CC��CD=qCD�CD�RCD��CEG�CE��CE��CF(�CFffCF��CF��CGQ�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�@�
G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�AׁA�z�A�~�AׅAׅA׃AׁA�v�A�^5A�G�A�$�A���A֣�A�ĜA�I�A�A�JAŝ�A�G�A�r�A��uA�z�A�n�A��^A��A��hA�9XA��TA�t�A��`A�1A���A�|�A�v�A�z�A��A��mA�oA���A�(�A�dZA�VA��/A��/A�1A�S�A��#A�JA��`A��A��^A�z�A���A���A�?}A��A�7LA�x�A�33A��#A�t�A���A��A��mA�G�A�VA�E�A�wA~�jA~(�A}l�A|Q�A{"�Az�RAy��Ax�Aw33Av=qAuC�At��AtQ�As�ArffAq�^Ao�hAn�AmC�AlȴAl9XAk
=Aj-AiC�AhĜAf�Ab��Aa��Aa�Aa%A`�jA`1'A_�hA_XA_�A_�A^��A^�A]�A\��A[ƨA[O�AZ�AYAW�;AW/AVr�AVAU��AUG�AT�ATQ�ASx�AR�AQ�AP�AP(�AO�AO�hAN�AN  AL��AL�AK\)AJ�/AJ��AJQ�AI\)AH��AHz�AH$�AG��AG�AE��AE�AE%AD�jADVAC/AAO�A?�-A>�A>�A=�;A=;dA;�FA:�\A:1A9��A9l�A9oA8��A8v�A8=qA7��A7�wA7�7A7G�A6ffA5S�A3��A3"�A2ffA1��A0�HA/��A.ȴA.v�A.$�A-�A-��A,�A,�jA,-A+�7A*�jA*{A)hsA(Q�A({A'�;A&��A%�A%VA$�A#�-A#C�A"v�A"(�A!��A!��A!`BA!7LA ��A E�A��Ax�A��A  A��At�AK�A��A&�AM�A �A�TAdZA+A��AA�-A��Az�A�A��A��A�A�A��AS�A�A�FAȴA�A"�A�!AbA+A
ffA	�FA�HA5?A��A%Az�A�/A�hA^5AJA�A�A ��@��m@��+@�?}@�  @��!@���@��@�~�@�{@�`B@�9X@�@�@�ȴ@���@��@� �@�J@��`@㕁@�J@�
=@�bN@٩�@أ�@ץ�@�~�@ԛ�@�J@�A�@��@͑h@�O�@��@�-@ɉ7@��@�z�@�+@��@���@Ų-@���@��@���@��@��@��;@���@��h@��/@�9X@��@��@�|�@�t�@�|�@�dZ@�+@�n�@�%@� �@��@���@�9X@�t�@��y@�@��@�9X@���@�v�@��@���@�O�@��9@���@��9@�I�@�S�@�ȴ@���@�M�@��#@�%@��u@�I�@���@�t�@�@�M�@�`B@�A�@���@�&�@��@�j@��m@�ȴ@�^5@�E�@���@�7L@��@��@���@�^5@��^@�Q�@�dZ@��@�^5@��T@�G�@��D@�(�@�1@���@�dZ@�o@���@�E�@���@�|�@��w@�S�@�x�@��u@�1'@�  @�ƨ@��P@��@�@��@�/@���@��@�(�@�K�@��\@�n�@�=q@�J@�J@���@���@��@��@�A�@��m@�K�@�"�@��+@�V@��@���@���@��@�?}@��@�1@���@���@�dZ@�+@��\@��#@�hs@�%@��9@���@�j@��@�w@~��@~5?@}�T@}`B@}V@|�@|��@|�D@|Z@|Z@|(�@{��@{��@{�
@{��@z�@z^5@z=q@z�@y��@y��@y��@y7L@xr�@w��@w|�@w;d@v��@v�+@vE�@u�T@u��@u�@u/@sƨ@s"�@s@s@s@r�@r�H@r��@r��@rM�@q�#@qX@p��@pb@o��@o;d@n5?@m�-@m`B@m�@l��@l�@l��@lj@l1@kƨ@k�F@k��@kdZ@kC�@k"�@j��@jn�@j=q@i��@g��@g+@f��@f�y@f�@f�R@f��@f�+@f�+@fv�@fff@fff@fE�@f5?@f$�@f{@f@e�T@e�T@e��@e@e�-@e�-@e��@eO�@d�@c��@b^5@bM�@bM�@bM�@b=q@bJ@ahs@a%@`Ĝ@`�@`r�@`bN@`bN@`Q�@` �@_�;@_�P@_K�@^�@^ff@^$�@^@]�@]�@]��@\�j@[33@Z�@Z��@Z��@Z~�@Z^5@Z�@Y��@Y�#@Y�7@YG�@Y%@X��@X�u@XbN@W�w@W;d@W
=@V�@V�+@V@U�-@UO�@T�@T��@T(�@St�@R��@R~�@RM�@R�@Q�^@Qx�@QG�@Q&�@P�`@P�@O�P@N�y@N5?@M?}@L��@L�D@Lj@LI�@LI�@L9X@L9X@L9X@L(�@L(�@L�@L1@K�m@K�
@KdZ@KdZ@KdZ@KdZ@KS�@K"�@Ko@J�@J��@J�\@I�#@IX@I&�@H��@H��@Hr�@HQ�@H �@Hb@G�@G�w@G|�@F�@E�T@E/@D��@D�j@D�D@Dz�@Dj@Dj@DI�@D9X@D�@D1@Cƨ@B�H@AX@@�`@@�u@@r�@?�;@?�w@?�@?�@?��@?�P@?+@?
=@>ff@>$�@>@=�@=�T@=�T@=��@=@=�-@=��@=�h@=�@=O�@<��@<�/@<j@<z�@<�D@<z�@;ƨ@;�F@;�F@;��@;�F@;�F@;�F@;�@;"�@;o@;o@:�H@:�\@:J@9x�@9�@8��@8�9@8�u@8�u@8Q�@7|�@7K�@7;d@7�@7
=@6�y@6�@6ȴ@6��@6��@6�+@6v�@6ff@6V@6V@6E�@6E�@65?@65?@65?@65?@65?@65?@65?@6$�@6$�@5�@5�@5�@4z�@4(�@3@0�`@0�@0A�@0b@0  @/�@/��@/�w@/�w@/�P@/|�@/+@.�@.v�@-�h@-p�@-?}@,�@,��@,Z@,9X@,9X@,9X@,(�@+�@+"�@+@*�@*�@*��@*�\@*~�@*J@)��@)�@(�9@(��@(�@(bN@( �@'�@';d@&�@&�+@&$�@%�T@%�@%�@%`B@%V@$�j@$�D@#�m@"^5@ bN@ 1'@ 1'@ 1'@   @��@��@l�@\)@K�@;d@+@�y@��@E�@E�@$�@{@{@@�@��@�-@�h@O�@V@�j@��@��@�D@I�@ƨ@��@dZ@o@�@M�@�#@��@%@��@�@Q�@1'@1'@ �@ �@b@b@b@�;@��@�w@��@�P@\)@\)@\)@+@�@�@��@��@��@��@��@��@�y@�y@ȴ@ȴ@��@ff@�-@O�@?}@/@/@/@�@V@�@�@��@z�@�
@��@dZ@�@��@��@��@�!@~�@=q@J@��@�@��@�^@��@x�@G�@��@Q�@1'@ �@  @��@l�@l�@\)@;d@�@�R@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@�R@��@�+@V@@�@�T@�T@�T@��@O�@��@�/@��@��@j@j@I�@I�@1@�m@ƨ@�@dZ@S�@33@"�@
�H@
��@
�!@
�\@
~�@
n�@
M�@
J@	�7@	x�@	X@	7L@	�@��@Ĝ@�u@�u@r�@r�@r�@r�@ �@�P@l�@+@�@��@v�@V@V@E�@$�@{@�@��@��@`B@/@�@�@��@�/@�j@��@z�@9X@1@�
@��@dZ@@�!@��@~�@M�@J@�@��@�7@G�@G�@G�@7L@�@�@%@%@ ��@ ��@ �`@ �`@ Ĝ@ r�@ bN@ bN@ A�@ 1'@ 1'@  �@   ?��;?��wA�dZA�dZA�|�AׁAׅAׇ+AׁA�|�A�~�A�x�A�x�A׃A�~�A�x�A�v�A׃A�~�Aׇ+AׅA׃AׁA׃AׅAׇ+Aׇ+Aׇ+AׅA׃A׃A׃AׅA׃AׅA�|�A�|�A�|�A�|�A�t�A�t�A�l�A�hsA�ffA�\)A�G�A�M�A�I�A�?}A�A�A�=qA�(�A��A��A��A��A��A�"�A��A�oA��A�JA��A��A��yA��;A���A�ƨA�A���Aֺ^AֶFAֲ-A֛�A֛�A֓uA�t�A�p�A�`BA�S�A�O�A�A�A�5?A�+A�oA��
A�t�Aԗ�A�-A���A��#A���AӸRAӗ�A�z�A�XA�(�A��A�1A��A��
AҰ!A�p�A�`BA�\)A�ZA�\)A�Q�A��A���A��A��;A���AѶFAѬAэPA��Aк^A���AϮA�E�A�/A�I�A��TA�x�A��A�|�A�ƨAȥ�AǓuA��TA��
A��
A�+A�VA��
A�-A��TA�\)A�%A��`A�A�A�-A���A���A��^A��A�A��RA���A�ƨA���A��A��A��
A��hA�jA�7LA���A��+A��A���A�hsA��TA�v�A�1'A��
A���A���A���A��+A�M�A��A��A���A�;dA���A�jA�A�A��A���A���A�A�jA���A��A�JA���A� �A��7A�?}A��A�ĜA�1'A�I�A�A��#A�ȴA���A�VA���A��RA�Q�A���A�ZA��9A��-A�9XA��9A���A�"�A�n�A��DA��A��;A��jA��uA�hsA�7LA�A�ȴA��DA�ffA�I�A�=qA�-A�A��mA��A���A���A�bNA�I�A���A�t�A�$�A��A���A�^5A�ZA�S�A�33A���A��A�~�A�1'A�&�A�{A�A��mA���A�;dA��A���A��-A��uA��7A�v�A�S�A�=qA�"�A��A���A��A���A�~�A�Q�A� �A��A��
A�bNA�A��TA��;A��A�1A�A��A�/A�1'A�1'A�(�A��A���A�r�A�C�A�/A�&�A�&�A�$�A�"�A� �A��A�bA�
=A�A���A��mA��wA�v�A�$�A���A�l�A�(�A��A�oA�JA�  A��A��HA���A�A��FA���A�n�A�M�A�1'A�bA��A���A��-A���A���A��+A�~�A�hsA�5?A��A��A�oA�oA�VA�A���A��mA��A��FA��A�hsA�S�A�/A�bA�%A���A���A��A��;A��
A���A���A���A��#A��`A���A�
=A�VA�%A�  A���A���A��A��mA��HA��#A���A���A�ȴA��RA��-A��!A���A���A���A���A��hA��7A��A�x�A�jA�^5A�O�A�=qA�(�A��A�bA���A��yA��A�ȴA��jA���A�x�A�G�A���A���A�S�A�?}A�9XA�7LA�9XA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�;dA�1'A��A�%A��A��HA��A���A��9A���A���A���A��7A�t�A�VA��A��RA��!A���A���A���A��uA��A�t�A�`BA�ZA�XA�O�A�E�A�C�A�?}A�;dA�9XA�5?A�(�A��A�VA�A���A��yA��TA���A���A�ȴA�ĜA��A���A��\A��PA��DA��A�r�A�jA�\)A� �A���A��A��mA��mA���A��^A�v�A�33A���A��#A���A��wA��A���A�x�A�^5A�O�A�9XA�"�A�oA�
=A�%A���A��A��;A���A��wA��RA���A�z�A�x�A�^5A�XA�M�A�A�A��A�VA��A���A��RA��\A�x�A�Q�A�33A��A���A��
A��PA�^5A�K�A�C�A�;dA�1'A�&�A��A��A��A�bA�
=A�A�TA��Al�AK�A33A�AoA%A~�A~��A~��A~ffA~I�A~E�A~E�A~A�A~A�A~E�A~I�A~bA}�mA~1A~  A~  A}�#A}��A}��A}XA}"�A|�A|�`A|�/A|ȴA|�!A|�uA|r�A|M�A|$�A{�A{ƨA{��A{l�A{O�A{7LA{�Az��Az�yAz�HAz�Az��Az��Az��Az��AzȴAz�jAz��Az�\Azn�AzE�Az1'Az�AzbAy�Ay��Ay��Ay&�Ax��Ax�9Ax��AxE�Ax$�Ax1Aw�Aw��Aw�Awl�Aw\)AwS�AwK�AwK�AwC�Aw&�Av��Av��Av�RAv�\AvjAvZAv=qAv �Av{Au��Au�Au�
Au��Au��Aul�Au?}Au�At��At�`At�At��At�RAt�At��At��At�uAt�\At�+At~�Atr�AtjAtbNAt^5AtZAtQ�At5?As�TAsAs�7As?}As
=Ar�Ar�/ArȴAr�9Ar��Ar��Ar�\Ar�Arn�ArZArE�Ar=qAr1'Ar(�Ar �Ar{Aq��Aq�
Aq��Aq�Aqt�Aq`BAp��Ap  Ao�wAo�Ao|�AodZAo;dAo33Ao&�Ao%An��An��An�HAn��An��AnI�An�Am�Am��Am�AmhsAmS�Am/Al�Al��Al��Al��Al��Al��Al��Al��AlĜAl��Al�Al��Alr�AlZAlQ�AlE�Al(�AlAk��Ak��Ak\)AkK�Ak&�AkAj��AjĜAj�RAj�!Aj��Aj�DAjVAj9XAj �Aj  Ai�
AiAi��Ai|�AiK�Ai33Ai+Ai&�Ai"�AioAiVAh��Ah��Ah�/Ah��Ah��Ahn�AhM�Ah$�Ag��AgS�Ag%Af��Af1Ad��Adr�AdI�Ac��Ac%AbĜAbv�AbA�Ab �Ab�Ab1Aa�;Aa�wAa�hAaXAa\)AaO�Aa;dAa33Aa&�Aa�AaoAaVAa%Aa%Aa%Aa
=AaVAaVAa
=Aa%A`��A`�yA`�HA`��A`ĜA`�A`�!A`�9A`��A`��A`��A`n�A`A�A` �A_�A_ƨA_��A_�^A_��A_�hA_�A_�A_|�A_x�A_p�A_hsA_dZA_`BA_\)A_K�A_;dA_33A_+A_&�A_"�A_�A_VA_VA_oA_oA_�A_�A_�A_�A_oA_oA_VA_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          A�z�AׁA�z�A�~�AׅAׅA׃AׁA�v�A�^5A�G�A�$�A���A֣�A�ĜA�I�A�A�JAŝ�A�G�A�r�A��uA�z�A�n�A��^A��A��hA�9XA��TA�t�A��`A�1A���A�|�A�v�A�z�A��A��mA�oA���A�(�A�dZA�VA��/A��/A�1A�S�A��#A�JA��`A��A��^A�z�A���A���A�?}A��A�7LA�x�A�33A��#A�t�A���A��A��mA�G�A�VA�E�A�wA~�jA~(�A}l�A|Q�A{"�Az�RAy��Ax�Aw33Av=qAuC�At��AtQ�As�ArffAq�^Ao�hAn�AmC�AlȴAl9XAk
=Aj-AiC�AhĜAf�Ab��Aa��Aa�Aa%A`�jA`1'A_�hA_XA_�A_�A^��A^�A]�A\��A[ƨA[O�AZ�AYAW�;AW/AVr�AVAU��AUG�AT�ATQ�ASx�AR�AQ�AP�AP(�AO�AO�hAN�AN  AL��AL�AK\)AJ�/AJ��AJQ�AI\)AH��AHz�AH$�AG��AG�AE��AE�AE%AD�jADVAC/AAO�A?�-A>�A>�A=�;A=;dA;�FA:�\A:1A9��A9l�A9oA8��A8v�A8=qA7��A7�wA7�7A7G�A6ffA5S�A3��A3"�A2ffA1��A0�HA/��A.ȴA.v�A.$�A-�A-��A,�A,�jA,-A+�7A*�jA*{A)hsA(Q�A({A'�;A&��A%�A%VA$�A#�-A#C�A"v�A"(�A!��A!��A!`BA!7LA ��A E�A��Ax�A��A  A��At�AK�A��A&�AM�A �A�TAdZA+A��AA�-A��Az�A�A��A��A�A�A��AS�A�A�FAȴA�A"�A�!AbA+A
ffA	�FA�HA5?A��A%Az�A�/A�hA^5AJA�A�A ��@��m@��+@�?}@�  @��!@���@��@�~�@�{@�`B@�9X@�@�@�ȴ@���@��@� �@�J@��`@㕁@�J@�
=@�bN@٩�@أ�@ץ�@�~�@ԛ�@�J@�A�@��@͑h@�O�@��@�-@ɉ7@��@�z�@�+@��@���@Ų-@���@��@���@��@��@��;@���@��h@��/@�9X@��@��@�|�@�t�@�|�@�dZ@�+@�n�@�%@� �@��@���@�9X@�t�@��y@�@��@�9X@���@�v�@��@���@�O�@��9@���@��9@�I�@�S�@�ȴ@���@�M�@��#@�%@��u@�I�@���@�t�@�@�M�@�`B@�A�@���@�&�@��@�j@��m@�ȴ@�^5@�E�@���@�7L@��@��@���@�^5@��^@�Q�@�dZ@��@�^5@��T@�G�@��D@�(�@�1@���@�dZ@�o@���@�E�@���@�|�@��w@�S�@�x�@��u@�1'@�  @�ƨ@��P@��@�@��@�/@���@��@�(�@�K�@��\@�n�@�=q@�J@�J@���@���@��@��@�A�@��m@�K�@�"�@��+@�V@��@���@���@��@�?}@��@�1@���@���@�dZ@�+@��\@��#@�hs@�%@��9@���@�j@��@�w@~��@~5?@}�T@}`B@}V@|�@|��@|�D@|Z@|Z@|(�@{��@{��@{�
@{��@z�@z^5@z=q@z�@y��@y��@y��@y7L@xr�@w��@w|�@w;d@v��@v�+@vE�@u�T@u��@u�@u/@sƨ@s"�@s@s@s@r�@r�H@r��@r��@rM�@q�#@qX@p��@pb@o��@o;d@n5?@m�-@m`B@m�@l��@l�@l��@lj@l1@kƨ@k�F@k��@kdZ@kC�@k"�@j��@jn�@j=q@i��@g��@g+@f��@f�y@f�@f�R@f��@f�+@f�+@fv�@fff@fff@fE�@f5?@f$�@f{@f@e�T@e�T@e��@e@e�-@e�-@e��@eO�@d�@c��@b^5@bM�@bM�@bM�@b=q@bJ@ahs@a%@`Ĝ@`�@`r�@`bN@`bN@`Q�@` �@_�;@_�P@_K�@^�@^ff@^$�@^@]�@]�@]��@\�j@[33@Z�@Z��@Z��@Z~�@Z^5@Z�@Y��@Y�#@Y�7@YG�@Y%@X��@X�u@XbN@W�w@W;d@W
=@V�@V�+@V@U�-@UO�@T�@T��@T(�@St�@R��@R~�@RM�@R�@Q�^@Qx�@QG�@Q&�@P�`@P�@O�P@N�y@N5?@M?}@L��@L�D@Lj@LI�@LI�@L9X@L9X@L9X@L(�@L(�@L�@L1@K�m@K�
@KdZ@KdZ@KdZ@KdZ@KS�@K"�@Ko@J�@J��@J�\@I�#@IX@I&�@H��@H��@Hr�@HQ�@H �@Hb@G�@G�w@G|�@F�@E�T@E/@D��@D�j@D�D@Dz�@Dj@Dj@DI�@D9X@D�@D1@Cƨ@B�H@AX@@�`@@�u@@r�@?�;@?�w@?�@?�@?��@?�P@?+@?
=@>ff@>$�@>@=�@=�T@=�T@=��@=@=�-@=��@=�h@=�@=O�@<��@<�/@<j@<z�@<�D@<z�@;ƨ@;�F@;�F@;��@;�F@;�F@;�F@;�@;"�@;o@;o@:�H@:�\@:J@9x�@9�@8��@8�9@8�u@8�u@8Q�@7|�@7K�@7;d@7�@7
=@6�y@6�@6ȴ@6��@6��@6�+@6v�@6ff@6V@6V@6E�@6E�@65?@65?@65?@65?@65?@65?@65?@6$�@6$�@5�@5�@5�@4z�@4(�@3@0�`@0�@0A�@0b@0  @/�@/��@/�w@/�w@/�P@/|�@/+@.�@.v�@-�h@-p�@-?}@,�@,��@,Z@,9X@,9X@,9X@,(�@+�@+"�@+@*�@*�@*��@*�\@*~�@*J@)��@)�@(�9@(��@(�@(bN@( �@'�@';d@&�@&�+@&$�@%�T@%�@%�@%`B@%V@$�j@$�D@#�m@"^5@ bN@ 1'@ 1'@ 1'@   @��@��@l�@\)@K�@;d@+@�y@��@E�@E�@$�@{@{@@�@��@�-@�h@O�@V@�j@��@��@�D@I�@ƨ@��@dZ@o@�@M�@�#@��@%@��@�@Q�@1'@1'@ �@ �@b@b@b@�;@��@�w@��@�P@\)@\)@\)@+@�@�@��@��@��@��@��@��@�y@�y@ȴ@ȴ@��@ff@�-@O�@?}@/@/@/@�@V@�@�@��@z�@�
@��@dZ@�@��@��@��@�!@~�@=q@J@��@�@��@�^@��@x�@G�@��@Q�@1'@ �@  @��@l�@l�@\)@;d@�@�R@ȴ@ȴ@ȴ@ȴ@ȴ@ȴ@�R@��@�+@V@@�@�T@�T@�T@��@O�@��@�/@��@��@j@j@I�@I�@1@�m@ƨ@�@dZ@S�@33@"�@
�H@
��@
�!@
�\@
~�@
n�@
M�@
J@	�7@	x�@	X@	7L@	�@��@Ĝ@�u@�u@r�@r�@r�@r�@ �@�P@l�@+@�@��@v�@V@V@E�@$�@{@�@��@��@`B@/@�@�@��@�/@�j@��@z�@9X@1@�
@��@dZ@@�!@��@~�@M�@J@�@��@�7@G�@G�@G�@7L@�@�@%@%@ ��@ ��@ �`@ �`@ Ĝ@ r�@ bN@ bN@ A�@ 1'@ 1'@  �@   ?��;G�O�A�dZA�dZA�|�AׁAׅAׇ+AׁA�|�A�~�A�x�A�x�A׃A�~�A�x�A�v�A׃A�~�Aׇ+AׅA׃AׁA׃AׅAׇ+Aׇ+Aׇ+AׅA׃A׃A׃AׅA׃AׅA�|�A�|�A�|�A�|�A�t�A�t�A�l�A�hsA�ffA�\)A�G�A�M�A�I�A�?}A�A�A�=qA�(�A��A��A��A��A��A�"�A��A�oA��A�JA��A��A��yA��;A���A�ƨA�A���Aֺ^AֶFAֲ-A֛�A֛�A֓uA�t�A�p�A�`BA�S�A�O�A�A�A�5?A�+A�oA��
A�t�Aԗ�A�-A���A��#A���AӸRAӗ�A�z�A�XA�(�A��A�1A��A��
AҰ!A�p�A�`BA�\)A�ZA�\)A�Q�A��A���A��A��;A���AѶFAѬAэPA��Aк^A���AϮA�E�A�/A�I�A��TA�x�A��A�|�A�ƨAȥ�AǓuA��TA��
A��
A�+A�VA��
A�-A��TA�\)A�%A��`A�A�A�-A���A���A��^A��A�A��RA���A�ƨA���A��A��A��
A��hA�jA�7LA���A��+A��A���A�hsA��TA�v�A�1'A��
A���A���A���A��+A�M�A��A��A���A�;dA���A�jA�A�A��A���A���A�A�jA���A��A�JA���A� �A��7A�?}A��A�ĜA�1'A�I�A�A��#A�ȴA���A�VA���A��RA�Q�A���A�ZA��9A��-A�9XA��9A���A�"�A�n�A��DA��A��;A��jA��uA�hsA�7LA�A�ȴA��DA�ffA�I�A�=qA�-A�A��mA��A���A���A�bNA�I�A���A�t�A�$�A��A���A�^5A�ZA�S�A�33A���A��A�~�A�1'A�&�A�{A�A��mA���A�;dA��A���A��-A��uA��7A�v�A�S�A�=qA�"�A��A���A��A���A�~�A�Q�A� �A��A��
A�bNA�A��TA��;A��A�1A�A��A�/A�1'A�1'A�(�A��A���A�r�A�C�A�/A�&�A�&�A�$�A�"�A� �A��A�bA�
=A�A���A��mA��wA�v�A�$�A���A�l�A�(�A��A�oA�JA�  A��A��HA���A�A��FA���A�n�A�M�A�1'A�bA��A���A��-A���A���A��+A�~�A�hsA�5?A��A��A�oA�oA�VA�A���A��mA��A��FA��A�hsA�S�A�/A�bA�%A���A���A��A��;A��
A���A���A���A��#A��`A���A�
=A�VA�%A�  A���A���A��A��mA��HA��#A���A���A�ȴA��RA��-A��!A���A���A���A���A��hA��7A��A�x�A�jA�^5A�O�A�=qA�(�A��A�bA���A��yA��A�ȴA��jA���A�x�A�G�A���A���A�S�A�?}A�9XA�7LA�9XA�=qA�?}A�?}A�?}A�?}A�A�A�A�A�;dA�1'A��A�%A��A��HA��A���A��9A���A���A���A��7A�t�A�VA��A��RA��!A���A���A���A��uA��A�t�A�`BA�ZA�XA�O�A�E�A�C�A�?}A�;dA�9XA�5?A�(�A��A�VA�A���A��yA��TA���A���A�ȴA�ĜA��A���A��\A��PA��DA��A�r�A�jA�\)A� �A���A��A��mA��mA���A��^A�v�A�33A���A��#A���A��wA��A���A�x�A�^5A�O�A�9XA�"�A�oA�
=A�%A���A��A��;A���A��wA��RA���A�z�A�x�A�^5A�XA�M�A�A�A��A�VA��A���A��RA��\A�x�A�Q�A�33A��A���A��
A��PA�^5A�K�A�C�A�;dA�1'A�&�A��A��A��A�bA�
=A�A�TA��Al�AK�A33A�AoA%A~�A~��A~��A~ffA~I�A~E�A~E�A~A�A~A�A~E�A~I�A~bA}�mA~1A~  A~  A}�#A}��A}��A}XA}"�A|�A|�`A|�/A|ȴA|�!A|�uA|r�A|M�A|$�A{�A{ƨA{��A{l�A{O�A{7LA{�Az��Az�yAz�HAz�Az��Az��Az��Az��AzȴAz�jAz��Az�\Azn�AzE�Az1'Az�AzbAy�Ay��Ay��Ay&�Ax��Ax�9Ax��AxE�Ax$�Ax1Aw�Aw��Aw�Awl�Aw\)AwS�AwK�AwK�AwC�Aw&�Av��Av��Av�RAv�\AvjAvZAv=qAv �Av{Au��Au�Au�
Au��Au��Aul�Au?}Au�At��At�`At�At��At�RAt�At��At��At�uAt�\At�+At~�Atr�AtjAtbNAt^5AtZAtQ�At5?As�TAsAs�7As?}As
=Ar�Ar�/ArȴAr�9Ar��Ar��Ar�\Ar�Arn�ArZArE�Ar=qAr1'Ar(�Ar �Ar{Aq��Aq�
Aq��Aq�Aqt�Aq`BAp��Ap  Ao�wAo�Ao|�AodZAo;dAo33Ao&�Ao%An��An��An�HAn��An��AnI�An�Am�Am��Am�AmhsAmS�Am/Al�Al��Al��Al��Al��Al��Al��Al��AlĜAl��Al�Al��Alr�AlZAlQ�AlE�Al(�AlAk��Ak��Ak\)AkK�Ak&�AkAj��AjĜAj�RAj�!Aj��Aj�DAjVAj9XAj �Aj  Ai�
AiAi��Ai|�AiK�Ai33Ai+Ai&�Ai"�AioAiVAh��Ah��Ah�/Ah��Ah��Ahn�AhM�Ah$�Ag��AgS�Ag%Af��Af1Ad��Adr�AdI�Ac��Ac%AbĜAbv�AbA�Ab �Ab�Ab1Aa�;Aa�wAa�hAaXAa\)AaO�Aa;dAa33Aa&�Aa�AaoAaVAa%Aa%Aa%Aa
=AaVAaVAa
=Aa%A`��A`�yA`�HA`��A`ĜA`�A`�!A`�9A`��A`��A`��A`n�A`A�A` �A_�A_ƨA_��A_�^A_��A_�hA_�A_�A_|�A_x�A_p�A_hsA_dZA_`BA_\)A_K�A_;dA_33A_+A_&�A_"�A_�A_VA_VA_oA_oA_�A_�A_�A_�A_oA_oA_VA_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BܒB�pB�dB�5B��B�;B�B�HB��B� B�B�B�B�B��B�&BɺB�nBR�BM6B��B��B�*B��B��B�QB��BI�B
�B
ŢB
��B
�aB
�mB
�BB
уB
ںB
�yB
�B
��B
�+B�B/OB2aB-�B�BB	lB%B�BuB*0B$�B!-B�B�B�B�B�B
��B
��B
�B
�yB
��B
�KB
��B
��B
��B
�bB
�+B
cB
zDB
y�B
p�B
f�B
aB
[�B
OB
FB
?B
8�B
1�B
/B
(�B
 �B
xB
�B
1B	�(B	�>B	��B	�B	�B	�fB	�B	�
B	��B	�nB	��B	�zB	�B	��B	�$B	�RB	�LB	�FB	��B	�nB	�hB	��B	��B	�FB	��B	��B	��B	��B	��B	�iB	~(B	|�B	{�B	x�B	t�B	r�B	poB	jB	f�B	d�B	b�B	^�B	V�B	R�B	M6B	H�B	D�B	E�B	G�B	H�B	D�B	C�B	B'B	@�B	<�B	:�B	6B	5?B	1�B	/�B	,�B	%B	!B	=B	�B	MB	hB	�B	MB	;B�.B��B��B��B��B��B�2B�ZB�B�GB��B��B�NB�;B�KB�
B� B�NB�KB�B�B�-B�aB��B�B�^B�*B�B�'B��B��B��B��B��B�~B�+B�B��B��B�B�lB�_B��B��B�SB�GB�BcB}�B}"ByrBx8Bv�Bu�Bt�Br|Bl�Bk�Bj�Bi�Bg�Bf�Bf�BbB`�Ba�B[�BZ�BW�BV�BW?BPBLdBJ�BG�BD�BB�B>wB?HB=<B=qB9�B8B7�B7B5?B2�B2�B/�B2�B0!B/�B/�B/�B-B-�B+�B)�B'�B$�B�B�B�B�B�B�B�B�BSB�BB!B�B�B+B�BB	7B
�B%B%BJB%B�B�B{B�B��BMB{B�BBBSB�BJB�B(BBhBFB�B�B�BuB�B.B�B�B�B�BYB$B�B�B�B.B�B.B(B�B4B�B�BFB�B�BeBxB"�B-CB0�B2-B6�B:*B:�B:*B:�B;0B<�B<6B;�B;�B<�B=�B?B@�BCaBG�BJ#BJ�BJ�BL0BPBQNBQ�BTaBTaBV9BZ�B[#B[�B]�BbNBc�Bf2Bk�Bn�BrGBu�Bv`Bw2BxBx8ByrBzxB{JB|PBy>B{B~�B��B��B��B��B�SB��B��B�PB��B�bB�MB��B��B��B�OB�!B�bB��B��B��B�B��B�kB��B��B��B��B��B��B�B��B�wB�B��BĜB�B�)B��B��B��B��B�B��BߤB�NB��B��B�B�DB��B�oB�|B��B��B��B��B�"B�(B�(B	 4B	�B	MB	
�B	xB	�B	SB	$B	�B	�B	_B	1B	kB	�B	"�B	$@B	%zB	&�B	(�B	*0B	,=B	,�B	-wB	.�B	6B	8B	8�B	8�B	8�B	8�B	9XB	:*B	:�B	<B	=B	?HB	AUB	D�B	E�B	H�B	M6B	OBB	P}B	Q�B	R B	RTB	R�B	T�B	V�B	W�B	XB	X�B	Y�B	ZQB	Z�B	[�B	]�B	^B	aB	j�B	l�B	m�B	m�B	n/B	n�B	o5B	o�B	o�B	p;B	poB	poB	qAB	qvB	q�B	rB	r|B	r�B	sMB	s�B	s�B	s�B	tB	tB	u�B	xlB	~]B	�{B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�JB	�JB	�~B	��B	��B	��B	�.B	�4B	�uB	�B	��B	�SB	�B	�SB	�B	��B	�hB	��B	�@B	�B	��B	�LB	��B	�$B	��B	��B	��B	��B	��B	�B	��B	�hB	�FB	�B	��B	�XB	�B	�qB	�B	��B	�[B	�3B	�zB	�#B	�^B	��B	��B	�B	��B	�BB	��B	бB	��B	��B	خB	��B	�B	�vB	�|B	�B	�B	��B	� B	��B	� B	�B	�B	�B	�B	��B	�B	��B	��B	��B	�B	��B	�B	��B	�8B	�mB	�sB	�B	�)B	�/B	��B	�B	�AB	�B	�B	�B	�MB	�B	�B	�+B	�rB	��B	�"B	�"B	��B	�(B	�]B	��B	�.B	�.B	�cB	�.B	��B
�B
�B
�B
�B
1B
	7B
	7B
	lB
	B
	lB
	7B
	�B
	�B

rB
B
DB
xB
xB
�B
�B
�B
�B
B
~B
~B
B
"B
VB
\B
�B
"B
�B
4B
�B
4B
:B
4B
�B
�B
:B
B
B
�B
�B
MB
�B
�B
_B
1B
�B
1B
�B
B
B
B
�B
CB
CB
CB
�B
�B
�B
B
~B
�B
�B
IB
~B
�B
~B
�B
IB
�B
B
~B
~B
~B
�B
�B
~B
OB
VB
 'B
�B
#:B
$�B
%FB
&B
%�B
&B
&B
&B
&B
%�B
&�B
&�B
'B
'�B
(�B
*�B
*�B
*�B
+B
,B
,�B
,B
,B
,=B
,qB
-�B
.IB
-�B
-wB
-wB
-�B
.IB
.�B
/�B
1'B
2-B
2aB
1�B
2�B
2�B
2�B
49B
4B
4�B
5B
6B
6zB
7LB
7LB
7B
7LB
7�B
8RB
8�B
<�B
?HB
@B
?�B
?�B
?�B
@�B
A B
@�B
A B
@�B
A�B
@�B
B'B
B�B
B�B
B�B
B�B
C�B
C-B
B�B
C-B
C�B
C�B
C�B
D�B
E9B
E�B
E�B
E9B
EB
FB
F�B
FtB
GB
GzB
G�B
IRB
I�B
J#B
J�B
J�B
K�B
K�B
L0B
LdB
K�B
K�B
L0B
LdB
L0B
K�B
K�B
L�B
L0B
LdB
L�B
L�B
L�B
MjB
M�B
MB
M�B
NB
M�B
M�B
M6B
MjB
NB
MjB
M�B
M�B
NpB
N�B
PB
P}B
P�B
QB
P�B
P�B
P�B
P�B
QB
P�B
QNB
Q�B
S&B
S�B
S�B
T�B
T�B
T�B
T,B
T�B
T�B
U2B
U�B
U�B
UgB
U�B
V9B
VmB
V�B
VmB
W?B
W�B
W�B
XyB
XEB
X�B
YKB
X�B
X�B
YB
Z�B
Z�B
Z�B
ZB
Y�B
Y�B
ZQB
Z�B
ZB
ZQB
Z�B
[�B
[�B
[�B
[�B
\)B
[�B
\]B
\�B
]dB
^B
]�B
^B
^jB
^B
^5B
^5B
^�B
^�B
_;B
_pB
_;B
_pB
_�B
_�B
`BB
_�B
`vB
`BB
`vB
`B
`�B
aHB
bB
a�B
a�B
b�B
bNB
b�B
c�B
c�B
cTB
cTB
c B
c B
c B
d&B
e,B
e`B
e�B
ffB
f�B
gB
f�B
ffB
ffB
f�B
gmB
g�B
g�B
gmB
g�B
h�B
h�B
h>B
hsB
iB
iyB
h�B
iDB
i�B
jB
jKB
j�B
kQB
k�B
l"B
lWB
lWB
l�B
m]B
m)B
m]B
m�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
o B
o5B
o�B
o�B
o�B
pB
pB
pB
p;B
poB
p�B
qB�)BچB�QB��BߤB�;B�B��B�;B�B�BچB�B�BߤB�jB�5B�dB�B��BޞB�pB�B�B�jB�vB�|B�NB�HB��B�B�vB�B�B�B�NB�B��B�|B�HB�B�NB��B�B��B�HB��B�B�B�ZB��B��B��B��B�&B�TB�&B� B��B�B�B�8B�`B��B�yB�B�WB�DB�B�B�B�B�B��B�oB�/B�5B��B�B��B�B�B�B��B��B�B��B�B��B��B�"B�sB�B��B�DB�WB�B�/B�B�/B�sB�B��B�B�B�0B��B�RB��B�?B��B��B��B�B�B�6B҉B��B��B�kB��B��B�B�VB��Bl�B�Bd�Bb�Bc�BL�B33BI�BfBH�B� B)�BlWBE�B*�B"�B�B��B�B�#B�B�B�hB�UB��B�sB��B�
B�B�
B��B�mB�fB�B��B�B��B�bB�uB��B�B�DB�zB�IB�'B�BB��B�B�-B�RB��B��B��B��B��B�:B��B��B�(BAUB��BуB��B�B��B�qB��B��Bd�B]�BQ�BXEBS[BC-B33B3�B,=B'RBB
��B
��B
˒B
�dB
�gB
�B
�<B
�'B
�LB
�B
�XB
��B
�dB
��B
��B
�B
�UB
��B
� B
��B
ĜB
�BB
B
̘B
�#B
�#B
��B
�fB
��B
�fB
�2B
��B
�)B
�B
��B
�3B
�zB
�EB
�TB
�B
ӏB
�mB
�mB
چB
�NB
�HB
��B
�,B
��B
�?B
��B
خB
�#B
ںB
�B
��B
�B
�ZB
��B
�,B
�yB
�B
��B
�cB�B
��B
�]B
�)B
�;B�B�B�B �B!bB!�B!�B#�B4nB/B0�B/OB0�B1�B1�B0�B1�B2aB1�B1'B2-B2aB2aB8�B7�B7�B2�B/�B#BqB7B�B7B�B$B�B�B�BMB BB�BDB
rB	lB�B	7BfB�BSBBbB	B�B�B�B�B�B%BSBMB+B�BuBYBJBB�B{BGB�B�B	B	B�B�B"B�B�B&LB+�B*�B+�B*�B*�B*eB(�B(�B(�B'�B($B'RB%FB"hB �B!�B!bB!bB"hB!�B#�B"�B"4BVB!B�B �B�B�B�BkB�BB:B BuBB�B�B�B�B�BBBB�B�BB�BB�B�BoB BoB@B�B"B�B�B�B"B
rB	�BB
�B(B�BGB
��B
��B
��B
�JB
�JB
��B
��B
�rB
��B
�lB
��B
��B
�%B
�%B
��B
�%B
��B
��B
��B
�B
�GB
�B
�B
�AB
��B
��B
�]B
��B
�oB
�"B
�QB
�B
�B
�B
�>B
�B
�B
�/B
�B
�dB
ݘB
��B
��B
�)B
�B
�B
ҽB
�B
�dB
�dB
��B
ȀB
��B
�-B
�-B
ÖB
��B
��B
��B
�0B
�B
��B
��B
�dB
�B
�FB
��B
��B
��B
�aB
�B
��B
��B
�'B
�B
��B
��B
��B
��B
��B
�nB
�7B
�B
��B
�B
�B
�B
�VB
��B
�VB
�~B
��B
�B
�lB
�fB
�7B
�fB
�fB
��B
��B
��B
��B
�B
��B
�B
�B
�B
cB
�oB
��B
|PB
zDB
zxB
{B
z�B
zDB
y>B
�B
v�B
~(B
v+B
y	B
�oB
y�B
z�B
}VB
zB
u�B
tTB
sMB
r�B
qAB
qvB
rB
qB
oiB
n�B
l�B
m]B
hsB
gmB
hsB
h>B
e�B
b�B
b�B
b�B
c B
c B
bB
a|B
`B
`�B
_�B
_�B
`�B
`B
\�B
[WB
Y�B
[�B
Z�B
\)B
cTB
W
B
P�B
Q�B
S�B
N�B
NpB
MB
PHB
JXB
J�B
H�B
F?B
E�B
E�B
D3B
G�B
CaB
EmB
CaB
EmB
A�B
>BB
>BB
=�B
=<B
=B
:�B
>�B
:�B
7�B
;�B
>BB
8�B
6�B
4nB
2�B
2�B
2�B
3�B
2�B
1�B
0UB
/�B
0UB
0�B
0UB
/�B
.}B
-B
-CB
-�B
0�B
/OB
,�B
.B
-CB
($B
&�B
%�B
$@B
#�B
"�B
"4B
"hB
#:B
!�B
 �B
�B
�B
�B
�B
~B
�B
IB
B
�B
eB
�B
�B
*0B
�B
FB
B
�B
	�B
�B
DB

=B
fB
�B
�B
�B
1B

�B
�B
�B
�B	��B	��B	��B
 4B	��B
�B	��B	�B	��B	��B	��B	��B	��B	�>B	��B	�DB	�lB	�DB	�8B	�2B	�`B	��B	�`B	�`B	�2B	�+B	�MB	�|B	�B	�B	� B	�B	�/B	�B	�B	��B	�B	�)B	�QB	��B	�DB	�sB	�KB	�sB	�
B	��B	�TB	�TB	��B	�B	��B	�vB	��B	�B	�HB	�B	�;B	ܒB	�B	�B	ٴB	��B	�)B	�QB	�^B	��B	ȀB	��B	�UB	��B	�B	�B	�tB	�tB	�zB	��B	�nB	�4B	��B	��B	��B	��B	�hB	�B	�B	��B	�hB	�4B	��B	��B	�FB	��B	�FB	��B	�B	��B	��B	�FB	��B	�LB	�B	�B	��B	��B	�FB	�B	��B	��B	��B	�XB	�B	��B	�*B	�_B	��B	��B	��B	��B	��B	�XB	�RB	��B	��B	��B	��B	�RB	�B	�LB	�LB	�LB	�LB	��B	�zB	�B	��B	�zB	��B	�B	��B	��B	��B	�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          BܯB�mB�6B�B��B�kB�HB��B�B�sB�eB�B�2B�BYB�B�B��B�tBv4B�[B�QB�BB��B�wB�CB��B]�B^B
�B
�vB
ʟB
� B
��B
��B
��B
��B
�tB
��B
��B �B2	B9�B3�BiB�B�B
�B�BEB+;B&�B$�B�BZBB,BB
��B
�HB
�B
��B
�B
̟B
�AB
�B
�sB
�?B
�bB
�eB
|�B
}SB
tzB
hQB
d,B
a6B
RJB
I6B
B.B
:�B
2�B
2�B
*�B
#�B
#kB
B
�B	��B	�]B	�\B	�B	�B	��B	�yB	�B	�pB	�MB	�3B	��B	��B	�xB	��B	�B	��B	��B	��B	��B	�5B	�B	�~B	��B	��B	��B	�cB	�2B	�WB	��B	hB	~CB	}�B	{jB	wB	v�B	s�B	k�B	g�B	fgB	edB	aHB	Z�B	U B	O�B	J�B	ESB	G�B	J�B	JwB	FB	D�B	C�B	B�B	@�B	<�B	7�B	6UB	3oB	4B	3B	*iB	!�B	B	�B	�B	�B	�B	DB	�B	 B��B��B� B��B��B�
B�/B�:B��B��B�B��B��B�<BٟB��B԰B�xB�BB��B�eBŲB��B�
B��B�B�sB��B�"B��B�	B�fB��B��B�EB��B��B��B�PB��B�<B�kB�|B��B�>B��B��B��BBBz�Bx�Bw�BxBzSBuuBm�Bl�Bl�Bj�Bh�Bj
Bh Bb�Bd�Be=B]�B[3BX�B[B\WBR�BO�BM�BK?BH�BD�B@:BA�B@�B@WB<KB;B:5B9IB7�B5BB8{B4�B6�B1�B1�B1SB1LB/�B0.B.EB,KB*�B*CB&B �B_B!_B �B!DB%�B�B�BUB!�B"�B�BBDBxB�B�B�B+B�B�B
�B
BKB�B\BjBeB�B�BQB|BxB_BjB�B�B�BkBkBB�BoB�B�BIB?B�BBB�B�BUBrBMB�B*B�B�BBB�BB�B�BBBLB$B-iB1B3&B8�B;7B;B:�B;�B<�B=�B<�B<�B<�B=�B?TBABCDBFyBJ�BK BKfBK�BNABP�BQ�BSBUGBU�BYJB[pB[�B]\B`SBdBd�Bg�Bl{Bo�Bs�Bv�Bv�BxBx�Bx�BzhB{_B~ B~�By"B|1B�cB��B��B�'B�<B��B��B�aB�SB�pB��B��B��B��B�<B��B��B��B��B�B�B��B�+B��B�|B��B�B��B�`B��B�RB�
B��B��B��B��BɡBˋB̉B�hB�%B�gB�B݉B�EB�B�`B�rB�'B�B�B��B�B�OB��B�B�0B�YB�9B�fB	 oB	�B	�B	&B	FB	_B	wB	QB	�B	5B	�B	�B	3B	�B	"�B	$�B	%�B	'_B	)EB	*�B	,�B	-B	-�B	0,B	6�B	8GB	8�B	8�B	8�B	9B	9�B	:[B	:�B	<|B	=�B	?�B	BB	E B	FEB	I�B	M�B	O�B	P�B	Q�B	R=B	R�B	SaB	U.B	V�B	W�B	X@B	YB	Y�B	Z�B	[KB	\\B	]�B	^�B	b�B	kB	l�B	m�B	m�B	nXB	n�B	o]B	o�B	o�B	pSB	p|B	p�B	q\B	q�B	q�B	r/B	r�B	r�B	shB	s�B	s�B	s�B	t>B	t�B	v~B	y�B	rB	��B	��B	��B	�B	�]B	��B	�eB	��B	� B	��B	�aB	�WB	��B	��B	��B	�B	�zB	��B	��B	�aB	�B	�iB	�-B	��B	�PB	�OB	��B	��B	�iB	�;B	��B	��B	��B	�RB	�B	�IB	��B	�B	�B	�^B	��B	��B	�}B	�SB	�HB	��B	�XB	��B	�}B	�LB	��B	��B	�B	�wB	˗B	�6B	�3B	�NB	�B	�mB	�.B	�'B	��B	�}B	�qB	ܭB	�wB	�B	�B	�>B	��B	�B	�&B	��B	�6B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�3B	�B	��B	�cB	��B	�*B	�B	�cB	�B	�B	��B	�hB	��B	��B	��B	�B	��B	��B	�'B	�B	�B	�9B	�XB	�	B	�>B	�gB	��B	�DB	�TB	��B	��B
 �B
NB
�B
B
�B
�B
	\B
	MB
	rB
	B
	�B
	�B

B

sB

�B
4B
[B
�B
�B
�B
�B
�B
�B
,B
�B
�B
qB
LB
�B
QB
�B
KB
hB
DB
�B
VB
B
>B
�B

B
�B
"B
B
B
;B
�B
�B
WB
�B
SB
�B
>B
B
�B
DB
&B
B
YB
gB
[B
�B
�B
�B
+B
�B
�B
�B
PB
�B
�B
�B
�B
RB
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!B
%B
%B
%�B
&HB
%�B
&0B
&;B
&-B
&"B
&B
&�B
&�B
'yB
(-B
)�B
*�B
*�B
+%B
+WB
,KB
,�B
,B
,B
,^B
-B
.CB
.lB
-�B
-�B
-�B
.$B
.hB
/*B
05B
1�B
2�B
2xB
2!B
2�B
2�B
3uB
4�B
4qB
5,B
5rB
6YB
6�B
7UB
7sB
7oB
7�B
8*B
9B
:�B
>�B
?zB
@B
?�B
?�B
@B
@�B
APB
@�B
A3B
AB
A�B
A8B
B}B
B�B
B�B
CB
CB
C�B
CCB
CB
CSB
C�B
D'B
DB
EB
E�B
E�B
E�B
ESB
ERB
F�B
F�B
F�B
GcB
G�B
H�B
I�B
I�B
J�B
KRB
KB
K�B
K�B
L6B
LxB
K�B
K�B
L5B
LkB
L`B
LB
K�B
L�B
LGB
L�B
L�B
L�B
L�B
M|B
M�B
M'B
M�B
N	B
M�B
M�B
M<B
MB
NB
M�B
M�B
M�B
N�B
O�B
PsB
P�B
P�B
QB
P�B
P�B
P�B
P�B
Q!B
QB
Q�B
R�B
SjB
S�B
T5B
T�B
UB
T�B
TGB
T�B
T�B
UfB
U�B
U�B
U�B
U�B
VPB
V�B
V�B
V�B
W�B
XB
W�B
X�B
X�B
Y>B
YPB
X�B
X�B
Y�B
Z�B
Z�B
Z�B
Z!B
Y�B
Y�B
ZUB
Z�B
Z6B
ZxB
Z�B
[�B
[�B
\B
[�B
\4B
\:B
\�B
]B
]�B
^B
]�B
^3B
^pB
^$B
^?B
^xB
^�B
^�B
_|B
_�B
_QB
_�B
_�B
_�B
`WB
_�B
`�B
`VB
`�B
`;B
a(B
a�B
b.B
bB
bB
b�B
byB
c B
c�B
c�B
cvB
cXB
c%B
c,B
cB
d�B
eTB
e�B
fTB
f�B
f�B
g#B
f�B
f|B
f�B
f�B
g�B
g�B
g�B
g�B
h	B
h�B
h�B
hdB
h�B
i0B
iuB
i2B
i�B
jB
j�B
j�B
k/B
k�B
lAB
l:B
l~B
l�B
mB
m�B
mQB
m�B
n<B
n�B
n�B
nB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oB
o)B
o�B
o�B
o�B
o�B
pB
pB
pB
p]B
p�B
p�G�O�B�)BچB�QB��BߤB�;B�B��B�;B�B�BچB�B�BߤB�jB�5B�dB�B��BޞB�pB�B�B�jB�vB�|B�NB�HB��B�B�vB�B�B�B�NB�B��B�|B�HB�B�NB��B�B��B�HB��B�B�B�ZB��B��B��B��B�&B�TB�&B� B��B�B�B�8B�`B��B�yB�B�WB�DB�B�B�B�B�B��B�oB�/B�5B��B�B��B�B�B�B��B��B�B��B�B��B��B�"B�sB�B��B�DB�WB�B�/B�B�/B�sB�B��B�B�B�0B��B�RB��B�?B��B��B��B�B�B�6B҉B��B��B�kB��B��B�B�VB��Bl�B�Bd�Bb�Bc�BL�B33BI�BfBH�B� B)�BlWBE�B*�B"�B�B��B�B�#B�B�B�hB�UB��B�sB��B�
B�B�
B��B�mB�fB�B��B�B��B�bB�uB��B�B�DB�zB�IB�'B�BB��B�B�-B�RB��B��B��B��B��B�:B��B��B�(BAUB��BуB��B�B��B�qB��B��Bd�B]�BQ�BXEBS[BC-B33B3�B,=B'RBB
��B
��B
˒B
�dB
�gB
�B
�<B
�'B
�LB
�B
�XB
��B
�dB
��B
��B
�B
�UB
��B
� B
��B
ĜB
�BB
B
̘B
�#B
�#B
��B
�fB
��B
�fB
�2B
��B
�)B
�B
��B
�3B
�zB
�EB
�TB
�B
ӏB
�mB
�mB
چB
�NB
�HB
��B
�,B
��B
�?B
��B
خB
�#B
ںB
�B
��B
�B
�ZB
��B
�,B
�yB
�B
��B
�cB�B
��B
�]B
�)B
�;B�B�B�B �B!bB!�B!�B#�B4nB/B0�B/OB0�B1�B1�B0�B1�B2aB1�B1'B2-B2aB2aB8�B7�B7�B2�B/�B#BqB7B�B7B�B$B�B�B�BMB BB�BDB
rB	lB�B	7BfB�BSBBbB	B�B�B�B�B�B%BSBMB+B�BuBYBJBB�B{BGB�B�B	B	B�B�B"B�B�B&LB+�B*�B+�B*�B*�B*eB(�B(�B(�B'�B($B'RB%FB"hB �B!�B!bB!bB"hB!�B#�B"�B"4BVB!B�B �B�B�B�BkB�BB:B BuBB�B�B�B�B�BBBB�B�BB�BB�B�BoB BoB@B�B"B�B�B�B"B
rB	�BB
�B(B�BGB
��B
��B
��B
�JB
�JB
��B
��B
�rB
��B
�lB
��B
��B
�%B
�%B
��B
�%B
��B
��B
��B
�B
�GB
�B
�B
�AB
��B
��B
�]B
��B
�oB
�"B
�QB
�B
�B
�B
�>B
�B
�B
�/B
�B
�dB
ݘB
��B
��B
�)B
�B
�B
ҽB
�B
�dB
�dB
��B
ȀB
��B
�-B
�-B
ÖB
��B
��B
��B
�0B
�B
��B
��B
�dB
�B
�FB
��B
��B
��B
�aB
�B
��B
��B
�'B
�B
��B
��B
��B
��B
��B
�nB
�7B
�B
��B
�B
�B
�B
�VB
��B
�VB
�~B
��B
�B
�lB
�fB
�7B
�fB
�fB
��B
��B
��B
��B
�B
��B
�B
�B
�B
cB
�oB
��B
|PB
zDB
zxB
{B
z�B
zDB
y>B
�B
v�B
~(B
v+B
y	B
�oB
y�B
z�B
}VB
zB
u�B
tTB
sMB
r�B
qAB
qvB
rB
qB
oiB
n�B
l�B
m]B
hsB
gmB
hsB
h>B
e�B
b�B
b�B
b�B
c B
c B
bB
a|B
`B
`�B
_�B
_�B
`�B
`B
\�B
[WB
Y�B
[�B
Z�B
\)B
cTB
W
B
P�B
Q�B
S�B
N�B
NpB
MB
PHB
JXB
J�B
H�B
F?B
E�B
E�B
D3B
G�B
CaB
EmB
CaB
EmB
A�B
>BB
>BB
=�B
=<B
=B
:�B
>�B
:�B
7�B
;�B
>BB
8�B
6�B
4nB
2�B
2�B
2�B
3�B
2�B
1�B
0UB
/�B
0UB
0�B
0UB
/�B
.}B
-B
-CB
-�B
0�B
/OB
,�B
.B
-CB
($B
&�B
%�B
$@B
#�B
"�B
"4B
"hB
#:B
!�B
 �B
�B
�B
�B
�B
~B
�B
IB
B
�B
eB
�B
�B
*0B
�B
FB
B
�B
	�B
�B
DB

=B
fB
�B
�B
�B
1B

�B
�B
�B
�B	��B	��B	��B
 4B	��B
�B	��B	�B	��B	��B	��B	��B	��B	�>B	��B	�DB	�lB	�DB	�8B	�2B	�`B	��B	�`B	�`B	�2B	�+B	�MB	�|B	�B	�B	� B	�B	�/B	�B	�B	��B	�B	�)B	�QB	��B	�DB	�sB	�KB	�sB	�
B	��B	�TB	�TB	��B	�B	��B	�vB	��B	�B	�HB	�B	�;B	ܒB	�B	�B	ٴB	��B	�)B	�QB	�^B	��B	ȀB	��B	�UB	��B	�B	�B	�tB	�tB	�zB	��B	�nB	�4B	��B	��B	��B	��B	�hB	�B	�B	��B	�hB	�4B	��B	��B	�FB	��B	�FB	��B	�B	��B	��B	�FB	��B	�LB	�B	�B	��B	��B	�FB	�B	��B	��B	��B	�XB	�B	��B	�*B	�_B	��B	��B	��B	��B	��B	�XB	�RB	��B	��B	��B	��B	�RB	�B	�LB	�LB	�LB	�LB	��B	�zB	�B	��B	�zB	��B	�B	��B	��B	��B	�FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                          <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<[<�<#�
<ȏ=N=�)=$o�<#�
<I�<Sˡ<���<#�
<���<sQ<�'�<�w�<)�7<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OW + CTM + resolution error)     202006200039532020062000395320200620003953202006200039532020062000395320200620003953SI  SI  ARFMARFM                                                                                                                                                2019081315071020190813150710IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019082322010620190823220106QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019082322010620190823220106QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020061910341620200619103416IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020062000395920200620003959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V01                       ARGO_for_DMQC Climatology Version 2020V01                       2020062000395920200620003959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020062000395920200620003959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                