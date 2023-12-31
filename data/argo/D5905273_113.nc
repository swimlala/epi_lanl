CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-03T14:22:14Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20210203142214  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               q   qAA  AOAO7314_008642_113                 7314_008642_113                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�[�߹8�@�[�߹8�11  @�[�!�R�@�[�!�R�@0��M��@0��M���b�%��S�b�%��S11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @=p�@}p�@�G�@�G�@޸R@��RA  A   A+�A@  A`  A�Q�A�  A��A�  A�  A�Q�A�  A�  B   B  B  B  B   B((�B0(�B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bp  Bw�
B�B��B�  B�  B��B��
B��B��B�  B�  B�  B�  B�{B�  B�{B�{B�{B�  B��
B��B��
B��B�{B�{B�{B�  B��
B�{B�=qB�(�B�{B�  C 
=C
=C
=C{C  C
  C  C
=C
=C  C
=C
=C  C  C��C��C   C"  C$  C&  C'��C*  C,  C-��C/��C2  C4  C6
=C8
=C:
=C<  C>  C@  CB
=CD
=CE��CG��CI��CL  CN  CP
=CR
=CT  CU��CX
=CZ
=C\  C^  C`  Cb
=Cd
=Cf  Ch
=Cj  Cl  Cn{Cp
=Cq��Cs�Cu��Cx  Cy��C|  C~{C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C���C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C���C�  C�C�  C�C�C���C���C���C�  C���C���C�  C�  C�C�C���C���C�C�  C���C���C�  C�  C���C�  C�C�C�C�C�C���C���C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�C�
=C�
=C�  C���C�C�C���C���C���C���C���C���C���C�  C�  C�C�C�C�
=C�
=C�C�C�
=C�
=C�C���C�  C�  C�  C�  C�  C�  C�C�C�
=C�  C���C���C���C�  C�  C�  C�C�C�  C���C�  C���C���C�  C�C�
=C�
=C�  C�  C���C�  C�C�
=D �D � D �qDz�D  D�DD}qD�qD��D�D��D�Dz�D��D��D�D��D	D	� D	��D
}qD  D}qD��D� DD��D  D}qD��D� D�D��D  D� D  D}qD�qD}qD  D� D�RD}qD  Dz�D��D}qD�qD}qD  D� D  D� D�D}qD��D}qD  D��D�D� D�D� D�qD ��D!D!��D"  D"��D#  D#��D$�D$� D$�qD%}qD&  D&��D'D'��D'�qD(� D)  D)� D*  D*��D+  D+}qD+�qD,}qD,�qD-� D.  D.� D.�qD/� D0  D0� D1  D1��D2�D2�D3  D3}qD4  D4� D5  D5� D6  D6� D7  D7��D8�D8� D9  D9� D9�qD:��D;�D;� D<  D<� D=  D=� D>  D>� D?  D?� D?�qD@}qDA  DA� DB  DB��DC  DC� DD  DD��DE�DE� DF  DF� DG�DG��DH  DH}qDH�qDI}qDI�qDJ��DK�DK��DL�DL� DMDM�DN  DN��DO�DO��DPDP� DQ  DQ��DR  DR� DR�qDS}qDS�qDT}qDU�DU��DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[�D[� D\  D\}qD]  D]� D^  D^��D_�D_� D`  D`}qD`�qDa� Db�Db� Dc  Dc� Dc�qDd}qDe�De��Df  Df� Df�qDg� Dh�Dh� Di�Di��Dj�Dj��Dj�qDk}qDl  Dl}qDl��Dm� Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�Dr�Dr� Dr�qDs}qDt  Dt�Du�Du� DvDv��Dv�qDw}qDw�qDx}qDx�qDy}qDz�Dz��Dz�qD{� D{�qD|� D}  D}� D~  D~��D�D��D�HD�AHD���D�� D�  D�@ D�� D�� D�HD�AHD�~�D���D�  D�AHD��HD���D���D�@ D�� D��HD�HD�B�D�� D���D���D�@ D�� D��HD�HD�B�D�� D��qD���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D��HD��HD�  D�@ D��HD�� D��qD�>�D��HD�� D�  D�AHD�~�D���D���D�>�D�� D��HD�HD�@ D�� D��HD�HD�B�D�� D��HD�HD�>�D�~�D�� D��qD�=qD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�@ D�}qD���D���D�@ D�~�D��qD���D�>�D�~�D���D�HD�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D��qD���D�AHD��HD���D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�>�D�� D�� D���D�>�D�� D��HD�HD�AHD�� D��HD��D�@ D��HD��HD���D�@ D���D��HD�  D�>�D�}qD�� D��D�B�D��HD��HD�  D�@ D�~�D���D���D�=qD�~�D�D�HD�@ D��HD��HD�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD���D�  D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D���D���D�@ D��HD��HD�HD�AHD�� D���D�  D�AHD��HD�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�~�D���D�  D�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�� D�� D��qD�>�D�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD���D�� D���D�>�D D��HD�HD�@ D�~�Dþ�D��qD�>�DĀ Dľ�D���D�@ D�~�Dž�D�  D�AHDƁHD�� D�  D�@ DǁHD��HD�  D�@ DȀ D�� D�  D�@ Dɀ Dɾ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D���D�>�D̀ D̾�D��qD�>�D̀ D�� D���D�@ D΂�D��HD�HD�@ Dπ DϾ�D���D�@ DЀ DнqD���D�>�DсHD�D��D�AHD҂�D�D��D�AHDӀ D�� D�  D�@ DԀ D�� D�HD�AHDՁHD�D�HD�@ DցHD��HD�  D�>�D׀ D��HD�  D�@ D؀ D�� D���D�@ Dـ Dپ�D���D�>�Dڀ D�� D���D�AHDۀ D�� D���D�>�D܀ D�� D�  D�@ D�~�Dݾ�D���D�@ Dހ D��HD�HD�@ D߀ D�� D�HD�AHD�� DྸD���D�@ D�HD�� D���D�>�D� D�� D�  D�>�D�HD�D�HD�@ D� D��HD�HD�AHD� D�� D�  D�@ D�~�D澸D���D�>�D�~�D�� D�  D�>�D�}qD�qD���D�>�D�~�D�� D�HD�B�DꂏD��HD�  D�>�D� D뾸D��qD�>�D� D�� D�  D�@ D� D�� D���D�AHD� DD�HD�AHD�HD��HD�HD�AHD�~�D�D���D�>�D� D�� D�  D�>�D�~�D�� D�  D�>�D�HD�� D�  D�@ D� D�� D�  D�>�D�~�D��HD�HD�@ D�� D��HD�HD�AHD��HD��qD���D�@ D�� D�� D�  D�@ D�� D��HD��D�N?�?8Q�?u?�z�?�p�?�G�@   @�@&ff@5@L��@aG�@p��@��@�{@�
=@�G�@�=q@�33@�(�@�ff@�{@�Q�@�G�@���@�z�@��RAz�A	��A{A�\A
=A�A   A$z�A(Q�A,��A1G�A5A:=qA>�RAC�
AHQ�AL(�AP��ATz�AX��A]p�AaG�AeAj=qAn{Aq�AuAy��A~{A�G�A�33A�p�A��A���A��A��A�
=A�G�A�33A�p�A�\)A���A��
A�A�  A��\A���A�
=A���A��
A�ffA���A��HA��A�\)A���A��
A�{A�Q�A�=qA�(�AƸRA���A�33A�p�A�  A��A��
A�{A�Q�Aڏ\A��A�\)A��A�(�A�ffA��A��HA��A�
=A�G�A��
A�{A�Q�A�33A�p�B   B�BffB�B��B�B33B��B
{B\)B��B�B
=B(�Bp�B�RB  Bp�B�\B�B��B{B33BQ�B��B�RB�
B!�B"ffB#�B$��B&=qB'\)B(z�B)B*�HB,  B-p�B.�\B/�
B1�B2�\B3�B4��B6{B7\)B8z�B9B;
=B<Q�B=p�B>�\B?�
B@��BB=qBC�BD��BE�BG\)BH��BIBK
=BLQ�BM��BN�HBP(�BQp�BR�RBT(�BUp�BV�RBW�
BY�BZ=qB[�B\��B^{B_\)B`��BaBb�HBd  Be�Bf�\Bg�
Bh��BjffBk�Bl��Bm��Bn�HBp(�Bq��Br�RBt  Bu�Bv=qBw�Bx��By�B{\)B|��B~{B
=B�(�B��RB�\)B�  B���B�\)B��B�z�B��B��B�ffB�
=B��B�Q�B��HB��B�(�B��RB�p�B�  B��\B�33B��
B��\B�33B��
B�Q�B���B��B�Q�B�
=B���B�(�B���B�\)B�{B���B�\)B��B�z�B�
=B��B�Q�B���B��B�{B���B��B�B�ffB���B��B�  B�z�B�
=B���B�=qB���B�p�B�{B���B�33B�B�ffB�
=B��B�Q�B���B���B�(�B��RB�G�B��B���B�G�B��B��\B�33B��B��\B��B�B�z�B�33B��B��\B�33B��
B�z�B�G�B��B���B�G�B�  B��\B�G�B�{B���B��B�=qB��HB�B��\B�G�B�  B£�B�\)B�(�B���BŮB�ffB��B�Bȏ\B�\)B�{B���B˅B�=qB�
=B��
BΣ�B�G�B�  B���Bљ�B�ffB��B��Bԣ�BՅB�Q�B�33B��Bأ�B�p�B�Q�B��B��
Bܣ�B݅B�Q�B��B��B��B�B�ffB��B�  B���B�B�z�B�G�B�  B�RB陚B�z�B�G�B�{B���B홚B�ffB�G�B�(�B���B�B�Q�B�33B�  B���B��B�(�B�
=B��B���B�p�B�(�B���B��
B���B�G�B�{B���B�C =qC ��C  CffC��C(�Cz�C�
C33C�\C��C�Cp�C�RC�C33Cz�C�C�
C
=C=qCp�C��CC�C�C\)C�C�C�
C
=C=qCp�C��CC�HC	�C	\)C	z�C	��C	�
C

=C
=qC
ffC
�\C
C
��C(�CQ�Cz�C��C�
C{C33CffC�\CC��C�CG�Cp�C�C�HC
=C33C\)C��CC�C{CQ�Cz�C��C��C��C(�C\)Cz�C��C�
C
=C33CQ�C�\C�RC�C
=C33CffC��C�RC�HC
=C=qCp�C�\C�RC�C�CG�CffC�\CC�C{CG�Cz�C��CC  C33CQ�Cz�C�RC�HC
=C33Cp�C��C��C��C33CffC�\C�RC��C(�C\)Cz�C�C�C�CG�Cp�C�C�C{C=qCffC��C�
C��C33Cp�C�\CC  C(�CQ�C�\C��C��C(�CffC��C��C��C33Cp�C��C�
C {C =qC p�C �C �C!�C!Q�C!��C!��C"  C"33C"p�C"�C"�HC#{C#Q�C#�\C#C#�C$(�C$p�C$��C$�
C%{C%G�C%z�C%�C%��C&(�C&Q�C&��C&�
C'
=C'=qC'�\C'�C'��C(33C(p�C(��C(�HC)(�C)\)C)�\C)�
C*
=C*G�C*z�C*C*��C+33C+p�C+�C+�HC,{C,\)C,��C,C-
=C-G�C-z�C-�RC.  C.33C.ffC.�C.�C/{C/\)C/��C/��C0
=C0Q�C0�\C0C1  C1G�C1z�C1C2  C2=qC2p�C2�RC2��C3(�C3p�C3��C3�
C4(�C4\)C4��C4�
C5�C5G�C5�C5��C5��C633C6p�C6��C6�
C7�C7G�C7�C7��C8  C833C8z�C8��C8�HC9�C9Q�C9�C9��C:  C:33C:p�C:�C:�HC;{C;\)C;�\C;C<  C<=qC<ffC<�C<�C={C=\)C=��C=C>{C>G�C>z�C>C>��C?(�C?p�C?��C?�HC@�C@Q�C@�\C@�
CA
=CAG�CA�CA�RCA��CB33CBp�CB�RCB�CC�CCp�CC�\CC�
CD�CDQ�CD�\CD��CE  CE=qCE�\CE�RCE��CF=qCFp�CF��CF�CG�CG\)CG��CGCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                               ?�  @   @=p�@}p�@�G�@�G�@޸R@��RA  A   A+�A@  A`  A�Q�A�  A��A�  A�  A�Q�A�  A�  B   B  B  B  B   B((�B0(�B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bp  Bw�
B�B��B�  B�  B��B��
B��B��B�  B�  B�  B�  B�{B�  B�{B�{B�{B�  B��
B��B��
B��B�{B�{B�{B�  B��
B�{B�=qB�(�B�{B�  C 
=C
=C
=C{C  C
  C  C
=C
=C  C
=C
=C  C  C��C��C   C"  C$  C&  C'��C*  C,  C-��C/��C2  C4  C6
=C8
=C:
=C<  C>  C@  CB
=CD
=CE��CG��CI��CL  CN  CP
=CR
=CT  CU��CX
=CZ
=C\  C^  C`  Cb
=Cd
=Cf  Ch
=Cj  Cl  Cn{Cp
=Cq��Cs�Cu��Cx  Cy��C|  C~{C�C�  C�  C�C�C�  C�  C�  C���C���C���C���C���C�  C�  C�C�C�  C���C�  C�  C���C���C���C�  C���C�  C�C�  C�C�C���C���C���C�  C���C���C�  C�  C�C�C���C���C�C�  C���C���C�  C�  C���C�  C�C�C�C�C�C���C���C�  C�  C�  C�  C�  C���C�  C�C�  C�  C�C�
=C�
=C�  C���C�C�C���C���C���C���C���C���C���C�  C�  C�C�C�C�
=C�
=C�C�C�
=C�
=C�C���C�  C�  C�  C�  C�  C�  C�C�C�
=C�  C���C���C���C�  C�  C�  C�C�C�  C���C�  C���C���C�  C�C�
=C�
=C�  C�  C���C�  C�C�
=D �D � D �qDz�D  D�DD}qD�qD��D�D��D�Dz�D��D��D�D��D	D	� D	��D
}qD  D}qD��D� DD��D  D}qD��D� D�D��D  D� D  D}qD�qD}qD  D� D�RD}qD  Dz�D��D}qD�qD}qD  D� D  D� D�D}qD��D}qD  D��D�D� D�D� D�qD ��D!D!��D"  D"��D#  D#��D$�D$� D$�qD%}qD&  D&��D'D'��D'�qD(� D)  D)� D*  D*��D+  D+}qD+�qD,}qD,�qD-� D.  D.� D.�qD/� D0  D0� D1  D1��D2�D2�D3  D3}qD4  D4� D5  D5� D6  D6� D7  D7��D8�D8� D9  D9� D9�qD:��D;�D;� D<  D<� D=  D=� D>  D>� D?  D?� D?�qD@}qDA  DA� DB  DB��DC  DC� DD  DD��DE�DE� DF  DF� DG�DG��DH  DH}qDH�qDI}qDI�qDJ��DK�DK��DL�DL� DMDM�DN  DN��DO�DO��DPDP� DQ  DQ��DR  DR� DR�qDS}qDS�qDT}qDU�DU��DV  DV� DW  DW� DX  DX� DY  DY� DY��DZ� D[�D[� D\  D\}qD]  D]� D^  D^��D_�D_� D`  D`}qD`�qDa� Db�Db� Dc  Dc� Dc�qDd}qDe�De��Df  Df� Df�qDg� Dh�Dh� Di�Di��Dj�Dj��Dj�qDk}qDl  Dl}qDl��Dm� Dn�Dn��Do�Do��Dp�Dp��Dq�Dq�Dr�Dr� Dr�qDs}qDt  Dt�Du�Du� DvDv��Dv�qDw}qDw�qDx}qDx�qDy}qDz�Dz��Dz�qD{� D{�qD|� D}  D}� D~  D~��D�D��D�HD�AHD���D�� D�  D�@ D�� D�� D�HD�AHD�~�D���D�  D�AHD��HD���D���D�@ D�� D��HD�HD�B�D�� D���D���D�@ D�� D��HD�HD�B�D�� D��qD���D�@ D��HD��HD�  D�@ D�� D��HD�HD�@ D��HD��HD�  D�@ D��HD�� D��qD�>�D��HD�� D�  D�AHD�~�D���D���D�>�D�� D��HD�HD�@ D�� D��HD�HD�B�D�� D��HD�HD�>�D�~�D�� D��qD�=qD�� D�� D�  D�@ D�� D��HD�HD�AHD��HD��HD�  D�@ D��HD��HD�HD�@ D�}qD���D���D�@ D�~�D��qD���D�>�D�~�D���D�HD�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D��qD���D�AHD��HD���D�  D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�  D�>�D�� D�� D���D�>�D�� D��HD�HD�AHD�� D��HD��D�@ D��HD��HD���D�@ D���D��HD�  D�>�D�}qD�� D��D�B�D��HD��HD�  D�@ D�~�D���D���D�=qD�~�D�D�HD�@ D��HD��HD�  D�@ D��HD�� D�  D�>�D�~�D��HD�HD�@ D�� D���D�  D�AHD��HD���D�  D�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D��HD�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D���D���D�@ D��HD��HD�HD�AHD�� D���D�  D�AHD��HD�� D���D�>�D�� D�� D���D�>�D�� D�� D���D�@ D�~�D���D�  D�>�D�~�D�� D���D�@ D�� D�� D�HD�@ D�� D�� D��qD�>�D�� D�� D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD���D�� D���D�>�D D��HD�HD�@ D�~�Dþ�D��qD�>�DĀ Dľ�D���D�@ D�~�Dž�D�  D�AHDƁHD�� D�  D�@ DǁHD��HD�  D�@ DȀ D�� D�  D�@ Dɀ Dɾ�D�  D�AHD�~�D�� D�  D�>�D�~�D�� D���D�>�D̀ D̾�D��qD�>�D̀ D�� D���D�@ D΂�D��HD�HD�@ Dπ DϾ�D���D�@ DЀ DнqD���D�>�DсHD�D��D�AHD҂�D�D��D�AHDӀ D�� D�  D�@ DԀ D�� D�HD�AHDՁHD�D�HD�@ DցHD��HD�  D�>�D׀ D��HD�  D�@ D؀ D�� D���D�@ Dـ Dپ�D���D�>�Dڀ D�� D���D�AHDۀ D�� D���D�>�D܀ D�� D�  D�@ D�~�Dݾ�D���D�@ Dހ D��HD�HD�@ D߀ D�� D�HD�AHD�� DྸD���D�@ D�HD�� D���D�>�D� D�� D�  D�>�D�HD�D�HD�@ D� D��HD�HD�AHD� D�� D�  D�@ D�~�D澸D���D�>�D�~�D�� D�  D�>�D�}qD�qD���D�>�D�~�D�� D�HD�B�DꂏD��HD�  D�>�D� D뾸D��qD�>�D� D�� D�  D�@ D� D�� D���D�AHD� DD�HD�AHD�HD��HD�HD�AHD�~�D�D���D�>�D� D�� D�  D�>�D�~�D�� D�  D�>�D�HD�� D�  D�@ D� D�� D�  D�>�D�~�D��HD�HD�@ D�� D��HD�HD�AHD��HD��qD���D�@ D�� D�� D�  D�@ D�� D��HD��G�O�?�?8Q�?u?�z�?�p�?�G�@   @�@&ff@5@L��@aG�@p��@��@�{@�
=@�G�@�=q@�33@�(�@�ff@�{@�Q�@�G�@���@�z�@��RAz�A	��A{A�\A
=A�A   A$z�A(Q�A,��A1G�A5A:=qA>�RAC�
AHQ�AL(�AP��ATz�AX��A]p�AaG�AeAj=qAn{Aq�AuAy��A~{A�G�A�33A�p�A��A���A��A��A�
=A�G�A�33A�p�A�\)A���A��
A�A�  A��\A���A�
=A���A��
A�ffA���A��HA��A�\)A���A��
A�{A�Q�A�=qA�(�AƸRA���A�33A�p�A�  A��A��
A�{A�Q�Aڏ\A��A�\)A��A�(�A�ffA��A��HA��A�
=A�G�A��
A�{A�Q�A�33A�p�B   B�BffB�B��B�B33B��B
{B\)B��B�B
=B(�Bp�B�RB  Bp�B�\B�B��B{B33BQ�B��B�RB�
B!�B"ffB#�B$��B&=qB'\)B(z�B)B*�HB,  B-p�B.�\B/�
B1�B2�\B3�B4��B6{B7\)B8z�B9B;
=B<Q�B=p�B>�\B?�
B@��BB=qBC�BD��BE�BG\)BH��BIBK
=BLQ�BM��BN�HBP(�BQp�BR�RBT(�BUp�BV�RBW�
BY�BZ=qB[�B\��B^{B_\)B`��BaBb�HBd  Be�Bf�\Bg�
Bh��BjffBk�Bl��Bm��Bn�HBp(�Bq��Br�RBt  Bu�Bv=qBw�Bx��By�B{\)B|��B~{B
=B�(�B��RB�\)B�  B���B�\)B��B�z�B��B��B�ffB�
=B��B�Q�B��HB��B�(�B��RB�p�B�  B��\B�33B��
B��\B�33B��
B�Q�B���B��B�Q�B�
=B���B�(�B���B�\)B�{B���B�\)B��B�z�B�
=B��B�Q�B���B��B�{B���B��B�B�ffB���B��B�  B�z�B�
=B���B�=qB���B�p�B�{B���B�33B�B�ffB�
=B��B�Q�B���B���B�(�B��RB�G�B��B���B�G�B��B��\B�33B��B��\B��B�B�z�B�33B��B��\B�33B��
B�z�B�G�B��B���B�G�B�  B��\B�G�B�{B���B��B�=qB��HB�B��\B�G�B�  B£�B�\)B�(�B���BŮB�ffB��B�Bȏ\B�\)B�{B���B˅B�=qB�
=B��
BΣ�B�G�B�  B���Bљ�B�ffB��B��Bԣ�BՅB�Q�B�33B��Bأ�B�p�B�Q�B��B��
Bܣ�B݅B�Q�B��B��B��B�B�ffB��B�  B���B�B�z�B�G�B�  B�RB陚B�z�B�G�B�{B���B홚B�ffB�G�B�(�B���B�B�Q�B�33B�  B���B��B�(�B�
=B��B���B�p�B�(�B���B��
B���B�G�B�{B���B�C =qC ��C  CffC��C(�Cz�C�
C33C�\C��C�Cp�C�RC�C33Cz�C�C�
C
=C=qCp�C��CC�C�C\)C�C�C�
C
=C=qCp�C��CC�HC	�C	\)C	z�C	��C	�
C

=C
=qC
ffC
�\C
C
��C(�CQ�Cz�C��C�
C{C33CffC�\CC��C�CG�Cp�C�C�HC
=C33C\)C��CC�C{CQ�Cz�C��C��C��C(�C\)Cz�C��C�
C
=C33CQ�C�\C�RC�C
=C33CffC��C�RC�HC
=C=qCp�C�\C�RC�C�CG�CffC�\CC�C{CG�Cz�C��CC  C33CQ�Cz�C�RC�HC
=C33Cp�C��C��C��C33CffC�\C�RC��C(�C\)Cz�C�C�C�CG�Cp�C�C�C{C=qCffC��C�
C��C33Cp�C�\CC  C(�CQ�C�\C��C��C(�CffC��C��C��C33Cp�C��C�
C {C =qC p�C �C �C!�C!Q�C!��C!��C"  C"33C"p�C"�C"�HC#{C#Q�C#�\C#C#�C$(�C$p�C$��C$�
C%{C%G�C%z�C%�C%��C&(�C&Q�C&��C&�
C'
=C'=qC'�\C'�C'��C(33C(p�C(��C(�HC)(�C)\)C)�\C)�
C*
=C*G�C*z�C*C*��C+33C+p�C+�C+�HC,{C,\)C,��C,C-
=C-G�C-z�C-�RC.  C.33C.ffC.�C.�C/{C/\)C/��C/��C0
=C0Q�C0�\C0C1  C1G�C1z�C1C2  C2=qC2p�C2�RC2��C3(�C3p�C3��C3�
C4(�C4\)C4��C4�
C5�C5G�C5�C5��C5��C633C6p�C6��C6�
C7�C7G�C7�C7��C8  C833C8z�C8��C8�HC9�C9Q�C9�C9��C:  C:33C:p�C:�C:�HC;{C;\)C;�\C;C<  C<=qC<ffC<�C<�C={C=\)C=��C=C>{C>G�C>z�C>C>��C?(�C?p�C?��C?�HC@�C@Q�C@�\C@�
CA
=CAG�CA�CA�RCA��CB33CBp�CB�RCB�CC�CCp�CC�\CC�
CD�CDQ�CD�\CD��CE  CE=qCE�\CE�RCE��CF=qCFp�CF��CF�CG�CG\)CG��CGCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                               @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�lG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�;dA�;dA�;dA�1'A�33A�/A�1'A�-A�/A�33A�5?A�33A�33A�7LA�5?A�;dA�A�A�G�A�I�A�G�A�G�A�A�A�I�A�K�A�I�A�K�A�M�A�M�A�K�A�?}A�?}A�;dA�9XA�7LA�7LA�7LA�(�A��A� �A��A�  A���A��mA���A�&�A�hsA�$�AuA�M�A��;A�E�A�M�A���A�&�A���A��`A���A��!A��-A�x�A��DA���A���A�M�A��A�(�A��A��A��!A�XA�33A��#A�VA���A��FA�hsA���A��RA�&�A�r�A�l�A�I�A��A�ƨA�l�A�~�A�XA��A���A�G�A�1A��+A��7A���A�v�A��mA�bA�(�A}�Av��Ar�\AlZAg�Ab�jA_A]&�AY��AXbAVr�ASVAQ+AM��AI+AD^5ABbNAA��A@�A?K�A<��A<(�A;��A:A�A8r�A7l�A5dZA0v�A/��A/C�A,�A,1A*  A(��A(r�A(9XA'��A'�;A'�A&v�A%��A$-A$�A#\)A"r�A"VA"$�A �\AƨA��AC�AVA33A�7A�A�Al�AO�A�AĜA�FA��AA�A�TAt�AdZA33AG�AƨA�;A�TA�`A�yAĜA �A;dAdZA	�
AA��A`BA�;AZA��A	G�A	33Av�A�!AQ�A�;AC�AXA`BA33A �HA �+A �DA v�A 1'@�33@���@�v�@�M�@��@��7@��@���@�K�@���@���@�G�@�&�@�X@�Z@��F@�S�@���@���@��`@�l�@�!@�@�!@�@���@�z�@�@���@���@��m@땁@�t�@�S�@�ff@�E�@���@�(�@��T@�v�@�@�S�@�$�@��/@�@�Z@���@��@�^5@�^@�`B@�A�@���@��@�"�@�p�@���@�1@��@ف@�p�@�`B@ش9@׶F@��T@�V@�z�@�j@�A�@�+@҇+@���@�X@��@Ь@��m@�"�@�
=@���@�-@�J@��@���@̛�@�9X@�9X@� �@��
@�|�@�ȴ@�M�@�{@�p�@�?}@���@ȣ�@�I�@��@���@ǅ@��@Ɨ�@�~�@�@őh@�?}@�Ĝ@�Q�@���@��@\@�M�@��7@���@��@�bN@��F@�C�@��\@�V@�-@�J@�$�@�p�@��@�z�@�(�@�ƨ@��F@��F@��@�1@�\)@�n�@�@��@���@��@�j@�Q�@�  @�|�@�K�@��H@�~�@�p�@���@�bN@�(�@��m@��@�\)@��@�V@�J@��#@���@�G�@��@��9@��@�Z@�1@��F@���@��@�;d@���@�~�@�5?@��@�x�@�&�@�Ĝ@�z�@���@��@�C�@��@�v�@�J@�@���@�p�@�G�@���@��9@�A�@�  @�ƨ@�|�@�\)@�K�@�33@��@���@�E�@��#@���@���@�x�@�7L@��/@�j@�1'@��@��@�1@��;@��@�\)@��y@�-@�@�p�@�O�@�/@���@�Q�@��;@�|�@�
=@���@�V@�J@���@���@�/@���@�1'@��@��@��F@�|�@���@�v�@�{@���@�p�@��@��@��D@��@�1'@��m@�|�@�C�@�
=@���@�n�@�-@��T@��-@�x�@�G�@�/@���@��D@�Q�@�(�@�  @��w@���@�dZ@�S�@�S�@�+@�@��y@���@���@�V@��@��7@�V@��`@��D@�bN@�b@�ƨ@���@�|�@�K�@���@���@�ff@�V@�E�@�-@��@���@�&�@���@��D@�A�@�1@��w@��@���@�|�@�K�@�"�@�
=@�ȴ@�^5@�{@��T@���@�X@�?}@�7L@�7L@�7L@�7L@��`@�Q�@�(�@�1@���@��P@�l�@�C�@�o@���@���@�n�@�@���@���@��h@�G�@��@���@��@��j@��D@�I�@��m@���@�l�@���@��y@��H@��@��R@���@��\@�ff@�-@�J@���@�7L@��/@��D@�I�@� �@�  @�P@~��@~E�@}��@|z�@|�@{��@z�@zn�@y��@y�@x�9@x�@w�;@v��@v��@vv�@vV@vE�@vE�@v5?@u��@u?}@t�@t9X@t�@s��@s33@r�\@q�@q&�@p�9@pA�@p �@p  @ol�@nȴ@nE�@m��@m�-@m`B@l��@l1@k�@j�@j�!@jn�@j-@i�^@ihs@i&�@h�@h1'@g�@gl�@f��@f��@fV@e�@e�-@eO�@d�D@c�m@ct�@cdZ@c"�@b~�@a�#@a�^@a�7@a7L@`�9@`A�@_�;@_�P@_�@^�@^ff@]�T@]��@]?}@\z�@\I�@[�
@[�@["�@Z^5@Y��@Y7L@X��@XQ�@X  @W��@W\)@W�@V5?@U�-@U`B@UV@T��@T1@S��@SdZ@R�@R~�@RM�@R�@Q�@Q�#@Q��@Q7L@PĜ@P1'@O�@Ol�@N�y@N��@N�R@M�@M��@M/@L�D@L1@K�
@Kƨ@K��@Kƨ@J��@J�@I��@Ihs@HĜ@H �@G�w@G�P@G|�@G+@Fȴ@F��@F�+@FE�@F{@E�@D��@D�j@DZ@D9X@C�m@C�F@C��@Ct�@C@B�\@B-@A�#@AX@@�`@@�@@bN@@A�@@b@?�;@?��@?�P@?�@>E�@=��@=�-@=/@<�/@<��@<z�@<I�@<(�@<1@;�
@;��@;�@;t�@;33@;@:�!@:=q@9��@9x�@8��@8��@8�9@8�u@8bN@81'@8b@7�@7|�@6��@6�@6ȴ@6��@6v�@6{@5��@5O�@5�@4�/@4��@4�@4��@4j@4I�@49X@41@3ƨ@3�@3dZ@3"�@2�\@2-@2J@1��@1��@1��@1�7@1x�@1G�@1�@1%@0��@0bN@01'@0  @/�w@/��@/l�@/+@/�@.�y@.�@.��@.5?@-�T@-`B@-O�@-?}@,�@,�j@,��@,j@,9X@+�
@+ƨ@+�F@+�@+dZ@+"�@*�@*��@*n�@)�@)�^@)x�@)&�@(��@(��@(��@(�@( �@'�w@'|�@'K�@&��@&��@&ff@&{@%�@%V@$z�@$9X@#�m@#�F@#��@#33@#@"��@"��@"��@"��@"~�@"M�@"�@"J@!�7@!7L@!7L@!%@ �u@ r�@ 1'@   @�w@�P@l�@K�@+@�@�y@ff@�@�T@@�@p�@?}@/@V@��@j@Z@j@I�@��@�
@ƨ@t�@C�@"�@o@�H@��@n�@�@J@�@�^@�7@hs@��@�u@r�@1'@�@�w@K�@+@
=@ȴ@v�@V@�T@@`B@/@��@��@��@�@��@z�@Z@(�@�m@��@S�@C�@o@�@�!@n�@M�@�@�^@��@X@7L@��@�9@�u@r�@bN@bN@ �@  @  @  @��@�w@�@�P@|�@K�@�@��@��@�y@�R@�+@E�@�@@�h@�@p�@O�@/@/@�@�@��@�j@z�@Z@Z@I�@(�@1@�m@�@33@@
��@
��@
~�@
n�@
M�@
�@	��@	�@	�#@	��@	G�@��@��@Ĝ@�9@��@�@r�@A�@ �@  @�;@��@l�@\)@��@��@��@v�@ff@ff@E�@$�@{@A�;dA�=qA�;dA�9XA�7LA�7LA�9XA�=qA�=qA�=qA�9XA�33A�1'A�1'A�/A�33A�9XA�/A�-A�/A�33A�/A�33A�1'A�/A�+A�/A�-A�/A�/A�/A�/A�1'A�5?A�5?A�33A�7LA�5?A�5?A�7LA�33A�1'A�1'A�1'A�1'A�1'A�/A�33A�5?A�7LA�7LA�5?A�7LA�7LA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�7LA�1'A�-A�/A�33A�/A�/A�33A�E�A�E�A�E�A�E�A�;dA�E�A�A�A�A�A�A�A�C�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�I�A�E�A�E�A�E�A�A�A�A�A�A�A�?}A�?}A�A�A�G�A�I�A�K�A�I�A�G�A�G�A�G�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�I�A�I�A�I�A�K�A�M�A�O�A�Q�A�Q�A�O�A�O�A�O�A�M�A�M�A�K�A�K�A�M�A�M�A�O�A�K�A�G�A�C�A�;dA�A�A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�?}A�?}A�=qA�?}A�=qA�=qA�;dA�5?A�;dA�=qA�7LA�9XA�;dA�=qA�;dA�;dA�9XA�5?A�7LA�5?A�7LA�9XA�;dA�9XA�7LA�5?A�5?A�9XA�;dA�=qA�=qA�?}A�5?A�/A�1'A�1'A�+A�&�A�$�A�$�A�$�A�"�A��A��A��A��A� �A�$�A� �A��A��A� �A�$�A�&�A� �A� �A� �A��A��A��A���A���A�  A�  A�  A�  A�  A���A���A���A���A���A��A��A��yA��`A��`A��mA��`A��TA��TA��A���A�  A�bA��A� �A� �A� �A�(�A�-A�-A�+A�/A�/A�33A�I�A�ZA���A��mA�K�A�A�VA�K�A�n�A�|�A�z�A�t�A�XA¶FA�JA���A��A��/A���A�v�A�XA�"�A��A��^A��A�5?A��mA���A�v�A��A���A��7A�A�A�A��yA�ƨA���A�l�A�K�A�5?A�(�A�ƨA�O�A�1A��;A���A��/A��TA�bNA�{A��A��TA��A��A�ĜA��A��+A�bNA�;dA�-A�VA���A�S�A��A���A��A��RA��A��^A�$�A��A���A�K�A�1A��mA��wA���A���A��hA�p�A�=qA��A��A�VA�VA��/A���A�K�A�33A��A���A���A���A��PA�1'A��9A���A�x�A�jA�\)A�Q�A�K�A�C�A�;dA�33A�-A�{A�A���A��mA���A�bA�ƨA��^A��A��hA�z�A�t�A�n�A�ZA�33A�"�A�JA��mA���A��-A��!A��!A���A��PA��+A�x�A�K�A��A�VA��;A�dZA��A��
A��DA� �A��A��;A���A��jA��!A���A��uA��+A�t�A�dZA�O�A�1'A���A��uA�;dA��/A�n�A�?}A��A�A��/A��-A�v�A�=qA�A���A��-A�v�A�E�A�7LA�bA�ƨA��DA�p�A�O�A�+A��A�JA���A��A���A���A��PA�l�A�K�A�$�A��yA�ĜA��!A���A��hA��hA���A��A���A���A���A���A���A���A���A���A��9A��^A��jA��jA��jA��jA��wA���A��jA��wA��wA��FA���A��hA��+A�hsA�33A��
A�ffA�O�A�33A�oA��A�ƨA��9A��uA�n�A�\)A�S�A�?}A�-A��A���A��jA�;dA��\A�+A��/A�r�A�5?A��HA�n�A��A�l�A��HA��^A�t�A���A�(�A�VA���A��
A�p�A�VA�ƨA��+A�E�A��A��A���A�ĜA��^A���A���A���A��hA��uA���A��hA��\A�ƨA��#A�bA�{A�%A��;A���A�K�A���A���A�bNA�$�A���A�z�A�O�A�5?A�%A��#A�ĜA���A�dZA�5?A�/A��A�A��mA�ƨA���A�v�A�M�A�/A�{A���A�ȴA���A�\)A�bA��A�VA���A�ZA�1'A�A���A��FA�t�A��A���A��\A�M�A�"�A���A��/A���A�|�A�G�A�+A�"�A��A�A��A���A�7LA���A��A��mA��HA��;A��#A��
A���A���A���A���A�z�A�`BA�G�A�/A�{A���A��#A��^A��uA�p�A�?}A�JA���A���A�|�A�E�A��A�t�A���A���A�`BA�A��9A��7A��A�n�A�^5A�K�A�;dA�/A�&�A��A�
=A�  A��yA���A��RA��+A�ffA�O�A�33A��A�A���A��A��yA��;A�ƨA��!A��hA�n�A�M�A�-A�JA��TA�+A�I�A7LA~ȴA~r�A~$�A}��A}oA{�FAy��Ax��AxVAw�wAv�HAvZAv�Au�Au�-AuhsAt��AtVAs�Ast�Arn�Aq��Ap�HApZAo��Ao
=An1'AmhsAl�+AkhsAj��Aj�!AjffAi��AiS�AhȴAh9XAg
=Ae��AehsAe`BAe\)AeG�Ad��Ad^5Ac�7Ab�`Aa/A`�\A`ZA`Q�A`~�A`bNA`$�A_�
A_�hA_G�A^��A^��A^I�A]�A]��A]\)A]VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                               A�;dA�;dA�;dA�1'A�33A�/A�1'A�-A�/A�33A�5?A�33A�33A�7LA�5?A�;dA�A�A�G�A�I�A�G�A�G�A�A�A�I�A�K�A�I�A�K�A�M�A�M�A�K�A�?}A�?}A�;dA�9XA�7LA�7LA�7LA�(�A��A� �A��A�  A���A��mA���A�&�A�hsA�$�AuA�M�A��;A�E�A�M�A���A�&�A���A��`A���A��!A��-A�x�A��DA���A���A�M�A��A�(�A��A��A��!A�XA�33A��#A�VA���A��FA�hsA���A��RA�&�A�r�A�l�A�I�A��A�ƨA�l�A�~�A�XA��A���A�G�A�1A��+A��7A���A�v�A��mA�bA�(�A}�Av��Ar�\AlZAg�Ab�jA_A]&�AY��AXbAVr�ASVAQ+AM��AI+AD^5ABbNAA��A@�A?K�A<��A<(�A;��A:A�A8r�A7l�A5dZA0v�A/��A/C�A,�A,1A*  A(��A(r�A(9XA'��A'�;A'�A&v�A%��A$-A$�A#\)A"r�A"VA"$�A �\AƨA��AC�AVA33A�7A�A�Al�AO�A�AĜA�FA��AA�A�TAt�AdZA33AG�AƨA�;A�TA�`A�yAĜA �A;dAdZA	�
AA��A`BA�;AZA��A	G�A	33Av�A�!AQ�A�;AC�AXA`BA33A �HA �+A �DA v�A 1'@�33@���@�v�@�M�@��@��7@��@���@�K�@���@���@�G�@�&�@�X@�Z@��F@�S�@���@���@��`@�l�@�!@�@�!@�@���@�z�@�@���@���@��m@땁@�t�@�S�@�ff@�E�@���@�(�@��T@�v�@�@�S�@�$�@��/@�@�Z@���@��@�^5@�^@�`B@�A�@���@��@�"�@�p�@���@�1@��@ف@�p�@�`B@ش9@׶F@��T@�V@�z�@�j@�A�@�+@҇+@���@�X@��@Ь@��m@�"�@�
=@���@�-@�J@��@���@̛�@�9X@�9X@� �@��
@�|�@�ȴ@�M�@�{@�p�@�?}@���@ȣ�@�I�@��@���@ǅ@��@Ɨ�@�~�@�@őh@�?}@�Ĝ@�Q�@���@��@\@�M�@��7@���@��@�bN@��F@�C�@��\@�V@�-@�J@�$�@�p�@��@�z�@�(�@�ƨ@��F@��F@��@�1@�\)@�n�@�@��@���@��@�j@�Q�@�  @�|�@�K�@��H@�~�@�p�@���@�bN@�(�@��m@��@�\)@��@�V@�J@��#@���@�G�@��@��9@��@�Z@�1@��F@���@��@�;d@���@�~�@�5?@��@�x�@�&�@�Ĝ@�z�@���@��@�C�@��@�v�@�J@�@���@�p�@�G�@���@��9@�A�@�  @�ƨ@�|�@�\)@�K�@�33@��@���@�E�@��#@���@���@�x�@�7L@��/@�j@�1'@��@��@�1@��;@��@�\)@��y@�-@�@�p�@�O�@�/@���@�Q�@��;@�|�@�
=@���@�V@�J@���@���@�/@���@�1'@��@��@��F@�|�@���@�v�@�{@���@�p�@��@��@��D@��@�1'@��m@�|�@�C�@�
=@���@�n�@�-@��T@��-@�x�@�G�@�/@���@��D@�Q�@�(�@�  @��w@���@�dZ@�S�@�S�@�+@�@��y@���@���@�V@��@��7@�V@��`@��D@�bN@�b@�ƨ@���@�|�@�K�@���@���@�ff@�V@�E�@�-@��@���@�&�@���@��D@�A�@�1@��w@��@���@�|�@�K�@�"�@�
=@�ȴ@�^5@�{@��T@���@�X@�?}@�7L@�7L@�7L@�7L@��`@�Q�@�(�@�1@���@��P@�l�@�C�@�o@���@���@�n�@�@���@���@��h@�G�@��@���@��@��j@��D@�I�@��m@���@�l�@���@��y@��H@��@��R@���@��\@�ff@�-@�J@���@�7L@��/@��D@�I�@� �@�  @�P@~��@~E�@}��@|z�@|�@{��@z�@zn�@y��@y�@x�9@x�@w�;@v��@v��@vv�@vV@vE�@vE�@v5?@u��@u?}@t�@t9X@t�@s��@s33@r�\@q�@q&�@p�9@pA�@p �@p  @ol�@nȴ@nE�@m��@m�-@m`B@l��@l1@k�@j�@j�!@jn�@j-@i�^@ihs@i&�@h�@h1'@g�@gl�@f��@f��@fV@e�@e�-@eO�@d�D@c�m@ct�@cdZ@c"�@b~�@a�#@a�^@a�7@a7L@`�9@`A�@_�;@_�P@_�@^�@^ff@]�T@]��@]?}@\z�@\I�@[�
@[�@["�@Z^5@Y��@Y7L@X��@XQ�@X  @W��@W\)@W�@V5?@U�-@U`B@UV@T��@T1@S��@SdZ@R�@R~�@RM�@R�@Q�@Q�#@Q��@Q7L@PĜ@P1'@O�@Ol�@N�y@N��@N�R@M�@M��@M/@L�D@L1@K�
@Kƨ@K��@Kƨ@J��@J�@I��@Ihs@HĜ@H �@G�w@G�P@G|�@G+@Fȴ@F��@F�+@FE�@F{@E�@D��@D�j@DZ@D9X@C�m@C�F@C��@Ct�@C@B�\@B-@A�#@AX@@�`@@�@@bN@@A�@@b@?�;@?��@?�P@?�@>E�@=��@=�-@=/@<�/@<��@<z�@<I�@<(�@<1@;�
@;��@;�@;t�@;33@;@:�!@:=q@9��@9x�@8��@8��@8�9@8�u@8bN@81'@8b@7�@7|�@6��@6�@6ȴ@6��@6v�@6{@5��@5O�@5�@4�/@4��@4�@4��@4j@4I�@49X@41@3ƨ@3�@3dZ@3"�@2�\@2-@2J@1��@1��@1��@1�7@1x�@1G�@1�@1%@0��@0bN@01'@0  @/�w@/��@/l�@/+@/�@.�y@.�@.��@.5?@-�T@-`B@-O�@-?}@,�@,�j@,��@,j@,9X@+�
@+ƨ@+�F@+�@+dZ@+"�@*�@*��@*n�@)�@)�^@)x�@)&�@(��@(��@(��@(�@( �@'�w@'|�@'K�@&��@&��@&ff@&{@%�@%V@$z�@$9X@#�m@#�F@#��@#33@#@"��@"��@"��@"��@"~�@"M�@"�@"J@!�7@!7L@!7L@!%@ �u@ r�@ 1'@   @�w@�P@l�@K�@+@�@�y@ff@�@�T@@�@p�@?}@/@V@��@j@Z@j@I�@��@�
@ƨ@t�@C�@"�@o@�H@��@n�@�@J@�@�^@�7@hs@��@�u@r�@1'@�@�w@K�@+@
=@ȴ@v�@V@�T@@`B@/@��@��@��@�@��@z�@Z@(�@�m@��@S�@C�@o@�@�!@n�@M�@�@�^@��@X@7L@��@�9@�u@r�@bN@bN@ �@  @  @  @��@�w@�@�P@|�@K�@�@��@��@�y@�R@�+@E�@�@@�h@�@p�@O�@/@/@�@�@��@�j@z�@Z@Z@I�@(�@1@�m@�@33@@
��@
��@
~�@
n�@
M�@
�@	��@	�@	�#@	��@	G�@��@��@Ĝ@�9@��@�@r�@A�@ �@  @�;@��@l�@\)@��@��@��@v�@ff@ff@E�@$�@{G�O�A�;dA�=qA�;dA�9XA�7LA�7LA�9XA�=qA�=qA�=qA�9XA�33A�1'A�1'A�/A�33A�9XA�/A�-A�/A�33A�/A�33A�1'A�/A�+A�/A�-A�/A�/A�/A�/A�1'A�5?A�5?A�33A�7LA�5?A�5?A�7LA�33A�1'A�1'A�1'A�1'A�1'A�/A�33A�5?A�7LA�7LA�5?A�7LA�7LA�5?A�5?A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�7LA�1'A�-A�/A�33A�/A�/A�33A�E�A�E�A�E�A�E�A�;dA�E�A�A�A�A�A�A�A�C�A�E�A�E�A�G�A�G�A�I�A�I�A�I�A�I�A�K�A�I�A�I�A�G�A�G�A�I�A�I�A�I�A�I�A�I�A�G�A�G�A�G�A�E�A�E�A�G�A�G�A�G�A�I�A�E�A�E�A�E�A�A�A�A�A�A�A�?}A�?}A�A�A�G�A�I�A�K�A�I�A�G�A�G�A�G�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�G�A�I�A�I�A�K�A�K�A�M�A�M�A�M�A�M�A�I�A�I�A�I�A�K�A�M�A�O�A�Q�A�Q�A�O�A�O�A�O�A�M�A�M�A�K�A�K�A�M�A�M�A�O�A�K�A�G�A�C�A�;dA�A�A�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�?}A�?}A�=qA�?}A�=qA�=qA�;dA�5?A�;dA�=qA�7LA�9XA�;dA�=qA�;dA�;dA�9XA�5?A�7LA�5?A�7LA�9XA�;dA�9XA�7LA�5?A�5?A�9XA�;dA�=qA�=qA�?}A�5?A�/A�1'A�1'A�+A�&�A�$�A�$�A�$�A�"�A��A��A��A��A� �A�$�A� �A��A��A� �A�$�A�&�A� �A� �A� �A��A��A��A���A���A�  A�  A�  A�  A�  A���A���A���A���A���A��A��A��yA��`A��`A��mA��`A��TA��TA��A���A�  A�bA��A� �A� �A� �A�(�A�-A�-A�+A�/A�/A�33A�I�A�ZA���A��mA�K�A�A�VA�K�A�n�A�|�A�z�A�t�A�XA¶FA�JA���A��A��/A���A�v�A�XA�"�A��A��^A��A�5?A��mA���A�v�A��A���A��7A�A�A�A��yA�ƨA���A�l�A�K�A�5?A�(�A�ƨA�O�A�1A��;A���A��/A��TA�bNA�{A��A��TA��A��A�ĜA��A��+A�bNA�;dA�-A�VA���A�S�A��A���A��A��RA��A��^A�$�A��A���A�K�A�1A��mA��wA���A���A��hA�p�A�=qA��A��A�VA�VA��/A���A�K�A�33A��A���A���A���A��PA�1'A��9A���A�x�A�jA�\)A�Q�A�K�A�C�A�;dA�33A�-A�{A�A���A��mA���A�bA�ƨA��^A��A��hA�z�A�t�A�n�A�ZA�33A�"�A�JA��mA���A��-A��!A��!A���A��PA��+A�x�A�K�A��A�VA��;A�dZA��A��
A��DA� �A��A��;A���A��jA��!A���A��uA��+A�t�A�dZA�O�A�1'A���A��uA�;dA��/A�n�A�?}A��A�A��/A��-A�v�A�=qA�A���A��-A�v�A�E�A�7LA�bA�ƨA��DA�p�A�O�A�+A��A�JA���A��A���A���A��PA�l�A�K�A�$�A��yA�ĜA��!A���A��hA��hA���A��A���A���A���A���A���A���A���A���A��9A��^A��jA��jA��jA��jA��wA���A��jA��wA��wA��FA���A��hA��+A�hsA�33A��
A�ffA�O�A�33A�oA��A�ƨA��9A��uA�n�A�\)A�S�A�?}A�-A��A���A��jA�;dA��\A�+A��/A�r�A�5?A��HA�n�A��A�l�A��HA��^A�t�A���A�(�A�VA���A��
A�p�A�VA�ƨA��+A�E�A��A��A���A�ĜA��^A���A���A���A��hA��uA���A��hA��\A�ƨA��#A�bA�{A�%A��;A���A�K�A���A���A�bNA�$�A���A�z�A�O�A�5?A�%A��#A�ĜA���A�dZA�5?A�/A��A�A��mA�ƨA���A�v�A�M�A�/A�{A���A�ȴA���A�\)A�bA��A�VA���A�ZA�1'A�A���A��FA�t�A��A���A��\A�M�A�"�A���A��/A���A�|�A�G�A�+A�"�A��A�A��A���A�7LA���A��A��mA��HA��;A��#A��
A���A���A���A���A�z�A�`BA�G�A�/A�{A���A��#A��^A��uA�p�A�?}A�JA���A���A�|�A�E�A��A�t�A���A���A�`BA�A��9A��7A��A�n�A�^5A�K�A�;dA�/A�&�A��A�
=A�  A��yA���A��RA��+A�ffA�O�A�33A��A�A���A��A��yA��;A�ƨA��!A��hA�n�A�M�A�-A�JA��TA�+A�I�A7LA~ȴA~r�A~$�A}��A}oA{�FAy��Ax��AxVAw�wAv�HAvZAv�Au�Au�-AuhsAt��AtVAs�Ast�Arn�Aq��Ap�HApZAo��Ao
=An1'AmhsAl�+AkhsAj��Aj�!AjffAi��AiS�AhȴAh9XAg
=Ae��AehsAe`BAe\)AeG�Ad��Ad^5Ac�7Ab�`Aa/A`�\A`ZA`Q�A`~�A`bNA`$�A_�
A_�hA_G�A^��A^��A^I�A]�A]��A]\)A]VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	.}B	-�B	-B	-�B	-�B	-B	,qB	,�B	.B	-�B	,�B	-�B	-wB	-B	-wB	,�B	-�B	-wB	-�B	-CB	-wB	-�B	,�B	,qB	-CB	,qB	-CB	-B	/B	1�B	1�B	1[B	2-B	2aB	1�B	2aB	1[B	1�B	2-B	2�B	0UB	0!B	/B	4�B	E9B	R�B
+kB
�vB
�QB�B,�B.}B?}BT�BLdBQBg�B|�B� B��BǮB�<B�BیB�EB��B�TB�>B��B�8B�B��B��B�JB��B��BƨB�B��B��B�1B&�B
��B1B�B
�B
��B
�^B
�@B
|�B
lWB
c�B
XEB
H�B
/�B
%�B
$B

=B	�,B	��B	��B	yrB	]dB	IRB	@�B	:^B	(�B	�B	�B��B�"B�iB�BB��B��B�dB��B�RB��B�B�B�mB�RB�EB�B�0B��B��B�B��B��B��B�wB�qB��B�_B��B��B��B�}B��B�
B�B�B��B��B�8B�B	�B	HB	J�B	V�B	e`B	dZB	`BB	cTB	^�B	^jB	W?B	NpB	EmB	5B	/�B	?}B	F?B	LdB	X�B	g�B	zB	�;B	�oB	�SB	�@B	��B	��B	kB	S�B	O�B	^�B	kB	{JB	�rB	�hB	�uB	�kB	cB	{B	y	B	v�B	l�B	r�B	u�B	w�B	zDB	��B	��B	�PB	�lB	��B	�=B	�xB	�~B	��B	�xB	�=B	�rB	��B	�7B	�_B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	� B	�bB	��B	�.B	��B	�B	�bB	��B	��B	��B	��B	�qB	��B	��B	��B	��B	�B	�RB	��B	��B	�B	��B	��B	��B	�B	�UB	��B	�'B	�*B	��B	�B	��B	�!B	�UB	��B	�0B	��B	��B	��B	�OB	�B	�XB	�_B	�0B	�[B	�aB	��B	��B	�aB	��B	��B	��B	�9B	��B	��B	�RB	��B	��B	�}B	�gB	ǮB	��B	��B	�}B	�B	�}B	�[B	��B	רB	רB	רB	��B	�B	�KB	�/B	�B	ޞB	�;B	�BB	��B	�B	�B	��B	�BB	ߤB	ߤB	�/B	ݘB	��B	یB	یB	��B	ܒB	�/B	�5B	ݘB	�jB	�B	�HB	�NB	�B	��B	��B	��B	�B	�B	�B	��B	�B	�)B	�QB	��B	��B	��B	�B	��B	�GB	��B	�B	�B	�B	�B	�AB	�AB	�B	�GB	�B	�|B	�B	�B	�|B	�MB	�B	��B	�`B	�8B	�	B	��B	��B	��B	�lB	�lB	�>B	�rB	��B	��B	��B	�B	�xB	��B	��B	��B	�xB	�JB	��B	��B	�(B	��B	��B	�.B	��B	��B
  B	��B	��B
 �B
;B
oB
�B
AB
�B
�B
�B
AB
GB
�B
MB
�B
+B
_B
_B
�B

�B

rB

�B

�B
DB
�B
"B
�B
�B
�B
�B
�B
�B
(B
�B
�B
oB
B
�B
�B
B
�B
�B
SB
B
SB
�B
�B
_B
_B
1B
eB
�B
kB
�B
�B
�B
=B
qB
�B
xB
�B
B
~B
�B
�B
B
OB
OB
OB
!B
!B
�B
�B
�B
 \B
 \B
 �B
 �B
 \B
 �B
 �B
 �B
!-B
 �B
!�B
!�B
"�B
#B
#:B
#�B
#�B
$tB
$�B
$�B
$�B
%B
%�B
&B
&LB
&B
&LB
&B
&�B
&�B
'RB
'�B
($B
(XB
(�B
(�B
(�B
(�B
(�B
)_B
)*B
)*B
*0B
*�B
+6B
+6B
+�B
,B
,B
+�B
+�B
+�B
+kB
,�B
-CB
,�B
,�B
-wB
-�B
-�B
-�B
-�B
.}B
.�B
/B
/�B
0!B
0UB
0�B
0�B
0�B
0�B
0�B
1'B
1'B
1�B
2�B
1�B
3�B
4B
49B
49B
4nB
4�B
5B
5tB
5�B
5�B
6B
6�B
7�B
8B
8�B
8�B
9$B
8�B
9�B
9�B
9�B
9�B
:�B
:^B
:�B
;0B
;0B
<�B
<6B
<�B
<�B
>BB
>wB
>�B
>�B
>�B
>�B
>wB
>wB
?B
?HB
@B
@B
@B
A B
A�B
B�B
B�B
CaB
CaB
C�B
C�B
CaB
C�B
C�B
D3B
D�B
D�B
D�B
EB
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
GzB
G�B
H�B
H�B
IB
IRB
IRB
I�B
I�B
J#B
J�B
J�B
K)B
K)B
K^B
LdB
LdB
L�B
LdB
L�B
MjB
M�B
NB
N<B
N�B
N�B
OB
O�B
O�B
PHB
QB
P�B
QNB
QNB
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
T,B
T,B
TaB
U�B
VB
W�B
W�B
X�B
Y�B
YB
Y�B
Z�B
Z�B
[�B
[�B
[�B
[�B
\]B
\�B
\�B
\]B
]/B
\�B
\)B
\�B
^B
]�B
]�B
]�B
\�B
\�B
]/B
]�B
^�B
`B
_B
^B
^5B
_B
`B
`�B
`�B
`vB
`�B
aB
`�B
`�B
`�B
`�B
aB
a�B
bB
b�B
b�B
b�B
c B
c B
cTB
cTB
c�B
dZB
dZB
d�B
e`B
e�B
e�B
e�B
e�B
f2B
ffB
f�B
ffB
f�B
g�B
g8B
g8B
g�B
gmB
g�B
h>B
hsB
h�B
h�B
h�B
iB
iB
iB
iDB
iyB
jB
jB
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
l�B
m]B
m]B
m]B
m]B
m�B
m�B
n/B
n�B
n�B
o B
o5B
o B
o5B
oiB
oiB
oiB
o�B
o�B
pB
pB
poB
qAB
qAB
qAB
qvB
q�B
q�B
q�B
q�B
q�B
rB
rGB
r�B
sB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
t�B
u%B
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
yrB
y�B
y�B
zB
zxB
z�B
z�B
z�B
{�B
{�B
|�B
}VB
}�B
}�B
}�B
~]B
~�B
~�B
.B
.B
cB
�B
�B
�B
�B
�4B
�4B
�iB
�B
�;B
�;B
��B
��B
�B
�B
�B
�B
�B
�AB
�uB
��B
�B
�B
�GB
�{B
�{B
��B
��B
��B
�B
��B
��B
�B
�MB
��B
��B
��B
��B
��B
�SB
�SB
��B
��B
��B
��B
��B
�+B
�_B
�_B
�_B
��B
�1B
�fB
��B
��B
��B
�B
��B
��B
�7B
�B
�7B
�lB
��B
�	B
�=B
�rB
�rB
��B
�B
�B
�xB
��B
��B
�B
�B
�JB
��B
�PB
��B
��B
��B
��B
�"B
��B
��B
��B
��B
�\B
�\B
�\B
��B
��B
��B
�.B
�.B
��B
��B
�bB
��B
��B
��B
� B
�4B
��B
�B
�:B
�oB
�oB
��B
�B
�@B
�@B
��B
��B
��B
�B
�B
�B
��B
�FB
�FB
�FB
��B
��B
��B
��B
�B
�B
�B
��B
�B
�SB
�SB
��B
��B
��B
��B
�$B
�$B
�$B
�$B
�YB
��B
��B
�+B
�_B
��B
��B
��B
��B
��B
��B
�1B
�1B
��B
��B
��B
�kB
�7B
�7B
�kB
�kB
��B
��B
��B
�=B
��B	.IB	.IB	.}B	/B	.}B	.IB	-wB	-�B	-B	,�B	,=B	-wB	.IB	-�B	.IB	,B	,qB	-wB	-�B	+�B	.}B	,qB	,B	-CB	*eB	-CB	+�B	-CB	-�B	.IB	.�B	.}B	-�B	-B	-�B	,�B	,�B	-CB	-B	-B	.IB	-�B	.B	.}B	.B	-�B	.IB	-�B	-wB	,qB	-B	-�B	-CB	-CB	,�B	,�B	,�B	-B	-CB	-wB	,�B	,�B	,=B	-CB	.}B	-�B	-�B	,�B	-CB	-�B	,qB	,�B	-wB	,�B	*0B	0!B	,B	.}B	.B	.}B	.B	.IB	.IB	.B	-�B	-wB	-wB	-CB	,�B	,�B	,�B	-�B	.IB	.IB	-�B	-B	-B	,=B	,qB	-B	-�B	-�B	-�B	.IB	-�B	-�B	-wB	,�B	.B	,�B	.B	-�B	-�B	.IB	.IB	-�B	.�B	+6B	+�B	+�B	,B	-�B	-�B	-CB	,qB	+�B	,B	,B	,=B	,�B	-B	-wB	-�B	.B	-CB	-B	,qB	,=B	,B	,�B	,=B	,�B	-CB	-B	.B	-�B	-�B	,�B	,�B	,�B	,=B	,�B	,qB	-�B	-�B	.B	.}B	/�B	/B	-�B	.�B	0�B	2-B	3�B	1'B	2-B	0�B	1'B	0�B	0�B	1�B	2-B	2aB	2-B	2�B	1�B	/�B	0�B	1'B	1�B	2-B	/�B	33B	2�B	1�B	1�B	2aB	2-B	1�B	2-B	33B	2�B	2�B	2aB	1�B	1�B	2-B	1�B	2�B	2�B	2�B	2�B	2-B	0�B	1'B	3hB	5?B	/�B	1[B	4�B	0�B	1'B	0�B	1[B	1�B	1�B	2aB	1�B	1�B	0!B	2-B	2�B	4B	2-B	1�B	1[B	0UB	1�B	1'B	2�B	2�B	4�B	2�B	5�B	/�B	0UB	0!B	/�B	/B	/�B	0�B	1'B	0�B	/�B	0!B	/B	/�B	0UB	/�B	.�B	.B	.B	.�B	/�B	33B	7�B	6�B	6�B	=qB	A�B	D�B	EmB	CaB	GzB	GB	G�B	H�B	I�B	K^B	I�B	QNB	YB	Z�B	�$B	��B
oB
7�B
Y�B
y	B
��B
��B
��B
�B
ѷB
ҽB
�&B
уB
�EB
�B
̘B
�B
�B
�B�B�B+BB�B(�B%�B0UB.IB*�B/�B*�B-CB/�B*�B*�B+6BA�B7�B2-B4B1�Bm�Bb�BYB\)BR�BP}BN�BK�BPHBO�BK^BI�BI�BGBK^BY�B^�B[�BZ�BZ�BgBtTB��BzDBtTBy�B��B~(B�B��ByrB|B�AB�7B��B�'B�[B�6B�'B�B��B�B��B�EBбB�jB̘B��B�>B�aB�B�|B�B�/B��BچB�BרB�sB�sB�jB��B�
B�EB�NB�MB�B��B�B�B��B�B�NB�mB��B�B��B��B�DB�B�B�B�WB�2B�fB��B��B�
B��B�QB�.B��B��B�cB��B�|B��B�JB��B��B��B��B�"B��B�(B��B��B �B �B�B�B
rBoB��B�B��B�lB��B�fB��B�;B�cB�cB�B�B��B�BخB�[B�NB�<B�B��B�9B�KB��B��B�B�B�XB�B��B��B�'B��B�IB��B�6B�BB� B�6B�sB�B�BרB�?BרB�]B�,B��B��B��B�B�B�>B�sB�yB�B��B�WB�B�B��B�>B�BB�rB�fB��B�B��B�B�B��B�B�vB�5B��B��B�sBרB��B�mB�'B��B��B��B�Bw2BncB[�BPB:�B8BH�B!�B�BB�B �B�B�B�B
�B�B �B
��B
��B
��B
��B
�B
��B
�>B
��B
�B
��B
��B
�KB�B�B!-B#�B#:B#�B"�B�B�B�B�B
=BSB
�.B
��B
��B
�>B
�B
�.B
��B
��B
�yB
�KB
��B
�B
�`B
��B
�pB
ߤB
�B
�sB
רB
�2B
��B
ǮB
��B
��B
��B
��B
�eB
��B
�nB
�	B
��B
��B
�YB
�7B
��B
��B
��B
�1B
�;B
��B
yrB
�xB
v�B
v�B
v`B
z�B
q�B
�4B
}�B
h�B
g8B
iDB
g8B
e`B
e�B
f�B
d�B
d�B
d�B
m�B
_B
f�B
_;B
_B
\�B
^jB
YKB
Y�B
YKB
V9B
U�B
T�B
S[B
L�B
IB
M6B
T�B
L0B
I�B
;dB
;�B
:�B
6zB
-�B
+�B
0�B
-�B
,�B
,qB
*�B
'�B
'�B
'RB
$@B
%B
%�B
"�B
%�B
�B
�B
CB
�B
YB
@B
�B
4B
hB
�B
\B
PB
DB
�B
oB
 �B
�B
B
�B	��B	�TB	�|B	�#B	ںB	�B	��B	��B	ĜB	��B	�qB	��B	�UB	��B	�B	��B	��B	��B	��B	��B	��B	�*B	�SB	�hB	�VB	�MB	�SB	�_B	�AB	�oB	��B	y�B	h
B	a�B	v�B	k�B	dZB	\�B	m]B	e�B	LdB	J�B	J�B	J�B	M6B	N�B	K)B	IRB	RTB	<�B	7�B	5tB	.IB	GzB	E�B	D�B	B[B	D3B	?�B	@�B	>wB	<B	:�B	6zB	8�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021020314221420210203142214IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021313222620210213132226QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021313222620210213132226QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014420210427140144IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162923              CF      PSAL                            ?�  G�O�D�NG�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163436    CF                  PSAL            G�O�?�G�O�CH{G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                