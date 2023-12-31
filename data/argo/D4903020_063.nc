CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-20T18:21:46Z creation; 2021-03-26T17:01:01Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210120182146  20210326170211  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               ?   ?AA  AOAO7836_008777_063                 7836_008777_063                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�X����@�X����11  @�X��r@�X��r@;��]x�@;��]x��d��(�߹�d��(�߹11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @�  @��R@��R@�p�A ��A�A   A*�HA?\)A`  A�  A�  A�  A��A��AϮA�  A�A��B  BQ�B(�B�
B((�B/�
B7�B@(�BHz�BPz�BX  B_�Bh  Bp  Bx  B�(�B�  B�{B�{B�{B�{B�  B�  B��B�{B�{B�{B�(�B�  B�  B�{B�  B�  B��
B��
B��
B��
B�  B�  B�{B�{B�(�B�{B��
B��B�(�B�{C   C
=C  C  C
=C
  C��C
=C  C  C�C�HC�C��C��C
=C 
=C"
=C$
=C&  C(
=C*{C,�C.
=C0  C2
=C4  C6
=C8
=C:  C<  C>  C?��CB  CD  CF
=CH{CJ
=CL
=CN
=CO��CQ��CT
=CV
=CX  CZ  C[�C]��C`  Ca��Cd  Cf  Cg��Ci��Ck��Cn
=Co��Cq�HCs��Cv  Cw��Cy��C{��C~  C�
=C�C���C�C�  C�  C�C���C���C�C���C�C�
=C�  C�\C�
=C�  C���C�  C�  C�  C���C�  C�C�  C�
=C�  C�
=C�C�  C���C�  C�C���C�  C�C�C�C�C���C�  C���C���C���C�  C���C�
=C�\C���C�  C�  C�C�C�  C���C���C���C���C���C�  C���C�  C�C�  C���C���C�  C�C�
=C�
=C�  C�  C�C���C���C�  C�C�  C���C���C�  C�  C���C���C���C���C�C�C�  C���C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C���C�  C���C���C�  C�  C���C�C�C�  C���C���C�  C�  C�  C�  C���C���C�C�C���C���C�  C���C���D xRD �qD� D�qDz�D�RDxRD�qD��D  D� D  D� D  D� D�qD��D	D	�D
  D
}qD
�qD� D  D� D�D� D�qD}qD  Dz�D�qD}qD  D��D�qD}qD  Dz�D�qD��D�D� D�qD}qD  D� D�D}qD�qD� D�D� D�qD}qD�qD��D  D}qD  D��D  D}qD�qD � D!  D!� D"  D"��D#D#� D$  D$� D$�qD%}qD%�qD&� D&�qD'z�D(  D(� D)�D)�D*�D*� D+  D+��D,D,}qD-  D-�D.  D.� D/  D/z�D/��D0� D1�D1��D2  D2� D3  D3� D4  D4}qD4��D5z�D5�RD6z�D7  D7� D7�qD8� D9�D9�D:�D:z�D:��D;z�D;�RD<z�D<��D=z�D=�qD>}qD?  D?� D@  D@�DA  DA}qDA�qDBz�DB�qDC� DD�DD}qDD��DE}qDE�qDFz�DF�RDG}qDG�qDH}qDH�qDI}qDJ�DJ��DK�DK��DLDL� DL�qDM}qDN�DN��DO�DO}qDO��DP��DQ�DQ}qDR  DR��DS�DS� DT  DT�DU  DU}qDV  DVz�DV��DW� DX  DXz�DX�qDY�DZ�DZ}qDZ�qD[}qD[�qD\��D]�D]��D^D^��D_  D_� D`  D`� D`�qDa��Db�Db� Db��Dc}qDd  Dd}qDe  De��Df�Df� Dg  Dgz�Dg�qDh}qDi  Di� Dj  Dj� Dj�qDk� DlDl��Dm�Dmz�Dm�qDn��Do�Do� Dp  Dp� Dq  Dq��DrDr�DsDs� Dt  Dt�Du  Du}qDv  Dv��DwDw��Dw��Dx� DyDy��Dz  Dz� D{D{� D|  D|}qD}  D}� D~D~��D�D��D�  D�>�D�� D��HD���D�>�D�~�D�� D��D�AHD�}qD���D�  D�@ D�� D�� D�  D�@ D��HD���D���D�AHD��HD���D��qD�>�D��HD��HD�  D�>�D�~�D���D���D�@ D���D�D��D�AHD�� D��HD�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD���D�@ D�~�D�� D�  D�@ D��HD��HD�HD�AHD�~�D��qD���D�>�D�� D��qD���D�@ D�~�D���D�  D�AHD���D��HD���D�@ D��HD��HD�  D�=qD�}qD��qD��qD�=qD�}qD�� D�  D�@ D��HD�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D�� D���D�@ D��HD��HD��D�AHD�}qD���D�  D�@ D��HD�� D��qD�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�AHD��HD�� D�HD�B�D��HD��HD�HD�B�D���D�� D�  D�AHD��HD�D�HD�@ D�~�D�� D�HD�@ D�~�D�� D��D�AHD�� D���D�HD�AHD��HD��HD���D�@ D��HD�D�HD�@ D���D�D��D�@ D�|)D���D���D�>�D��HD�D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D��)D�>�D��HD��HD�  D�AHD���D��HD��qD�=qD�� D�� D�HD�AHD�� D�� D�  D�=qD�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D���D�AHD�� D�� D�HD�@ D�}qD��qD�  D�@ D�}qD��qD�  D�B�D��HD���D�  D�B�D��HD���D�  D�AHD�~�D�� D�HD�>�D D��HD���D�>�D�~�D�� D��D�AHDāHD�D�  D�>�Dŀ D�D��D�AHDƁHD��HD�HD�AHDǁHD�� D�  D�>�DȀ D��HD�  D�@ D�~�Dɾ�D��qD�>�Dʀ D�� D�HD�AHD�~�D˾�D�  D�AHD�~�D̾�D�  D�AHD́HD�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�HD�>�DЀ D�D�HD�AHDсHD��HD�  D�B�DҁHD�� D�HD�AHDӀ DӾ�D�  D�@ D�~�DԾ�D�HD�@ D�~�Dվ�D�HD�B�DցHD�� D�  D�>�D׀ D�� D���D�=qD�|)DؽqD���D�=qDـ Dپ�D���D�@ Dڀ D�D�HD�=qD�~�D��HD�HD�AHD�~�D�� D�HD�@ D݁HD�� D�  D�@ DށHD��HD�  D�B�D߁HD߾�D���D�>�D�� D�� D��qD�@ D� D�� D�HD�AHD�HD�� D��qD�@ D��D��HD�HD�@ D�HD�� D�  D�AHD�~�D徸D�HD�AHD�HD��HD�HD�>�D� D��HD�  D�@ D� D�� D�  D�@ D邏D��HD���D�@ D� D��HD��D�AHD� D뾸D���D�>�D� D��HD�HD�AHD� D�� D�  D�AHD�~�D�qD��qD�AHD��D�D�  D�>�D�}qD�D�  D�AHD�HD�D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�>�D�� D��HD�  D�=qD�~�D�� D���D�=qD�}qD���D�HD�>�D�~�D��HD�HD�AHD�� D���D��qD�@ D�u�?��?k�?���?�Q�?�
=?�@
=q@�R@5@G�@Tz�@n{@��\@��@�@�  @�=q@�z�@�p�@�ff@У�@�Q�@�G�@�=q@�z�@�(�A33A�A��A�A
=A(�A!�A&ffA+�A0  A5�A9��A>�RAC�
AHQ�AN{AQ�AVffAZ�HA`  Adz�Ai��An{Ar�\Aw�A}p�A���A��
A�ffA���A�33A�p�A�Q�A��\A��A�\)A��A�z�A�
=A���A�(�A�{A���A��HA��A�\)A���A�(�A�ffA�G�A��
A��RA�G�AÅA�{A���A˅A�A�Q�A��HA��A�\)A��A�z�A�
=AᙚA�(�A�RA�G�A��
A�ffA���A��
A�ffA�G�A��
A�ffB ��B{B\)B��B=qB�B��B
ffB�B��B=qB�B��B{B\)B��BB
=BQ�Bp�B�RBQ�B��B�HB (�B!��B"�HB$Q�B%��B&�RB(  B)G�B*�RB,  B-G�B.�\B0  B1G�B2�RB4  B5G�B6�RB8  B9�B:ffB;�B<��B>{B?\)B@��BB{BC\)BD��BE�BG33BHz�BIBJ�RBL  BMG�BN�\BO�
BQ�BRffBS�BT��BV=qBW\)BX��BYB[33B\Q�B]�B_33B`Q�BaBb�HBd(�BeG�Bf�\Bg�
BiG�Bj�\Bk�Bl��Bn=qBo\)Bp��Bq�Bs33BtQ�Bu��Bv�RBx  ByG�BzffB{�B|��B}�B
=B�(�B���B�\)B��B�z�B��B��B�Q�B��HB��B�(�B���B�\)B��B��\B��B�B�ffB��HB��B�(�B��RB�\)B�  B�z�B�
=B��B�=qB���B�\)B��B�ffB��B��B�Q�B��HB��B�(�B��RB�G�B��B�z�B��B��B�Q�B���B���B�(�B��RB�G�B��
B�ffB���B���B�(�B���B��B�B�Q�B��HB�p�B�  B���B�G�B��
B�z�B�
=B��B�Q�B���B���B�(�B��HB��B�{B���B�33B��
B�ffB���B��B�  B��\B�33B�B�ffB���B���B�=qB��RB�p�B�  B��RB�G�B��B��\B�33B��
B�ffB��HB��B�  B��\B��B��B�ffB�
=B��B�ffB�
=B��B�Q�B��HB�\)B�  Bď\B��B�B�ffB��HB�p�B�  BȸRB�\)B�  BʸRB��BˮB�(�B̸RB�p�B�{BθRB�p�B�  BЏ\B�
=BхB�{B���B�p�B�(�Bԣ�B�33BծB�ffB�
=B��
B�Q�B���B�p�B�{B��HBۙ�B�(�Bܣ�B�33B�B�z�B�G�B��B�ffB��HBᙚB�Q�B�
=B�B�{B�\B�G�B��B�RB�G�B�B�Q�B��HB�B�Q�B��B뙚B�=qB�RB�\)B�{B�RB�B�{B��\B�G�B�{B��B��B��
B���B�G�B��B�z�B�G�B��
B�Q�B�
=B��
B�z�B�
=B���B�ffB�33B���B�(�B�
=B��
C (�C ffC C(�C�CC{C\)CC(�Cz�C�RC{Cp�C�
C{CffCC(�Cp�C�C�C�C��C
=Cz�C�HC	�C	p�C	�
C
=qC
z�C
��C=qC��C�HC33C��C�C33C��C  CG�C�\C�C\)C��C�HCQ�C��C�
CG�C��C�HC=qC�C�C33C��C��C33C��C
=CG�C��C  CG�C�\C  CQ�C�C��CG�C�C  C=qC�C��C(�C��C�HC=qC��C��C=qC��C�
CG�C��C�HCQ�C�RC��CQ�C��C 
=C \)C ��C!{C!ffC!��C"�C"p�C"�C#(�C#z�C#�C$33C$�\C$��C%33C%��C%��C&33C&��C'  C'=qC'�RC'��C(Q�C(�RC(�C)Q�C)�C)�C*\)C*�C*��C+\)C+��C+�C,\)C,�C,�HC-(�C-\)C-�RC-�HC.{C.p�C.�C.�
C/
=C/33C/�C/��C/�HC0�C0G�C0��C0�RC1  C1G�C1\)C1�C1�HC2{C2\)C2p�C2��C2�C333C3\)C3��C3��C4  C4=qC4p�C4�C4��C5�C5=qC5p�C5�C5�
C6�C633C6�\C6��C6��C7�C7\)C7��C7�C8{C8(�C8z�C8��C8�HC9�C9G�C9�C9C:  C:(�C:p�C:��C:�C;{C;Q�C;�C;��C;��C<=qC<ffC<�C<�HC={C=\)C=�\C=C>  C>=qC>p�C>��C>�HC?�C?=qC?��C?�C@  C@(�C@p�C@��C@�HCA
=CAQ�CAz�CA�RCA�CB(�CB\)CB�CB�HCC
=CC33CC�CC�\CC��CD
=CD\)CD�CDCD��CE(�CEffCE�\CE�HCE��CF\)CFp�CF�RCF�CG�CGffCG�CG�HCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                ?�  @   @@  @�  @��R@��R@�p�A ��A�A   A*�HA?\)A`  A�  A�  A�  A��A��AϮA�  A�A��B  BQ�B(�B�
B((�B/�
B7�B@(�BHz�BPz�BX  B_�Bh  Bp  Bx  B�(�B�  B�{B�{B�{B�{B�  B�  B��B�{B�{B�{B�(�B�  B�  B�{B�  B�  B��
B��
B��
B��
B�  B�  B�{B�{B�(�B�{B��
B��B�(�B�{C   C
=C  C  C
=C
  C��C
=C  C  C�C�HC�C��C��C
=C 
=C"
=C$
=C&  C(
=C*{C,�C.
=C0  C2
=C4  C6
=C8
=C:  C<  C>  C?��CB  CD  CF
=CH{CJ
=CL
=CN
=CO��CQ��CT
=CV
=CX  CZ  C[�C]��C`  Ca��Cd  Cf  Cg��Ci��Ck��Cn
=Co��Cq�HCs��Cv  Cw��Cy��C{��C~  C�
=C�C���C�C�  C�  C�C���C���C�C���C�C�
=C�  C�\C�
=C�  C���C�  C�  C�  C���C�  C�C�  C�
=C�  C�
=C�C�  C���C�  C�C���C�  C�C�C�C�C���C�  C���C���C���C�  C���C�
=C�\C���C�  C�  C�C�C�  C���C���C���C���C���C�  C���C�  C�C�  C���C���C�  C�C�
=C�
=C�  C�  C�C���C���C�  C�C�  C���C���C�  C�  C���C���C���C���C�C�C�  C���C�C�  C���C���C���C���C�  C�  C�  C�C�  C���C���C���C���C�  C���C���C�  C�  C���C�C�C�  C���C���C�  C�  C�  C�  C���C���C�C�C���C���C�  C���C���D xRD �qD� D�qDz�D�RDxRD�qD��D  D� D  D� D  D� D�qD��D	D	�D
  D
}qD
�qD� D  D� D�D� D�qD}qD  Dz�D�qD}qD  D��D�qD}qD  Dz�D�qD��D�D� D�qD}qD  D� D�D}qD�qD� D�D� D�qD}qD�qD��D  D}qD  D��D  D}qD�qD � D!  D!� D"  D"��D#D#� D$  D$� D$�qD%}qD%�qD&� D&�qD'z�D(  D(� D)�D)�D*�D*� D+  D+��D,D,}qD-  D-�D.  D.� D/  D/z�D/��D0� D1�D1��D2  D2� D3  D3� D4  D4}qD4��D5z�D5�RD6z�D7  D7� D7�qD8� D9�D9�D:�D:z�D:��D;z�D;�RD<z�D<��D=z�D=�qD>}qD?  D?� D@  D@�DA  DA}qDA�qDBz�DB�qDC� DD�DD}qDD��DE}qDE�qDFz�DF�RDG}qDG�qDH}qDH�qDI}qDJ�DJ��DK�DK��DLDL� DL�qDM}qDN�DN��DO�DO}qDO��DP��DQ�DQ}qDR  DR��DS�DS� DT  DT�DU  DU}qDV  DVz�DV��DW� DX  DXz�DX�qDY�DZ�DZ}qDZ�qD[}qD[�qD\��D]�D]��D^D^��D_  D_� D`  D`� D`�qDa��Db�Db� Db��Dc}qDd  Dd}qDe  De��Df�Df� Dg  Dgz�Dg�qDh}qDi  Di� Dj  Dj� Dj�qDk� DlDl��Dm�Dmz�Dm�qDn��Do�Do� Dp  Dp� Dq  Dq��DrDr�DsDs� Dt  Dt�Du  Du}qDv  Dv��DwDw��Dw��Dx� DyDy��Dz  Dz� D{D{� D|  D|}qD}  D}� D~D~��D�D��D�  D�>�D�� D��HD���D�>�D�~�D�� D��D�AHD�}qD���D�  D�@ D�� D�� D�  D�@ D��HD���D���D�AHD��HD���D��qD�>�D��HD��HD�  D�>�D�~�D���D���D�@ D���D�D��D�AHD�� D��HD�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD�� D���D�  D�AHD�� D�� D�  D�@ D�~�D��qD���D�@ D�~�D�� D�  D�@ D��HD��HD�HD�AHD�~�D��qD���D�>�D�� D��qD���D�@ D�~�D���D�  D�AHD���D��HD���D�@ D��HD��HD�  D�=qD�}qD��qD��qD�=qD�}qD�� D�  D�@ D��HD�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D�� D���D�@ D��HD��HD��D�AHD�}qD���D�  D�@ D��HD�� D��qD�>�D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�AHD��HD�� D�HD�B�D��HD��HD�HD�B�D���D�� D�  D�AHD��HD�D�HD�@ D�~�D�� D�HD�@ D�~�D�� D��D�AHD�� D���D�HD�AHD��HD��HD���D�@ D��HD�D�HD�@ D���D�D��D�@ D�|)D���D���D�>�D��HD�D�  D�@ D�� D���D�  D�@ D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D��)D�>�D��HD��HD�  D�AHD���D��HD��qD�=qD�� D�� D�HD�AHD�� D�� D�  D�=qD�~�D�� D�  D�>�D�~�D�� D�HD�AHD�� D�� D���D�AHD�� D�� D�HD�@ D�}qD��qD�  D�@ D�}qD��qD�  D�B�D��HD���D�  D�B�D��HD���D�  D�AHD�~�D�� D�HD�>�D D��HD���D�>�D�~�D�� D��D�AHDāHD�D�  D�>�Dŀ D�D��D�AHDƁHD��HD�HD�AHDǁHD�� D�  D�>�DȀ D��HD�  D�@ D�~�Dɾ�D��qD�>�Dʀ D�� D�HD�AHD�~�D˾�D�  D�AHD�~�D̾�D�  D�AHD́HD�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�HD�>�DЀ D�D�HD�AHDсHD��HD�  D�B�DҁHD�� D�HD�AHDӀ DӾ�D�  D�@ D�~�DԾ�D�HD�@ D�~�Dվ�D�HD�B�DցHD�� D�  D�>�D׀ D�� D���D�=qD�|)DؽqD���D�=qDـ Dپ�D���D�@ Dڀ D�D�HD�=qD�~�D��HD�HD�AHD�~�D�� D�HD�@ D݁HD�� D�  D�@ DށHD��HD�  D�B�D߁HD߾�D���D�>�D�� D�� D��qD�@ D� D�� D�HD�AHD�HD�� D��qD�@ D��D��HD�HD�@ D�HD�� D�  D�AHD�~�D徸D�HD�AHD�HD��HD�HD�>�D� D��HD�  D�@ D� D�� D�  D�@ D邏D��HD���D�@ D� D��HD��D�AHD� D뾸D���D�>�D� D��HD�HD�AHD� D�� D�  D�AHD�~�D�qD��qD�AHD��D�D�  D�>�D�}qD�D�  D�AHD�HD�D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�HD��HD�  D�>�D�� D��HD�  D�=qD�~�D�� D���D�=qD�}qD���D�HD�>�D�~�D��HD�HD�AHD�� D���D��qD�@ G�O�?��?k�?���?�Q�?�
=?�@
=q@�R@5@G�@Tz�@n{@��\@��@�@�  @�=q@�z�@�p�@�ff@У�@�Q�@�G�@�=q@�z�@�(�A33A�A��A�A
=A(�A!�A&ffA+�A0  A5�A9��A>�RAC�
AHQ�AN{AQ�AVffAZ�HA`  Adz�Ai��An{Ar�\Aw�A}p�A���A��
A�ffA���A�33A�p�A�Q�A��\A��A�\)A��A�z�A�
=A���A�(�A�{A���A��HA��A�\)A���A�(�A�ffA�G�A��
A��RA�G�AÅA�{A���A˅A�A�Q�A��HA��A�\)A��A�z�A�
=AᙚA�(�A�RA�G�A��
A�ffA���A��
A�ffA�G�A��
A�ffB ��B{B\)B��B=qB�B��B
ffB�B��B=qB�B��B{B\)B��BB
=BQ�Bp�B�RBQ�B��B�HB (�B!��B"�HB$Q�B%��B&�RB(  B)G�B*�RB,  B-G�B.�\B0  B1G�B2�RB4  B5G�B6�RB8  B9�B:ffB;�B<��B>{B?\)B@��BB{BC\)BD��BE�BG33BHz�BIBJ�RBL  BMG�BN�\BO�
BQ�BRffBS�BT��BV=qBW\)BX��BYB[33B\Q�B]�B_33B`Q�BaBb�HBd(�BeG�Bf�\Bg�
BiG�Bj�\Bk�Bl��Bn=qBo\)Bp��Bq�Bs33BtQ�Bu��Bv�RBx  ByG�BzffB{�B|��B}�B
=B�(�B���B�\)B��B�z�B��B��B�Q�B��HB��B�(�B���B�\)B��B��\B��B�B�ffB��HB��B�(�B��RB�\)B�  B�z�B�
=B��B�=qB���B�\)B��B�ffB��B��B�Q�B��HB��B�(�B��RB�G�B��B�z�B��B��B�Q�B���B���B�(�B��RB�G�B��
B�ffB���B���B�(�B���B��B�B�Q�B��HB�p�B�  B���B�G�B��
B�z�B�
=B��B�Q�B���B���B�(�B��HB��B�{B���B�33B��
B�ffB���B��B�  B��\B�33B�B�ffB���B���B�=qB��RB�p�B�  B��RB�G�B��B��\B�33B��
B�ffB��HB��B�  B��\B��B��B�ffB�
=B��B�ffB�
=B��B�Q�B��HB�\)B�  Bď\B��B�B�ffB��HB�p�B�  BȸRB�\)B�  BʸRB��BˮB�(�B̸RB�p�B�{BθRB�p�B�  BЏ\B�
=BхB�{B���B�p�B�(�Bԣ�B�33BծB�ffB�
=B��
B�Q�B���B�p�B�{B��HBۙ�B�(�Bܣ�B�33B�B�z�B�G�B��B�ffB��HBᙚB�Q�B�
=B�B�{B�\B�G�B��B�RB�G�B�B�Q�B��HB�B�Q�B��B뙚B�=qB�RB�\)B�{B�RB�B�{B��\B�G�B�{B��B��B��
B���B�G�B��B�z�B�G�B��
B�Q�B�
=B��
B�z�B�
=B���B�ffB�33B���B�(�B�
=B��
C (�C ffC C(�C�CC{C\)CC(�Cz�C�RC{Cp�C�
C{CffCC(�Cp�C�C�C�C��C
=Cz�C�HC	�C	p�C	�
C
=qC
z�C
��C=qC��C�HC33C��C�C33C��C  CG�C�\C�C\)C��C�HCQ�C��C�
CG�C��C�HC=qC�C�C33C��C��C33C��C
=CG�C��C  CG�C�\C  CQ�C�C��CG�C�C  C=qC�C��C(�C��C�HC=qC��C��C=qC��C�
CG�C��C�HCQ�C�RC��CQ�C��C 
=C \)C ��C!{C!ffC!��C"�C"p�C"�C#(�C#z�C#�C$33C$�\C$��C%33C%��C%��C&33C&��C'  C'=qC'�RC'��C(Q�C(�RC(�C)Q�C)�C)�C*\)C*�C*��C+\)C+��C+�C,\)C,�C,�HC-(�C-\)C-�RC-�HC.{C.p�C.�C.�
C/
=C/33C/�C/��C/�HC0�C0G�C0��C0�RC1  C1G�C1\)C1�C1�HC2{C2\)C2p�C2��C2�C333C3\)C3��C3��C4  C4=qC4p�C4�C4��C5�C5=qC5p�C5�C5�
C6�C633C6�\C6��C6��C7�C7\)C7��C7�C8{C8(�C8z�C8��C8�HC9�C9G�C9�C9C:  C:(�C:p�C:��C:�C;{C;Q�C;�C;��C;��C<=qC<ffC<�C<�HC={C=\)C=�\C=C>  C>=qC>p�C>��C>�HC?�C?=qC?��C?�C@  C@(�C@p�C@��C@�HCA
=CAQ�CAz�CA�RCA�CB(�CB\)CB�CB�HCC
=CC33CC�CC�\CC��CD
=CD\)CD�CDCD��CE(�CEffCE�\CE�HCE��CF\)CFp�CF�RCF�CG�CGffCG�CG�HCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�33A�5?A�5?A�33A�5?A�7LA�7LA�7LA�5?A�33A�33A�1'A�/A�1'A�/A�1'A�33A�1'A�1'A�;dA�7LA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�C�A�E�A�C�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�9XA�9XA�9XA�7LA�33A�(�A� �A��A�1A��A��HA���A���A�/A��A�JA���A�VA�A�ĜA�  A��RA��!A��^A�VA��HA�M�A�ȴA��wA�A���A�jA� �A��FA��#A���A�`BA�33A��A�\)A�%A��PA��;A�hsA�+A��wA�7LA��A���A�+A��-A��-A��A�|�A� �A��A~��Az��Au��At�At�uAt9XArJApQ�Ao&�An$�AmdZAl��Al�Ak��Ak�hAkS�Aj�Aj��AjVAi�-Ai�Ah(�Af1'Aa��A_t�A^-A]A\�\A\I�A\  A[hsAZ�`AZffAY�AYp�AX��AV5?AU;dAT��ATr�ATAQ��AN��AM�AK�
AKoAJȴAJJAIdZAH^5AH  AGXAF�AF�AES�AD�jAC�7AB�\AB{AA��AAC�A@~�A?�A>��A=�
A=hsA<�`A<9XA:�A9��A9��A9hsA9?}A8��A8�!A8��A8A�A81A8{A7�TA6JA5;dA4�`A4��A4I�A3O�A0��A/��A/�A.�9A.E�A.A-A-t�A-K�A,�/A,Q�A+XA*�`A)�A)p�A(�RA&��A%��A$��A${A"I�A ��A A�7A��A��A�^A/A5?A7LA�jA�#A\)A��AĜAbNA1AS�AƨAVA^5AƨA\)A�AE�AG�A�A��A��AffA^5AVA5?A�AE�A�FA
��A	�
A	�A��A&�Az�A��Ar�A1AƨA�jA�wA\)AA �A �A E�A -A b@�C�@��@��@��#@�x�@�1'@�;d@�~�@�S�@�9@�ff@�  @�@陚@�V@�z�@睲@�`B@���@��@�Z@ާ�@�z�@�$�@�V@�A�@��;@�K�@և+@��T@Ԭ@�t�@���@�~�@ѡ�@�%@�I�@�t�@�=q@���@���@�ƨ@�S�@�"�@ɩ�@�\)@Ɨ�@��@ź^@ēu@�+@°!@�$�@�X@�/@��F@�n�@��/@�1@�O�@���@��@���@�9X@�dZ@���@���@�I�@��@�t�@�S�@�@�n�@��^@��@�I�@�
=@�x�@�O�@�G�@���@�1'@���@�1'@�;d@�@��y@��\@�ff@�-@���@���@�1'@��R@��@��@��@�Ĝ@��D@�1'@��
@��@�V@�Ĝ@���@�A�@��;@�K�@�o@���@�v�@�$�@���@���@���@�
=@��R@�n�@�5?@�@��@���@���@��w@��P@�K�@�+@���@�ff@�{@���@��#@��-@���@�x�@�?}@�Ĝ@�Z@��@��
@�\)@�
=@���@�n�@���@�&�@��/@���@�Z@�b@�t�@��!@�n�@���@�&�@�%@���@��@�z�@�Q�@���@��@�o@�@���@�~�@�v�@�^5@�{@���@��@�Ĝ@�Q�@�(�@�@�@K�@~��@~�@~��@~V@~@~@}��@}p�@}`B@}/@|�@|�j@|z�@|j@|9X@{��@{��@{dZ@z�H@zM�@y�^@y�@xr�@xb@w;d@v�+@v5?@u�@uO�@t�/@t��@tz�@t�@s�
@s�@s"�@r��@r��@rn�@r�@q7L@p��@p��@p�9@p�@pQ�@p1'@p  @o�w@o�@o|�@n��@n�+@nV@n5?@n{@m@mO�@l��@l�@l�@l�j@lj@k�@j��@j��@j�@iX@hĜ@hQ�@hb@g��@g\)@g�@f��@f�@f��@e��@d��@d�@d(�@c�
@c��@ct�@cC�@co@b��@b�\@a��@aX@`A�@`b@_�w@_+@_
=@^�y@^ȴ@^�+@^ff@^E�@^$�@^@]�-@]p�@]`B@]O�@]�@\�/@\�@\j@\Z@\9X@\1@[ƨ@[dZ@[C�@[C�@[C�@Z��@Z^5@ZM�@Z-@ZJ@Y�#@Y�@X�@X  @W��@W�@W|�@Wl�@W+@V��@V�@V��@V�+@Vff@VV@VV@VV@VV@V$�@U�T@U/@Tj@S�
@S��@S33@S@R��@Rn�@Q�#@Q��@QX@PĜ@PQ�@O�w@O�P@Ol�@O+@N��@Nȴ@N��@M�@M�@M/@L��@L�@K��@K��@KS�@Ko@J��@Jn�@J^5@JM�@I��@I��@IX@I�@HĜ@Hr�@Hb@G�@G�w@GK�@G
=@F�+@FV@F@E��@Ep�@E�@D�/@D�@Dz�@DZ@D(�@C��@C�
@C��@C��@C��@C��@CdZ@CS�@Bn�@A�7@Ax�@Ax�@Ax�@AG�@A&�@A�@@��@@Q�@?�w@?+@?
=@>�y@>@=`B@=�@=V@<��@<�@<��@<9X@;��@;�@;dZ@:�@:=q@9�@9�^@9��@9x�@9hs@9x�@9x�@9G�@9�@8�`@8�9@8Q�@8 �@8 �@8b@7�;@7�P@7;d@6�y@6��@6@5�T@5@5�h@5�h@5?}@4��@4��@4I�@3�m@3�m@3��@3�@3t�@3dZ@3S�@3C�@3"�@3o@3@2�@2��@2�\@2n�@2^5@2M�@2-@1��@1&�@1%@0��@0��@0r�@0Q�@0A�@0  @/�;@/��@/�w@/��@/\)@/�@.�y@.�@.�R@.��@.ff@.@-��@-�-@-�h@-�@-p�@-`B@-/@,��@,9X@+�m@+��@+�@+S�@+S�@+C�@+33@+"�@+@*�H@*�\@*-@*�@)��@)�#@)�^@)��@)X@)7L@(��@(Ĝ@(Q�@(b@'��@'��@'l�@'�@&��@&�y@&�@&�@&ȴ@&�R@&��@&v�@&5?@&@%�T@%��@%�@$��@$��@%V@%V@$��@$�D@$z�@$1@#�
@#dZ@#@"��@"�\@"n�@"M�@"�@!�@!��@!�^@!��@!��@!��@!��@!��@!�7@!hs@!7L@!�@!�@!%@!%@ ��@ r�@  �@   @|�@K�@+@�y@��@��@�+@v�@V@E�@�h@O�@?}@?}@/@V@�/@�j@�D@Z@�@�m@��@t�@C�@@�H@��@~�@^5@M�@-@�@��@��@��@G�@&�@��@Ĝ@�u@�@r�@A�@b@�@��@�w@|�@\)@��@�+@V@5?@$�@@�T@�@�/@��@�j@z�@9X@1@�m@ƨ@��@dZ@"�@�H@��@^5@-@�@��@�^@�7@hs@X@X@7L@�@��@Ĝ@�@bN@A�@A�@1'@�@�w@�@�@�P@l�@l�@\)@+@�y@ȴ@�R@��@ff@{@{@@�T@�@�@��@Z@1@1@ƨ@��@C�@"�@@
��@
�\@
=q@	�@	�^@	��@	G�@	�@�`@�9@�u@A�@�;@�;@��@��@�;@�;@��@�w@�@�@��@|�@l�@\)@;d@+@��@ȴ@��@v�@V@E�@5?@$�@{@@�T@�h@�@��@�j@��@�D@�D@9X@(�@�@�@�@��@�
@�F@dZ@�!@�\@�\@�\@n�@�@J@J@��@�@�@�@��@X@%@ �`A�1'A�5?A�33A�5?A�33A�1'A�1'A�1'A�33A�33A�5?A�33A�33A�33A�1'A�33A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�7LA�7LA�7LA�7LA�5?A�33A�5?A�5?A�33A�33A�1'A�1'A�5?A�7LA�33A�33A�33A�1'A�1'A�1'A�33A�1'A�1'A�1'A�1'A�1'A�-A�+A�-A�-A�-A�-A�1'A�33A�33A�1'A�/A�+A�/A�/A�-A�/A�1'A�33A�33A�33A�/A�1'A�1'A�33A�/A�1'A�/A�1'A�1'A�5?A�1'A�33A�1'A�33A�33A�1'A�33A�33A�1'A�33A�1'A�1'A�1'A�1'A�7LA�7LA�7LA�;dA�;dA�;dA�;dA�7LA�9XA�7LA�33A�33A�5?A�5?A�5?A�9XA�;dA�=qA�;dA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�G�A�G�A�A�A�?}A�C�A�E�A�C�A�E�A�C�A�C�A�C�A�E�A�C�A�?}A�A�A�A�A�=qA�;dA�;dA�C�A�G�A�C�A�A�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�5?A�7LA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�7LA�9XA�7LA�5?A�5?A�5?A�33A�33A�33A�1'A�/A�-A�+A�$�A� �A��A� �A� �A�"�A� �A��A��A��A��A��A��A��A��A��A�{A�oA�VA�VA�
=A�A�A���A���A���A���A��A��A��A��yA��`A��`A��TA��HA��;A��#A��
A���A���A���A��wA���A�C�A��HA���A�ĜA���A�XA�^5A�Q�A�I�A�I�A�(�A�"�A�$�A�"�A��A��A��A�oA�oA�{A�oA�1A�
=A�
=A�
=A�VA�VA�bA���A���A��+A�p�A��+A�VA�K�A���A��A�S�A��/A���A�ƨA���A��A�=qA��A���A�t�A�K�A��!A�"�A���A��RA�=qA���A���A�9XA�A���A��`A��wA���A�|�A�=qA��A�x�A�^5A��/A���A���A��^A��A���A���A��A�ffA�/A��A���A�&�A��A�ĜA���A�p�A�O�A�5?A��TA��A��A� �A��#A�ffA�1'A��A��uA�l�A�G�A�"�A�%A��;A��+A�;dA���A�n�A�S�A�;dA�$�A�{A��`A���A�t�A�7LA�JA�A��yA��A��PA�l�A�ffA�XA�E�A�+A��A�33A��#A��wA���A�r�A�oA���A���A�`BA�S�A�9XA���A� �A���A��A��`A���A��A�9XA���A��yA��`A���A�ƨA�ƨA���A��!A���A���A��hA�v�A�r�A�jA�^5A�VA�Q�A�O�A�K�A�A�A�+A�(�A�$�A� �A���A�ȴA��+A�jA��A���A��A���A�XA�1A���A��A�"�A�{A�A��A��HA��/A��RA��-A���A�hsA�XA�=qA� �A�bA�1A��/A���A�M�A���A�I�A�oA�n�A��A��A�t�A�^5A��A�JA���A��mA��`A��;A�ȴA���A��\A�v�A�1'A�7LA�$�A�33A�/A�&�A���A��A��#A���A���A���A�ĜA���A���A��7A�p�A�M�A�A�A�-A��A�bA���A��A���A��wA���A�|�A�l�A�1'A��A�A���A�ffA�G�A��A���A��mA��#A���A���A��jA��9A���A���A���A��7A�z�A�t�A�l�A�bNA�ZA�K�A�=qA�7LA�/A�&�A�$�A��A�VA�1A��A�AƨA�FA��A��A�PA�Al�AS�A/A�AoA~��A~�A~��A~v�A~VA~=qA~  A}�A}�A|ȴA|ZA{�PAz�`Az1Ax�jAw�Av�jAvn�Av5?Av�Au�Au��Au��Au�7AuhsAuK�Au�At��At�`At�HAt��At��AtĜAt��At�!At�!At��At��At�DAt�DAt~�At~�Atr�Atr�AtffAtbNAtZAtQ�AtA�At �As��AsƨAs��AsdZArĜAr5?Aq�FAqt�Aqx�Aq`BAq&�Ap�ApȴApn�Ap^5Ap^5ApVApE�Ap �Ao�#Ao�wAo�-Ao��Ao�AoS�Ao;dAn��An�jAn��An��Anv�An^5An1'An-AnbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                A�1'A�33A�5?A�5?A�33A�5?A�7LA�7LA�7LA�5?A�33A�33A�1'A�/A�1'A�/A�1'A�33A�1'A�1'A�;dA�7LA�9XA�;dA�=qA�=qA�=qA�?}A�?}A�A�A�C�A�E�A�C�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�9XA�9XA�9XA�7LA�33A�(�A� �A��A�1A��A��HA���A���A�/A��A�JA���A�VA�A�ĜA�  A��RA��!A��^A�VA��HA�M�A�ȴA��wA�A���A�jA� �A��FA��#A���A�`BA�33A��A�\)A�%A��PA��;A�hsA�+A��wA�7LA��A���A�+A��-A��-A��A�|�A� �A��A~��Az��Au��At�At�uAt9XArJApQ�Ao&�An$�AmdZAl��Al�Ak��Ak�hAkS�Aj�Aj��AjVAi�-Ai�Ah(�Af1'Aa��A_t�A^-A]A\�\A\I�A\  A[hsAZ�`AZffAY�AYp�AX��AV5?AU;dAT��ATr�ATAQ��AN��AM�AK�
AKoAJȴAJJAIdZAH^5AH  AGXAF�AF�AES�AD�jAC�7AB�\AB{AA��AAC�A@~�A?�A>��A=�
A=hsA<�`A<9XA:�A9��A9��A9hsA9?}A8��A8�!A8��A8A�A81A8{A7�TA6JA5;dA4�`A4��A4I�A3O�A0��A/��A/�A.�9A.E�A.A-A-t�A-K�A,�/A,Q�A+XA*�`A)�A)p�A(�RA&��A%��A$��A${A"I�A ��A A�7A��A��A�^A/A5?A7LA�jA�#A\)A��AĜAbNA1AS�AƨAVA^5AƨA\)A�AE�AG�A�A��A��AffA^5AVA5?A�AE�A�FA
��A	�
A	�A��A&�Az�A��Ar�A1AƨA�jA�wA\)AA �A �A E�A -A b@�C�@��@��@��#@�x�@�1'@�;d@�~�@�S�@�9@�ff@�  @�@陚@�V@�z�@睲@�`B@���@��@�Z@ާ�@�z�@�$�@�V@�A�@��;@�K�@և+@��T@Ԭ@�t�@���@�~�@ѡ�@�%@�I�@�t�@�=q@���@���@�ƨ@�S�@�"�@ɩ�@�\)@Ɨ�@��@ź^@ēu@�+@°!@�$�@�X@�/@��F@�n�@��/@�1@�O�@���@��@���@�9X@�dZ@���@���@�I�@��@�t�@�S�@�@�n�@��^@��@�I�@�
=@�x�@�O�@�G�@���@�1'@���@�1'@�;d@�@��y@��\@�ff@�-@���@���@�1'@��R@��@��@��@�Ĝ@��D@�1'@��
@��@�V@�Ĝ@���@�A�@��;@�K�@�o@���@�v�@�$�@���@���@���@�
=@��R@�n�@�5?@�@��@���@���@��w@��P@�K�@�+@���@�ff@�{@���@��#@��-@���@�x�@�?}@�Ĝ@�Z@��@��
@�\)@�
=@���@�n�@���@�&�@��/@���@�Z@�b@�t�@��!@�n�@���@�&�@�%@���@��@�z�@�Q�@���@��@�o@�@���@�~�@�v�@�^5@�{@���@��@�Ĝ@�Q�@�(�@�@�@K�@~��@~�@~��@~V@~@~@}��@}p�@}`B@}/@|�@|�j@|z�@|j@|9X@{��@{��@{dZ@z�H@zM�@y�^@y�@xr�@xb@w;d@v�+@v5?@u�@uO�@t�/@t��@tz�@t�@s�
@s�@s"�@r��@r��@rn�@r�@q7L@p��@p��@p�9@p�@pQ�@p1'@p  @o�w@o�@o|�@n��@n�+@nV@n5?@n{@m@mO�@l��@l�@l�@l�j@lj@k�@j��@j��@j�@iX@hĜ@hQ�@hb@g��@g\)@g�@f��@f�@f��@e��@d��@d�@d(�@c�
@c��@ct�@cC�@co@b��@b�\@a��@aX@`A�@`b@_�w@_+@_
=@^�y@^ȴ@^�+@^ff@^E�@^$�@^@]�-@]p�@]`B@]O�@]�@\�/@\�@\j@\Z@\9X@\1@[ƨ@[dZ@[C�@[C�@[C�@Z��@Z^5@ZM�@Z-@ZJ@Y�#@Y�@X�@X  @W��@W�@W|�@Wl�@W+@V��@V�@V��@V�+@Vff@VV@VV@VV@VV@V$�@U�T@U/@Tj@S�
@S��@S33@S@R��@Rn�@Q�#@Q��@QX@PĜ@PQ�@O�w@O�P@Ol�@O+@N��@Nȴ@N��@M�@M�@M/@L��@L�@K��@K��@KS�@Ko@J��@Jn�@J^5@JM�@I��@I��@IX@I�@HĜ@Hr�@Hb@G�@G�w@GK�@G
=@F�+@FV@F@E��@Ep�@E�@D�/@D�@Dz�@DZ@D(�@C��@C�
@C��@C��@C��@C��@CdZ@CS�@Bn�@A�7@Ax�@Ax�@Ax�@AG�@A&�@A�@@��@@Q�@?�w@?+@?
=@>�y@>@=`B@=�@=V@<��@<�@<��@<9X@;��@;�@;dZ@:�@:=q@9�@9�^@9��@9x�@9hs@9x�@9x�@9G�@9�@8�`@8�9@8Q�@8 �@8 �@8b@7�;@7�P@7;d@6�y@6��@6@5�T@5@5�h@5�h@5?}@4��@4��@4I�@3�m@3�m@3��@3�@3t�@3dZ@3S�@3C�@3"�@3o@3@2�@2��@2�\@2n�@2^5@2M�@2-@1��@1&�@1%@0��@0��@0r�@0Q�@0A�@0  @/�;@/��@/�w@/��@/\)@/�@.�y@.�@.�R@.��@.ff@.@-��@-�-@-�h@-�@-p�@-`B@-/@,��@,9X@+�m@+��@+�@+S�@+S�@+C�@+33@+"�@+@*�H@*�\@*-@*�@)��@)�#@)�^@)��@)X@)7L@(��@(Ĝ@(Q�@(b@'��@'��@'l�@'�@&��@&�y@&�@&�@&ȴ@&�R@&��@&v�@&5?@&@%�T@%��@%�@$��@$��@%V@%V@$��@$�D@$z�@$1@#�
@#dZ@#@"��@"�\@"n�@"M�@"�@!�@!��@!�^@!��@!��@!��@!��@!��@!�7@!hs@!7L@!�@!�@!%@!%@ ��@ r�@  �@   @|�@K�@+@�y@��@��@�+@v�@V@E�@�h@O�@?}@?}@/@V@�/@�j@�D@Z@�@�m@��@t�@C�@@�H@��@~�@^5@M�@-@�@��@��@��@G�@&�@��@Ĝ@�u@�@r�@A�@b@�@��@�w@|�@\)@��@�+@V@5?@$�@@�T@�@�/@��@�j@z�@9X@1@�m@ƨ@��@dZ@"�@�H@��@^5@-@�@��@�^@�7@hs@X@X@7L@�@��@Ĝ@�@bN@A�@A�@1'@�@�w@�@�@�P@l�@l�@\)@+@�y@ȴ@�R@��@ff@{@{@@�T@�@�@��@Z@1@1@ƨ@��@C�@"�@@
��@
�\@
=q@	�@	�^@	��@	G�@	�@�`@�9@�u@A�@�;@�;@��@��@�;@�;@��@�w@�@�@��@|�@l�@\)@;d@+@��@ȴ@��@v�@V@E�@5?@$�@{@@�T@�h@�@��@�j@��@�D@�D@9X@(�@�@�@�@��@�
@�F@dZ@�!@�\@�\@�\@n�@�@J@J@��@�@�@�@��@X@%G�O�A�1'A�5?A�33A�5?A�33A�1'A�1'A�1'A�33A�33A�5?A�33A�33A�33A�1'A�33A�7LA�7LA�7LA�7LA�7LA�9XA�7LA�7LA�7LA�7LA�7LA�5?A�33A�5?A�5?A�33A�33A�1'A�1'A�5?A�7LA�33A�33A�33A�1'A�1'A�1'A�33A�1'A�1'A�1'A�1'A�1'A�-A�+A�-A�-A�-A�-A�1'A�33A�33A�1'A�/A�+A�/A�/A�-A�/A�1'A�33A�33A�33A�/A�1'A�1'A�33A�/A�1'A�/A�1'A�1'A�5?A�1'A�33A�1'A�33A�33A�1'A�33A�33A�1'A�33A�1'A�1'A�1'A�1'A�7LA�7LA�7LA�;dA�;dA�;dA�;dA�7LA�9XA�7LA�33A�33A�5?A�5?A�5?A�9XA�;dA�=qA�;dA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�;dA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�9XA�;dA�;dA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�A�A�A�A�C�A�C�A�C�A�C�A�E�A�E�A�E�A�G�A�G�A�A�A�?}A�C�A�E�A�C�A�E�A�C�A�C�A�C�A�E�A�C�A�?}A�A�A�A�A�=qA�;dA�;dA�C�A�G�A�C�A�A�A�C�A�A�A�C�A�A�A�A�A�A�A�C�A�C�A�C�A�C�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�?}A�?}A�=qA�=qA�=qA�=qA�;dA�=qA�=qA�=qA�=qA�=qA�;dA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�5?A�7LA�9XA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�7LA�9XA�7LA�5?A�5?A�5?A�33A�33A�33A�1'A�/A�-A�+A�$�A� �A��A� �A� �A�"�A� �A��A��A��A��A��A��A��A��A��A�{A�oA�VA�VA�
=A�A�A���A���A���A���A��A��A��A��yA��`A��`A��TA��HA��;A��#A��
A���A���A���A��wA���A�C�A��HA���A�ĜA���A�XA�^5A�Q�A�I�A�I�A�(�A�"�A�$�A�"�A��A��A��A�oA�oA�{A�oA�1A�
=A�
=A�
=A�VA�VA�bA���A���A��+A�p�A��+A�VA�K�A���A��A�S�A��/A���A�ƨA���A��A�=qA��A���A�t�A�K�A��!A�"�A���A��RA�=qA���A���A�9XA�A���A��`A��wA���A�|�A�=qA��A�x�A�^5A��/A���A���A��^A��A���A���A��A�ffA�/A��A���A�&�A��A�ĜA���A�p�A�O�A�5?A��TA��A��A� �A��#A�ffA�1'A��A��uA�l�A�G�A�"�A�%A��;A��+A�;dA���A�n�A�S�A�;dA�$�A�{A��`A���A�t�A�7LA�JA�A��yA��A��PA�l�A�ffA�XA�E�A�+A��A�33A��#A��wA���A�r�A�oA���A���A�`BA�S�A�9XA���A� �A���A��A��`A���A��A�9XA���A��yA��`A���A�ƨA�ƨA���A��!A���A���A��hA�v�A�r�A�jA�^5A�VA�Q�A�O�A�K�A�A�A�+A�(�A�$�A� �A���A�ȴA��+A�jA��A���A��A���A�XA�1A���A��A�"�A�{A�A��A��HA��/A��RA��-A���A�hsA�XA�=qA� �A�bA�1A��/A���A�M�A���A�I�A�oA�n�A��A��A�t�A�^5A��A�JA���A��mA��`A��;A�ȴA���A��\A�v�A�1'A�7LA�$�A�33A�/A�&�A���A��A��#A���A���A���A�ĜA���A���A��7A�p�A�M�A�A�A�-A��A�bA���A��A���A��wA���A�|�A�l�A�1'A��A�A���A�ffA�G�A��A���A��mA��#A���A���A��jA��9A���A���A���A��7A�z�A�t�A�l�A�bNA�ZA�K�A�=qA�7LA�/A�&�A�$�A��A�VA�1A��A�AƨA�FA��A��A�PA�Al�AS�A/A�AoA~��A~�A~��A~v�A~VA~=qA~  A}�A}�A|ȴA|ZA{�PAz�`Az1Ax�jAw�Av�jAvn�Av5?Av�Au�Au��Au��Au�7AuhsAuK�Au�At��At�`At�HAt��At��AtĜAt��At�!At�!At��At��At�DAt�DAt~�At~�Atr�Atr�AtffAtbNAtZAtQ�AtA�At �As��AsƨAs��AsdZArĜAr5?Aq�FAqt�Aqx�Aq`BAq&�Ap�ApȴApn�Ap^5Ap^5ApVApE�Ap �Ao�#Ao�wAo�-Ao��Ao�AoS�Ao;dAn��An�jAn��An��Anv�An^5An1'An-AnbG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�vB��B�B�vB�vB�B�AB�vB�;B��B�vB�|B�B�;B��B�B��B�B�MB�B��B�B�vB�B�;B�B�iB�B��B�;B� B�B��B��B��B�B�QB��B�B�B�sB�B�2B��B��B�HB�5B��B��BҽB�B�B�B��B�YB{B\]BD3B�BJB�B�B�2B�$B�=B�;Bk�BZ�BPB6FB-�B��B�B��B�&B��B�B�aB��B�B�B��Bh�Ba�BW�BLdBHKBA�B:*B1�B#:BCBSB�B�B��B�yBϫB��BǮB�wB��B�0B�:B��B��B�SB��B��B�(B��B��B��B�B|�Bu�Bl"BS&BC-B9�B2�B.IB,=B*eB'�B#�B!-B�B7B�B"B�B;B
��B
�8B
�|B
�B
��B
�jB
�gB
��B
�B
��B
�B
�nB
��B
��B
��B
�nB
�!B
��B
��B
�VB
�JB
��B
�fB
~�B
yrB
s�B
r�B
n�B
l"B
l�B
dZB
cTB
bNB
aHB
aB
_�B
^�B
_�B
e`B
d&B
b�B
^B
YKB
WsB
T�B
QB
P�B
FtB
?B
:^B
9�B
6zB
49B
2aB
0�B
.�B
,B
'�B
!�B
�B
�B
uB
hB
�B
oB
 �B	��B	�JB	��B	�;B	��B	�B	�B	�B	�B	�B	�B	�WB	ٴB	֡B	��B	��B	ҽB	�vB	��B	�0B	�B	�aB	��B	�qB	��B	��B	��B	�?B	��B	�B	�CB	�qB	�6B	�6B	�=B	�FB	�FB	��B	��B	�CB	��B	��B	��B	��B	� B	�"B	��B	�"B	��B	�1B	��B	�YB	��B	��B	�B	��B	�B	��B	�B	~�B	}�B	~(B	zDB	v�B	yrB	t�B	u�B	qvB	o5B	o�B	m]B	k�B	kB	lWB	k�B	f�B	e,B	g8B	d&B	g8B	d�B	e�B	dZB	c�B	d&B	b�B	d�B	c�B	bNB	b�B	b�B	a�B	a|B	bB	bNB	`�B	a�B	bB	`�B	_;B	c�B	cTB	c�B	c B	b�B	c�B	d�B	d�B	e,B	d�B	c�B	h
B	gmB	jKB	h�B	ncB	ncB	poB	rGB	q�B	rGB	q�B	v+B	v`B	v�B	x8B	xB	xlB	y�B	z�B	|PB	|�B	�iB	�SB	�SB	�MB	��B	��B	�VB	�oB	��B	�B	��B	�$B	�YB	�YB	�_B	��B	��B	��B	�0B	�IB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�[B	��B	�B	�zB	�B	˒B	̘B	�<B	�[B	�yB	�]B	��B	�;B	��B	�B	�8B	�B	�cB	�5B	��B	�B	��B	�B	��B	�DB	��B	�B	��B	��B	��B	��B
�B
�B
�B
�B
DB
�B
�B
bB
$B
B
B
IB
!B
!-B
&LB
,B
-CB
0�B
7LB
7�B
8B
:�B
;�B
=B
?�B
C�B
FtB
F�B
IB
K)B
K�B
K�B
NB
RTB
V�B
YKB
]dB
^5B
`B
`�B
b�B
c�B
d&B
d�B
f2B
gmB
gmB
iyB
jB
jB
kB
lWB
l�B
m�B
m�B
ncB
oiB
q�B
q�B
tTB
w2B
yrB
{�B
}�B
� B
�GB
�_B
�1B
�lB
�B
��B
�bB
��B
�B
�B
�{B
�SB
�+B
��B
��B
�qB
��B
�B
��B
�B
��B
��B
��B
��B
��B
�0B
��B
��B
�[B
��B
�nB
�?B
�?B
�B
��B
�B
�B
�$B
��B
��B
��B
��B
�'B
�aB
�9B
�B
ɆB
�dB
�BB
�}B
�NB
�NB
� B
��B
��B
�sB
��B
�QB
�#B
�)B
��B
��B
�jB
�;B
�B
�B
�sB
��B
��B
�"B
�"B
�B
�]B
�cB
�5B
�B
��B
��B
�B
�B
�|B
�MB
�B
�ZB
��B
��B
�2B
�fB
�B
��B
��B
�xB
��B
�DB
��B
��B
��B
��B 4BB�B�B�B�B�B�B�B1B�B	�B
rBBxB�BB
�B
�BxB�B\B�BoB�BuB�BB�BSB�B�B�B�B�B�B�B!B�B�B 'B!�B"�B#B#nB#�B%zB%�B&�B'RB'�B(XB'�B'�B(�B)_B)�B*eB+kB,B-B,�B-CB.}B.�B0!B/�B0UB0�B1�B2�B2�B2�B2�B2�B2�B2�B2�B2�B2�B1�B1�B1�B1�B5tB6FB6FB6FB6FB6�B6�B6�B7B8B9�B:�B;0B:�B=�B?B?}B?}B?}B?HB?B@�BA�BA�BAUBB�BD3BD�BEBEmBE�BFBE�BE�BF?BF�BGBGEBG�BG�BG�BG�BHBH�BIRBI�BJ�BK�BK�BL0BL�BLdBMBM�BN�BPHBQNBQ�BR�BS&BS&BR�BS&BS[BS�BS�BS�BS�BT,BS�BS�BS�BS&BS[BT�BU�BU�BVmBVmBW
BWsBW�BXEBXyBXyBXyBXyBYBYBZ�BZ�BZ�BZ�B[�B\]B\�B\�B\�B\�B\�B\�B\�B]�B^�B^�B^�B_;B_pB_�B_pB_�B_pB_pB_�B`vBa|Bb�Bb�Bb�Bc BcTBcTBd&Bd�Be�BffBf�BgmBgmBg�Bh
Bh>Bh
Bg�Bg�Bg�Bg�Bh>BhsBh�BiBiyBi�BkBj�Bj�Bj�BjBkBkBkBl"Bk�Bl�Bm�Bn/BncBn�Bn�Bo5BoiBo�Bo�BoiBo�BpBo�Bo�Bo�Bo�BpoBp�Bp�Bp�Bp�Bp�Bq�BrGBrGBs�Bs�Bs�BtTBtTBtBtTBs�BtBs�Bu�Bu�Bv�Bv�Bw2BwfBwfBwfBx8BxlBxlBx�Bx�By	By>By�ByrBy>By�BzBzBzBzBzDBz�Bz�B{B{�B|B|PB|�B|PB|PB|PB|�B}"B}"B|�B}"B}VB}�B.B�B�B�B�B�B�iB�B��B��B�;B�;B�oB�oB��B��B�B�AB��B��B�{B�{B�{B��B�B�B�B�B�B�MB�MB��B��B�B�SB��B��B��B�YB��B��B�YB�YB�YB�YB�YB��B�+B��B��B�YB��B�+B��B�_B��B��B��B�+B��B��B�_B��B��B��B��B��B��B��B��B�lB�7B��B�	B��B�	B�	B��B�rB�	B�	B�	B�=B��B��B�lB�lB�7B�7B�lB��B��B��B�	B��B�	B�rB�rB��B�DB�DB�xB�DB�DB�xB�DB��B�JB��B��B��B�JB�~B�B�B��B��B�~B��B�JB�JB��B��B��B�JB�~B��B�B��B�"B�VB��B��B��B��B�bB��B��B�AB�B�B�B�B�|B�B�B�B�|B��B��B�B�B�B�vB�;B�B�B�B�oB��B�AB�B�B�B�B�B�B��B��B�|B�B�AB�B��B�B�B�B�B��B��B�;B�oB�;B�vB��B��B�AB�B�B�B�B�B�MB�B�B�oB�B�oB�B�oB��B��B�;B�B�iB� B� B�B�iB�oB�oB��B�;B��B�B�B�B��B��B��B�B��B��B��B��B��B�MB�B�B�%B�B�B�TB�B�MB�B�MB��B�B�B�TB�B��B�B��B�GB�B�AB�vB�vB�vB�B�oB�B�B��B�;B��B��B�B�MB�B�GB�|B��B�B�B�B�B�oB�B�AB�B�;B�B�B�B�B�B�B�iB��B�B�B��B�B�/B�B��B�B�5B�B��B�cB�5B��B�;B� B�;B�/B��B�;B�B�oB�;B��B�5B�oB�vB�B�oB� B�B�iB��B��B�B��B�B�/B�B�B��B�B�B�B�"B�B�)B�)B�"B�WB�B�B�B�B��B��B�B�B�WB��B��B�B�"B��B�B��B��B�B�B�B�B�B�QB�B�B�B�KB�WB�B�DB��B�B��B�B�KB�yB��B�yB�yB�yB��B��B��B�B�mB��B��B�8B�fB�B�B��B�fB�2B�fB��B�2B�2B��B�2B�2B��B��B�,B��B��B�&B�TB��B�TB��B� B�NB�B��B�B��B�vB��B�B��B�B�/B�BޞB�dB�pB��BܒB�BںBٴBٴB��B�sB�B��B��B��B�,B��B�,B�vBϫB�dBԕB�
B�*B�BB�$B��B�!B��B��B�wB��B��B�0B�6B�XB�_B��B�FB�:B��B��B�bB�hB��B��B��B��B�OB��B��B�B��B�VB��B�+B��B�bB��B|Bp;BjBf2BiyBdZB_�B]/BQNBS�BOvBU�BK�B6�B6BOB7�B)_B3�BBBMBFB�BJBkB�B{B 4BB��B��B��B��B�%B��B�B�;B�(B�B�BMB��B�#BچB�WB�,B�dB��B��B��B�jB��B�wB�tB��B�bB�!B��B�SB��B�uB��B��B�%B�fBr|Bm�Bl�Bo5Bq�BjKBffBk�B_BZ�B\)B\]BW�BPBOBBN�BM�BK�BOvBR�B>�B8�B9�B7�B>�B*0B1'B!BBqABE�BYB�B��B�xB��B�(B��B�lB�B�/B�B��B�B�B�B�"B�B��B�B�fB�,B�,B�B��B�B�B�HB�BܒB�)BیB�;B�,BچB��B��B��B�dB��B�B�TB�dB��B�B�qB�wB��B��B��B�$B��B�tB�OB��B��B��B��B�B�hB��B�RB�'B��B��B��B�B��BiBm]Bc�BbBaBa�Ba|B`Bc�BiyB_�BZBc�BT,B\)BR�BQBN<BS[BOBBJXBI�BG�BH�BF�BJ�BI�BE�BEBIBC�BAUB@�B?B>B<6BA�B6zB6FB5?B6FB4�B;dB,qB1�B(�B)_B'�B'B#nB$�B!�B �B!bB�B!BBIBBkB�BCB�B	B�B�B�B�B�B�BB�B�B{B:BFB B�B�B\B�B�BVBDB	�B1B B4B�B�B
rB	7B	7B
rB�B 4B�B��B��B�>B�B�]B�B��BٴB��B��B֡B��B��B��B�NB��B�HBбB��B�BB��B�pB�jB�B�dB��B�^B��B��B��BɺB�XB��B�RB�KB�KB�B�zB��B�tB�9B��BŢB��B�B��B��B�B��B��B��B�hB��B��B�B�B�OB��B��B��B��B�B�kB�=B�$B�kB�B��B��B�XB�FB�_B��B�$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021012018214620210120182146IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021013010011020210130100110QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021013010011020210130100110QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164520210325101645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005120210326170051IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                